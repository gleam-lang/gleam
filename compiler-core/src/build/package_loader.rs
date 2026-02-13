#[cfg(test)]
mod tests;

use std::{
    collections::{HashMap, HashSet},
    time::{Duration, SystemTime},
};

use camino::{Utf8Path, Utf8PathBuf};

// TODO: emit warnings for cached modules even if they are not compiled again.

use ecow::EcoString;
use itertools::Itertools;
use vec1::Vec1;

use crate::{
    Error, Result,
    ast::SrcSpan,
    build::{Module, Origin, module_loader::ModuleLoader},
    config::{PackageConfig, PackageKind},
    dep_tree,
    error::{DefinedModuleOrigin, FileIoAction, FileKind, ImportCycleLocationDetails},
    io::{self, CommandExecutor, FileSystemReader, FileSystemWriter, files_with_extension},
    metadata,
    paths::ProjectPaths,
    type_,
    uid::UniqueIdGenerator,
    warning::WarningEmitter,
};

use super::{
    Mode, Target,
    module_loader::read_source,
    package_compiler::{
        CacheMetadata, CachedModule, CachedWarnings, Input, Loaded, UncompiledModule,
    },
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CodegenRequired {
    Yes,
    No,
}

impl CodegenRequired {
    /// Returns `true` if the codegen required is [`Yes`].
    ///
    /// [`Yes`]: CodegenRequired::Yes
    #[must_use]
    pub fn is_required(&self) -> bool {
        matches!(self, Self::Yes)
    }
}

#[derive(Debug)]
pub struct PackageLoader<'a, IO> {
    io: IO,
    ids: UniqueIdGenerator,
    mode: Mode,
    package_kind: PackageKind,
    paths: ProjectPaths,
    warnings: &'a WarningEmitter,
    codegen: CodegenRequired,
    artefact_directory: &'a Utf8Path,
    package_name: &'a EcoString,
    target: Target,
    stale_modules: &'a mut StaleTracker,
    already_defined_modules: &'a mut im::HashMap<EcoString, DefinedModuleOrigin>,
    incomplete_modules: &'a HashSet<EcoString>,
    cached_warnings: CachedWarnings,
}

impl<'a, IO> PackageLoader<'a, IO>
where
    IO: FileSystemWriter + FileSystemReader + CommandExecutor + Clone,
{
    pub(crate) fn new(
        io: IO,
        ids: UniqueIdGenerator,
        mode: Mode,
        package_kind: PackageKind,
        root: &'a Utf8Path,
        cached_warnings: CachedWarnings,
        warnings: &'a WarningEmitter,
        codegen: CodegenRequired,
        artefact_directory: &'a Utf8Path,
        target: Target,
        package_name: &'a EcoString,
        stale_modules: &'a mut StaleTracker,
        already_defined_modules: &'a mut im::HashMap<EcoString, DefinedModuleOrigin>,
        incomplete_modules: &'a HashSet<EcoString>,
    ) -> Self {
        Self {
            io,
            ids,
            mode,
            package_kind,
            paths: ProjectPaths::new(root.into()),
            warnings,
            codegen,
            target,
            package_name,
            cached_warnings,
            artefact_directory,
            stale_modules,
            already_defined_modules,
            incomplete_modules,
        }
    }

    pub(crate) fn run(mut self) -> Result<Loaded> {
        // First read the source files. This will use the `ModuleLoader`, which
        // will check the mtimes and hashes of sources and caches to determine
        // which should be loaded.
        let mut inputs = self.read_sources_and_caches()?;

        // Check for any removed modules, by looking at cache files that don't exist in inputs.
        // Delete the cache files for removed modules and mark them as stale
        // to trigger refreshing dependent modules.
        for module in CacheFiles::modules_with_meta_files(&self.io, &self.artefact_directory) {
            if (!inputs.contains_key(&module)) {
                tracing::debug!(%module, "module_removed");
                CacheFiles::new(&self.artefact_directory, &module).delete(&self.io)?;
                self.stale_modules.add(module);
            }
        }

        // Determine order in which modules are to be processed
        let mut dep_location_map = HashMap::new();
        let deps = inputs
            .values()
            .map(|(_, module)| {
                let name = module.name().clone();
                let _ = dep_location_map.insert(name.clone(), module);
                (name, module.dependencies())
            })
            // Making sure that the module order is deterministic, to prevent different
            // compilations of the same project compiling in different orders. This could impact
            // any bugged outcomes, though not any where the compiler is working correctly, so it's
            // mostly to aid debugging.
            .sorted_by(|(a, _), (b, _)| a.cmp(b))
            .collect();
        let sequence = dep_tree::toposort_deps(deps)
            .map_err(|error| self.convert_deps_tree_error(error, dep_location_map))?;

        // Now that we have loaded sources and caches we check to see if any of
        // the caches need to be invalidated because their dependencies have
        // changed.
        let mut loaded = Loaded::default();
        for name in sequence {
            let (_, input) = inputs
                .remove(&name)
                .expect("Getting parsed module for name");

            match input {
                // A new uncached module is to be compiled
                Input::New(module) => {
                    tracing::debug!(module = %module.name, "new_module_to_be_compiled");
                    self.stale_modules.add(module.name.clone());
                    loaded.to_compile.push(module);
                }

                // A cached module with dependencies that are stale must be
                // recompiled as the changes in the dependencies may have affect
                // the output, making the cache invalid.
                Input::Cached(info) if self.stale_modules.includes_any(&info.dependencies) => {
                    tracing::debug!(module = %info.name, "stale_module_to_be_compiled");
                    self.stale_modules.add(info.name.clone());
                    let module = self.load_stale_module(info)?;
                    loaded.to_compile.push(module);
                }

                // A cached module with no stale dependencies can be used as-is
                // and does not need to be recompiled.
                Input::Cached(info) => {
                    tracing::debug!(module = %info.name, "module_to_load_from_cache");
                    let module = self.load_cached_module(info)?;
                    loaded.cached.push(module);
                }
            }
        }

        Ok(loaded)
    }

    fn load_cached_module(&self, info: CachedModule) -> Result<type_::ModuleInterface, Error> {
        let cache_files = CacheFiles::new(&self.artefact_directory, &info.name);
        let bytes = self.io.read_bytes(&cache_files.cache_path)?;
        let mut module = metadata::ModuleDecoder::new(self.ids.clone()).read(bytes.as_slice())?;

        if self.io.exists(&cache_files.inline_path) {
            let bytes = self.io.read_bytes(&cache_files.inline_path)?;
            module.inline_functions =
                match bincode::serde::decode_from_slice(&bytes, bincode::config::legacy()) {
                    Ok((data, _)) => data,
                    Err(e) => {
                        return Err(Error::FileIo {
                            kind: FileKind::File,
                            action: FileIoAction::Parse,
                            path: cache_files.inline_path,
                            err: Some(e.to_string()),
                        });
                    }
                };
        }

        // Load warnings
        if self.cached_warnings.should_use() {
            let path = cache_files.warnings_path;
            if self.io.exists(&path) {
                let bytes = self.io.read_bytes(&path)?;
                module.warnings =
                    match bincode::serde::decode_from_slice(&bytes, bincode::config::legacy()) {
                        Ok((data, _)) => data,
                        Err(e) => {
                            return Err(Error::FileIo {
                                kind: FileKind::File,
                                action: FileIoAction::Parse,
                                path,
                                err: Some(e.to_string()),
                            });
                        }
                    };
            }
        }

        Ok(module)
    }

    fn read_sources_and_caches(
        &mut self,
    ) -> Result<HashMap<EcoString, (DefinedModuleOrigin, Input)>> {
        let span = tracing::info_span!("load");
        let _enter = span.enter();

        let mut inputs = Inputs::new(self.already_defined_modules);

        let src = self.paths.src_directory();
        let mut loader = ModuleLoader {
            io: self.io.clone(),
            warnings: self.warnings,
            mode: self.mode,
            target: self.target,
            codegen: self.codegen,
            package_name: self.package_name,
            artefact_directory: self.artefact_directory,
            origin: Origin::Src,
            incomplete_modules: self.incomplete_modules,
        };

        // Src
        for file in GleamFile::iterate_files_in_directory(&self.io, &src) {
            match file {
                Ok(file) => {
                    let input = loader.load(file)?;
                    inputs.insert(input, self.package_kind.clone())?;
                }
                Err(warning) => self.warnings.emit(warning),
            }
        }

        // Test and dev
        if self.mode.includes_dev_code() {
            let test = self.paths.test_directory();
            loader.origin = Origin::Test;

            for file in GleamFile::iterate_files_in_directory(&self.io, &test) {
                match file {
                    Ok(file) => {
                        let input = loader.load(file)?;
                        inputs.insert(input, self.package_kind.clone())?;
                    }
                    Err(warning) => self.warnings.emit(warning),
                }
            }

            let dev = self.paths.dev_directory();
            loader.origin = Origin::Dev;

            for file in GleamFile::iterate_files_in_directory(&self.io, &dev) {
                match file {
                    Ok(file) => {
                        let input = loader.load(file)?;
                        inputs.insert(input, self.package_kind.clone())?;
                    }
                    Err(warning) => self.warnings.emit(warning),
                }
            }
        }

        // If we are compiling for Erlang then modules all live in a single
        // namespace. If we were to name a module the same as a module that
        // is included in the standard Erlang distribution then this new
        // Gleam module would overwrite the existing Erlang one, likely
        // resulting in cryptic errors.
        // This would most commonly happen for modules like "user" and
        // "code". Emit an error so this never happens.
        if self.target.is_erlang() {
            for (_, input) in inputs.collection.values() {
                ensure_gleam_module_does_not_overwrite_standard_erlang_module(&input)?;
            }
        }

        Ok(inputs.collection)
    }

    fn load_stale_module(&self, cached: CachedModule) -> Result<UncompiledModule> {
        let mtime = self.io.modification_time(&cached.source_path)?;

        // We need to delete any existing cache files for this module.
        // While we figured it out this time because the module has stale dependencies,
        // next time the dependencies might no longer be stale, but we still need to be able to tell
        // that this module needs to be recompiled until it successfully compiles at least once.
        // This can happen if the stale dependency includes breaking changes.
        CacheFiles::new(&self.artefact_directory, &cached.name).delete(&self.io)?;

        read_source(
            self.io.clone(),
            self.target,
            cached.origin,
            cached.source_path,
            cached.name,
            self.package_name.clone(),
            mtime,
            self.warnings.clone(),
        )
    }

    fn convert_deps_tree_error(
        &self,
        e: dep_tree::Error,
        dep_location_map: HashMap<EcoString, &Input>,
    ) -> Error {
        match e {
            dep_tree::Error::Cycle(modules) => {
                let modules = modules
                    .iter()
                    .enumerate()
                    .map(|(i, module)| {
                        // cycles are in order of reference so get next in list or loop back to first
                        let index_of_imported = if i == 0 { modules.len() - 1 } else { i - 1 };
                        let imported_module = modules
                            .get(index_of_imported)
                            .expect("importing module must exist");
                        let input = dep_location_map.get(module).expect("dependency must exist");
                        let location = match input {
                            Input::New(module) => {
                                let (_, location) = module
                                    .dependencies
                                    .iter()
                                    .find(|d| &d.0 == imported_module)
                                    .expect("import must exist for there to be a cycle");
                                ImportCycleLocationDetails {
                                    location: *location,
                                    path: module.path.clone(),
                                    src: module.code.clone(),
                                }
                            }
                            Input::Cached(cached_module) => {
                                let (_, location) = cached_module
                                    .dependencies
                                    .iter()
                                    .find(|d| &d.0 == imported_module)
                                    .expect("import must exist for there to be a cycle");
                                let src = self
                                    .io
                                    .read(&cached_module.source_path)
                                    .expect("failed to read source")
                                    .into();
                                ImportCycleLocationDetails {
                                    location: *location,
                                    path: cached_module.source_path.clone(),
                                    src,
                                }
                            }
                        };
                        (module.clone(), location)
                    })
                    .collect_vec();
                Error::ImportCycle {
                    modules: Vec1::try_from(modules)
                        .expect("at least 1 module must exist in cycle"),
                }
            }
        }
    }
}

fn ensure_gleam_module_does_not_overwrite_standard_erlang_module(input: &Input) -> Result<()> {
    // We only need to check uncached modules as it's not possible for these
    // to have compiled successfully.
    let Input::New(input) = input else {
        return Ok(());
    };

    // These names were got with this Erlang
    //
    // ```erl
    // file:write_file("names.txt", lists:join("\n",lists:map(fun(T) -> erlang:element(1, T) end, code:all_available()))).
    // ```
    //
    match input.name.as_str() {
        "alarm_handler"
        | "application"
        | "application_controller"
        | "application_master"
        | "application_starter"
        | "appmon_info"
        | "argparse"
        | "array"
        | "asn1_db"
        | "asn1ct"
        | "asn1ct_check"
        | "asn1ct_constructed_ber_bin_v2"
        | "asn1ct_constructed_per"
        | "asn1ct_eval_ext"
        | "asn1ct_func"
        | "asn1ct_gen"
        | "asn1ct_gen_ber_bin_v2"
        | "asn1ct_gen_check"
        | "asn1ct_gen_jer"
        | "asn1ct_gen_per"
        | "asn1ct_imm"
        | "asn1ct_name"
        | "asn1ct_parser2"
        | "asn1ct_pretty_format"
        | "asn1ct_rtt"
        | "asn1ct_table"
        | "asn1ct_tok"
        | "asn1ct_value"
        | "asn1rt_nif"
        | "atomics"
        | "auth"
        | "base64"
        | "beam_a"
        | "beam_asm"
        | "beam_block"
        | "beam_bounds"
        | "beam_call_types"
        | "beam_clean"
        | "beam_dict"
        | "beam_digraph"
        | "beam_disasm"
        | "beam_flatten"
        | "beam_jump"
        | "beam_kernel_to_ssa"
        | "beam_lib"
        | "beam_listing"
        | "beam_opcodes"
        | "beam_ssa"
        | "beam_ssa_alias"
        | "beam_ssa_bc_size"
        | "beam_ssa_bool"
        | "beam_ssa_bsm"
        | "beam_ssa_check"
        | "beam_ssa_codegen"
        | "beam_ssa_dead"
        | "beam_ssa_lint"
        | "beam_ssa_opt"
        | "beam_ssa_pp"
        | "beam_ssa_pre_codegen"
        | "beam_ssa_private_append"
        | "beam_ssa_recv"
        | "beam_ssa_share"
        | "beam_ssa_throw"
        | "beam_ssa_type"
        | "beam_trim"
        | "beam_types"
        | "beam_utils"
        | "beam_validator"
        | "beam_z"
        | "binary"
        | "c"
        | "calendar"
        | "cdv_atom_cb"
        | "cdv_bin_cb"
        | "cdv_detail_wx"
        | "cdv_dist_cb"
        | "cdv_ets_cb"
        | "cdv_fun_cb"
        | "cdv_gen_cb"
        | "cdv_html_wx"
        | "cdv_info_wx"
        | "cdv_int_tab_cb"
        | "cdv_mem_cb"
        | "cdv_mod_cb"
        | "cdv_multi_wx"
        | "cdv_persistent_cb"
        | "cdv_port_cb"
        | "cdv_proc_cb"
        | "cdv_sched_cb"
        | "cdv_table_wx"
        | "cdv_term_cb"
        | "cdv_timer_cb"
        | "cdv_virtual_list_wx"
        | "cdv_wx"
        | "cerl"
        | "cerl_clauses"
        | "cerl_inline"
        | "cerl_prettypr"
        | "cerl_trees"
        | "code"
        | "code_server"
        | "compile"
        | "core_lib"
        | "core_lint"
        | "core_parse"
        | "core_pp"
        | "core_scan"
        | "counters"
        | "cover"
        | "cprof"
        | "cpu_sup"
        | "crashdump_viewer"
        | "crypto"
        | "crypto_ec_curves"
        | "ct"
        | "ct_config"
        | "ct_config_plain"
        | "ct_config_xml"
        | "ct_conn_log_h"
        | "ct_cover"
        | "ct_default_gl"
        | "ct_event"
        | "ct_framework"
        | "ct_ftp"
        | "ct_gen_conn"
        | "ct_groups"
        | "ct_hooks"
        | "ct_hooks_lock"
        | "ct_logs"
        | "ct_make"
        | "ct_master"
        | "ct_master_event"
        | "ct_master_logs"
        | "ct_master_status"
        | "ct_netconfc"
        | "ct_property_test"
        | "ct_release_test"
        | "ct_repeat"
        | "ct_rpc"
        | "ct_run"
        | "ct_slave"
        | "ct_snmp"
        | "ct_ssh"
        | "ct_suite"
        | "ct_telnet"
        | "ct_telnet_client"
        | "ct_testspec"
        | "ct_util"
        | "cth_conn_log"
        | "cth_log_redirect"
        | "cth_surefire"
        | "dbg"
        | "dbg_debugged"
        | "dbg_icmd"
        | "dbg_idb"
        | "dbg_ieval"
        | "dbg_iload"
        | "dbg_iserver"
        | "dbg_istk"
        | "dbg_wx_break"
        | "dbg_wx_break_win"
        | "dbg_wx_code"
        | "dbg_wx_filedialog_win"
        | "dbg_wx_interpret"
        | "dbg_wx_mon"
        | "dbg_wx_mon_win"
        | "dbg_wx_settings"
        | "dbg_wx_src_view"
        | "dbg_wx_trace"
        | "dbg_wx_trace_win"
        | "dbg_wx_view"
        | "dbg_wx_win"
        | "dbg_wx_winman"
        | "debugger"
        | "dets"
        | "dets_server"
        | "dets_sup"
        | "dets_utils"
        | "dets_v9"
        | "dialyzer"
        | "dialyzer_analysis_callgraph"
        | "dialyzer_behaviours"
        | "dialyzer_callgraph"
        | "dialyzer_cl"
        | "dialyzer_cl_parse"
        | "dialyzer_clean_core"
        | "dialyzer_codeserver"
        | "dialyzer_contracts"
        | "dialyzer_coordinator"
        | "dialyzer_cplt"
        | "dialyzer_dataflow"
        | "dialyzer_dep"
        | "dialyzer_dot"
        | "dialyzer_explanation"
        | "dialyzer_gui_wx"
        | "dialyzer_incremental"
        | "dialyzer_iplt"
        | "dialyzer_options"
        | "dialyzer_plt"
        | "dialyzer_succ_typings"
        | "dialyzer_timing"
        | "dialyzer_typegraph"
        | "dialyzer_typesig"
        | "dialyzer_utils"
        | "dialyzer_worker"
        | "diameter"
        | "diameter_app"
        | "diameter_callback"
        | "diameter_capx"
        | "diameter_codec"
        | "diameter_codegen"
        | "diameter_config"
        | "diameter_config_sup"
        | "diameter_dbg"
        | "diameter_dict_parser"
        | "diameter_dict_scanner"
        | "diameter_dict_util"
        | "diameter_dist"
        | "diameter_etcp"
        | "diameter_etcp_sup"
        | "diameter_exprecs"
        | "diameter_gen"
        | "diameter_gen_acct_rfc6733"
        | "diameter_gen_base_accounting"
        | "diameter_gen_base_rfc3588"
        | "diameter_gen_base_rfc6733"
        | "diameter_gen_doic_rfc7683"
        | "diameter_gen_relay"
        | "diameter_info"
        | "diameter_lib"
        | "diameter_make"
        | "diameter_misc_sup"
        | "diameter_peer"
        | "diameter_peer_fsm"
        | "diameter_peer_fsm_sup"
        | "diameter_reg"
        | "diameter_sctp"
        | "diameter_sctp_sup"
        | "diameter_service"
        | "diameter_service_sup"
        | "diameter_session"
        | "diameter_stats"
        | "diameter_sup"
        | "diameter_sync"
        | "diameter_tcp"
        | "diameter_tcp_sup"
        | "diameter_traffic"
        | "diameter_transport"
        | "diameter_transport_sup"
        | "diameter_types"
        | "diameter_watchdog"
        | "diameter_watchdog_sup"
        | "dict"
        | "digraph"
        | "digraph_utils"
        | "disk_log"
        | "disk_log_1"
        | "disk_log_server"
        | "disk_log_sup"
        | "disksup"
        | "dist_ac"
        | "dist_util"
        | "docgen_edoc_xml_cb"
        | "docgen_otp_specs"
        | "docgen_xmerl_xml_cb"
        | "docgen_xml_to_chunk"
        | "dtls_connection"
        | "dtls_connection_sup"
        | "dtls_gen_connection"
        | "dtls_handshake"
        | "dtls_listener_sup"
        | "dtls_packet_demux"
        | "dtls_record"
        | "dtls_server_session_cache_sup"
        | "dtls_server_sup"
        | "dtls_socket"
        | "dtls_sup"
        | "dtls_v1"
        | "dyntrace"
        | "edlin"
        | "edlin_context"
        | "edlin_expand"
        | "edlin_key"
        | "edlin_type_suggestion"
        | "edoc"
        | "edoc_cli"
        | "edoc_data"
        | "edoc_doclet"
        | "edoc_doclet_chunks"
        | "edoc_extract"
        | "edoc_layout"
        | "edoc_layout_chunks"
        | "edoc_lib"
        | "edoc_macros"
        | "edoc_parser"
        | "edoc_refs"
        | "edoc_report"
        | "edoc_run"
        | "edoc_scanner"
        | "edoc_specs"
        | "edoc_tags"
        | "edoc_types"
        | "edoc_wiki"
        | "eldap"
        | "epp"
        | "epp_dodger"
        | "eprof"
        | "erl2html2"
        | "erl_abstract_code"
        | "erl_anno"
        | "erl_bif_types"
        | "erl_bifs"
        | "erl_bits"
        | "erl_boot_server"
        | "erl_comment_scan"
        | "erl_compile"
        | "erl_compile_server"
        | "erl_ddll"
        | "erl_distribution"
        | "erl_epmd"
        | "erl_error"
        | "erl_erts_errors"
        | "erl_eval"
        | "erl_expand_records"
        | "erl_features"
        | "erl_init"
        | "erl_internal"
        | "erl_kernel_errors"
        | "erl_lint"
        | "erl_parse"
        | "erl_posix_msg"
        | "erl_pp"
        | "erl_prettypr"
        | "erl_prim_loader"
        | "erl_recomment"
        | "erl_reply"
        | "erl_scan"
        | "erl_signal_handler"
        | "erl_stdlib_errors"
        | "erl_syntax"
        | "erl_syntax_lib"
        | "erl_tar"
        | "erl_tracer"
        | "erl_types"
        | "erlang"
        | "erlsrv"
        | "erpc"
        | "error_handler"
        | "error_logger"
        | "error_logger_file_h"
        | "error_logger_tty_h"
        | "erts_alloc_config"
        | "erts_code_purger"
        | "erts_debug"
        | "erts_dirty_process_signal_handler"
        | "erts_internal"
        | "erts_literal_area_collector"
        | "escript"
        | "et"
        | "et_collector"
        | "et_selector"
        | "et_viewer"
        | "et_wx_contents_viewer"
        | "et_wx_viewer"
        | "etop"
        | "etop_tr"
        | "etop_txt"
        | "ets"
        | "eunit"
        | "eunit_autoexport"
        | "eunit_data"
        | "eunit_lib"
        | "eunit_listener"
        | "eunit_proc"
        | "eunit_serial"
        | "eunit_server"
        | "eunit_striptests"
        | "eunit_surefire"
        | "eunit_test"
        | "eunit_tests"
        | "eunit_tty"
        | "eval_bits"
        | "file"
        | "file_io_server"
        | "file_server"
        | "file_sorter"
        | "filelib"
        | "filename"
        | "format_lib_supp"
        | "fprof"
        | "ftp"
        | "ftp_app"
        | "ftp_internal"
        | "ftp_progress"
        | "ftp_response"
        | "ftp_sup"
        | "gb_sets"
        | "gb_trees"
        | "gen"
        | "gen_event"
        | "gen_fsm"
        | "gen_sctp"
        | "gen_server"
        | "gen_statem"
        | "gen_tcp"
        | "gen_tcp_socket"
        | "gen_udp"
        | "gen_udp_socket"
        | "gl"
        | "global"
        | "global_group"
        | "global_search"
        | "glu"
        | "group"
        | "group_history"
        | "heart"
        | "http_chunk"
        | "http_request"
        | "http_response"
        | "http_transport"
        | "http_uri"
        | "http_util"
        | "httpc"
        | "httpc_cookie"
        | "httpc_handler"
        | "httpc_handler_sup"
        | "httpc_manager"
        | "httpc_profile_sup"
        | "httpc_request"
        | "httpc_response"
        | "httpc_sup"
        | "httpd"
        | "httpd_acceptor"
        | "httpd_acceptor_sup"
        | "httpd_cgi"
        | "httpd_conf"
        | "httpd_connection_sup"
        | "httpd_custom"
        | "httpd_custom_api"
        | "httpd_esi"
        | "httpd_example"
        | "httpd_file"
        | "httpd_instance_sup"
        | "httpd_log"
        | "httpd_logger"
        | "httpd_manager"
        | "httpd_misc_sup"
        | "httpd_request"
        | "httpd_request_handler"
        | "httpd_response"
        | "httpd_script_env"
        | "httpd_socket"
        | "httpd_sup"
        | "httpd_util"
        | "i"
        | "inet"
        | "inet6_sctp"
        | "inet6_tcp"
        | "inet6_tcp_dist"
        | "inet6_tls_dist"
        | "inet6_udp"
        | "inet_config"
        | "inet_db"
        | "inet_dns"
        | "inet_epmd_dist"
        | "inet_epmd_socket"
        | "inet_gethost_native"
        | "inet_hosts"
        | "inet_parse"
        | "inet_res"
        | "inet_sctp"
        | "inet_tcp"
        | "inet_tcp_dist"
        | "inet_tls_dist"
        | "inet_udp"
        | "inets"
        | "inets_app"
        | "inets_lib"
        | "inets_service"
        | "inets_sup"
        | "inets_trace"
        | "init"
        | "instrument"
        | "int"
        | "io"
        | "io_lib"
        | "io_lib_format"
        | "io_lib_fread"
        | "io_lib_pretty"
        | "json"
        | "kernel"
        | "kernel_config"
        | "kernel_refc"
        | "lcnt"
        | "leex"
        | "lists"
        | "local_tcp"
        | "local_udp"
        | "log_mf_h"
        | "logger"
        | "logger_backend"
        | "logger_config"
        | "logger_disk_log_h"
        | "logger_filters"
        | "logger_formatter"
        | "logger_h_common"
        | "logger_handler_watcher"
        | "logger_olp"
        | "logger_proxy"
        | "logger_server"
        | "logger_simple_h"
        | "logger_std_h"
        | "logger_sup"
        | "make"
        | "maps"
        | "math"
        | "megaco"
        | "megaco_ber_encoder"
        | "megaco_ber_media_gateway_control_v1"
        | "megaco_ber_media_gateway_control_v2"
        | "megaco_ber_media_gateway_control_v3"
        | "megaco_binary_encoder"
        | "megaco_binary_encoder_lib"
        | "megaco_binary_name_resolver_v1"
        | "megaco_binary_name_resolver_v2"
        | "megaco_binary_name_resolver_v3"
        | "megaco_binary_term_id"
        | "megaco_binary_term_id_gen"
        | "megaco_binary_transformer_v1"
        | "megaco_binary_transformer_v2"
        | "megaco_binary_transformer_v3"
        | "megaco_compact_text_encoder"
        | "megaco_compact_text_encoder_v1"
        | "megaco_compact_text_encoder_v2"
        | "megaco_compact_text_encoder_v3"
        | "megaco_config"
        | "megaco_config_misc"
        | "megaco_digit_map"
        | "megaco_edist_compress"
        | "megaco_encoder"
        | "megaco_erl_dist_encoder"
        | "megaco_erl_dist_encoder_mc"
        | "megaco_filter"
        | "megaco_flex_scanner"
        | "megaco_flex_scanner_handler"
        | "megaco_messenger"
        | "megaco_messenger_misc"
        | "megaco_misc_sup"
        | "megaco_monitor"
        | "megaco_per_encoder"
        | "megaco_per_media_gateway_control_v1"
        | "megaco_per_media_gateway_control_v2"
        | "megaco_per_media_gateway_control_v3"
        | "megaco_pretty_text_encoder"
        | "megaco_pretty_text_encoder_v1"
        | "megaco_pretty_text_encoder_v2"
        | "megaco_pretty_text_encoder_v3"
        | "megaco_sdp"
        | "megaco_stats"
        | "megaco_sup"
        | "megaco_tcp"
        | "megaco_tcp_accept"
        | "megaco_tcp_accept_sup"
        | "megaco_tcp_connection"
        | "megaco_tcp_connection_sup"
        | "megaco_tcp_sup"
        | "megaco_text_mini_decoder"
        | "megaco_text_mini_parser"
        | "megaco_text_parser_v1"
        | "megaco_text_parser_v2"
        | "megaco_text_parser_v3"
        | "megaco_text_scanner"
        | "megaco_timer"
        | "megaco_trans_sender"
        | "megaco_trans_sup"
        | "megaco_transport"
        | "megaco_udp"
        | "megaco_udp_server"
        | "megaco_udp_sup"
        | "megaco_user"
        | "megaco_user_default"
        | "memsup"
        | "merl"
        | "merl_transform"
        | "misc_supp"
        | "mnesia"
        | "mnesia_app"
        | "mnesia_backend_type"
        | "mnesia_backup"
        | "mnesia_bup"
        | "mnesia_checkpoint"
        | "mnesia_checkpoint_sup"
        | "mnesia_controller"
        | "mnesia_dumper"
        | "mnesia_event"
        | "mnesia_ext_sup"
        | "mnesia_frag"
        | "mnesia_frag_hash"
        | "mnesia_index"
        | "mnesia_kernel_sup"
        | "mnesia_late_loader"
        | "mnesia_lib"
        | "mnesia_loader"
        | "mnesia_locker"
        | "mnesia_log"
        | "mnesia_monitor"
        | "mnesia_recover"
        | "mnesia_registry"
        | "mnesia_rpc"
        | "mnesia_schema"
        | "mnesia_snmp_hook"
        | "mnesia_sp"
        | "mnesia_subscr"
        | "mnesia_sup"
        | "mnesia_text"
        | "mnesia_tm"
        | "mod_actions"
        | "mod_alias"
        | "mod_auth"
        | "mod_auth_dets"
        | "mod_auth_mnesia"
        | "mod_auth_plain"
        | "mod_auth_server"
        | "mod_cgi"
        | "mod_dir"
        | "mod_disk_log"
        | "mod_esi"
        | "mod_get"
        | "mod_head"
        | "mod_log"
        | "mod_range"
        | "mod_responsecontrol"
        | "mod_security"
        | "mod_security_server"
        | "mod_trace"
        | "ms_transform"
        | "msacc"
        | "net"
        | "net_adm"
        | "net_kernel"
        | "nteventlog"
        | "observer"
        | "observer_alloc_wx"
        | "observer_app_wx"
        | "observer_backend"
        | "observer_html_lib"
        | "observer_lib"
        | "observer_perf_wx"
        | "observer_port_wx"
        | "observer_pro_wx"
        | "observer_procinfo"
        | "observer_sock_wx"
        | "observer_sys_wx"
        | "observer_trace_wx"
        | "observer_traceoptions_wx"
        | "observer_tv_table"
        | "observer_tv_wx"
        | "observer_wx"
        | "orddict"
        | "ordsets"
        | "os"
        | "os_mon"
        | "os_mon_mib"
        | "os_mon_sysinfo"
        | "os_sup"
        | "otp_internal"
        | "peer"
        | "persistent_term"
        | "pg"
        | "pg2"
        | "pool"
        | "prettypr"
        | "prim_buffer"
        | "prim_eval"
        | "prim_file"
        | "prim_inet"
        | "prim_net"
        | "prim_socket"
        | "prim_tty"
        | "prim_zip"
        | "proc_lib"
        | "proplists"
        | "pubkey_cert"
        | "pubkey_cert_records"
        | "pubkey_crl"
        | "pubkey_ocsp"
        | "pubkey_os_cacerts"
        | "pubkey_pbe"
        | "pubkey_pem"
        | "pubkey_policy_tree"
        | "pubkey_ssh"
        | "public_key"
        | "qlc"
        | "qlc_pt"
        | "queue"
        | "ram_file"
        | "rand"
        | "random"
        | "raw_file_io"
        | "raw_file_io_compressed"
        | "raw_file_io_deflate"
        | "raw_file_io_delayed"
        | "raw_file_io_inflate"
        | "raw_file_io_list"
        | "rb"
        | "rb_format_supp"
        | "re"
        | "rec_env"
        | "release_handler"
        | "release_handler_1"
        | "reltool"
        | "reltool_app_win"
        | "reltool_fgraph"
        | "reltool_fgraph_win"
        | "reltool_mod_win"
        | "reltool_server"
        | "reltool_sys_win"
        | "reltool_target"
        | "reltool_utils"
        | "rpc"
        | "runtime_tools"
        | "runtime_tools_sup"
        | "sasl"
        | "sasl_report"
        | "sasl_report_file_h"
        | "sasl_report_tty_h"
        | "scheduler"
        | "seq_trace"
        | "sets"
        | "shell"
        | "shell_default"
        | "shell_docs"
        | "slave"
        | "snmp"
        | "snmp_app"
        | "snmp_app_sup"
        | "snmp_community_mib"
        | "snmp_conf"
        | "snmp_config"
        | "snmp_framework_mib"
        | "snmp_generic"
        | "snmp_generic_mnesia"
        | "snmp_index"
        | "snmp_log"
        | "snmp_mini_mib"
        | "snmp_misc"
        | "snmp_note_store"
        | "snmp_notification_mib"
        | "snmp_pdus"
        | "snmp_shadow_table"
        | "snmp_standard_mib"
        | "snmp_target_mib"
        | "snmp_user_based_sm_mib"
        | "snmp_usm"
        | "snmp_verbosity"
        | "snmp_view_based_acm_mib"
        | "snmpa"
        | "snmpa_acm"
        | "snmpa_agent"
        | "snmpa_agent_sup"
        | "snmpa_app"
        | "snmpa_authentication_service"
        | "snmpa_conf"
        | "snmpa_discovery_handler"
        | "snmpa_discovery_handler_default"
        | "snmpa_error"
        | "snmpa_error_io"
        | "snmpa_error_logger"
        | "snmpa_error_report"
        | "snmpa_get"
        | "snmpa_get_lib"
        | "snmpa_get_mechanism"
        | "snmpa_local_db"
        | "snmpa_mib"
        | "snmpa_mib_data"
        | "snmpa_mib_data_tttn"
        | "snmpa_mib_lib"
        | "snmpa_mib_storage"
        | "snmpa_mib_storage_dets"
        | "snmpa_mib_storage_ets"
        | "snmpa_mib_storage_mnesia"
        | "snmpa_misc_sup"
        | "snmpa_mpd"
        | "snmpa_net_if"
        | "snmpa_net_if_filter"
        | "snmpa_network_interface"
        | "snmpa_network_interface_filter"
        | "snmpa_notification_delivery_info_receiver"
        | "snmpa_notification_filter"
        | "snmpa_set"
        | "snmpa_set_lib"
        | "snmpa_set_mechanism"
        | "snmpa_supervisor"
        | "snmpa_svbl"
        | "snmpa_symbolic_store"
        | "snmpa_target_cache"
        | "snmpa_trap"
        | "snmpa_usm"
        | "snmpa_vacm"
        | "snmpc"
        | "snmpc_lib"
        | "snmpc_mib_gram"
        | "snmpc_mib_to_hrl"
        | "snmpc_misc"
        | "snmpc_tok"
        | "snmpm"
        | "snmpm_conf"
        | "snmpm_config"
        | "snmpm_misc_sup"
        | "snmpm_mpd"
        | "snmpm_net_if"
        | "snmpm_net_if_filter"
        | "snmpm_net_if_mt"
        | "snmpm_network_interface"
        | "snmpm_network_interface_filter"
        | "snmpm_server"
        | "snmpm_server_sup"
        | "snmpm_supervisor"
        | "snmpm_user"
        | "snmpm_user_default"
        | "snmpm_user_old"
        | "snmpm_usm"
        | "socket"
        | "socket_registry"
        | "sofs"
        | "ssh"
        | "ssh_acceptor"
        | "ssh_acceptor_sup"
        | "ssh_agent"
        | "ssh_app"
        | "ssh_auth"
        | "ssh_bits"
        | "ssh_channel"
        | "ssh_channel_sup"
        | "ssh_cli"
        | "ssh_client_channel"
        | "ssh_client_key_api"
        | "ssh_connection"
        | "ssh_connection_handler"
        | "ssh_daemon_channel"
        | "ssh_dbg"
        | "ssh_file"
        | "ssh_fsm_kexinit"
        | "ssh_fsm_userauth_client"
        | "ssh_fsm_userauth_server"
        | "ssh_info"
        | "ssh_io"
        | "ssh_lib"
        | "ssh_message"
        | "ssh_no_io"
        | "ssh_options"
        | "ssh_server_channel"
        | "ssh_server_key_api"
        | "ssh_sftp"
        | "ssh_sftpd"
        | "ssh_sftpd_file"
        | "ssh_sftpd_file_api"
        | "ssh_shell"
        | "ssh_subsystem_sup"
        | "ssh_system_sup"
        | "ssh_tcpip_forward_acceptor"
        | "ssh_tcpip_forward_acceptor_sup"
        | "ssh_tcpip_forward_client"
        | "ssh_tcpip_forward_srv"
        | "ssh_transport"
        | "ssh_xfer"
        | "ssl"
        | "ssl_admin_sup"
        | "ssl_alert"
        | "ssl_app"
        | "ssl_certificate"
        | "ssl_cipher"
        | "ssl_cipher_format"
        | "ssl_client_session_cache_db"
        | "ssl_config"
        | "ssl_connection_sup"
        | "ssl_crl"
        | "ssl_crl_cache"
        | "ssl_crl_cache_api"
        | "ssl_crl_hash_dir"
        | "ssl_dh_groups"
        | "ssl_dist_admin_sup"
        | "ssl_dist_connection_sup"
        | "ssl_dist_sup"
        | "ssl_gen_statem"
        | "ssl_handshake"
        | "ssl_listen_tracker_sup"
        | "ssl_logger"
        | "ssl_manager"
        | "ssl_pem_cache"
        | "ssl_pkix_db"
        | "ssl_record"
        | "ssl_server_session_cache"
        | "ssl_server_session_cache_db"
        | "ssl_server_session_cache_sup"
        | "ssl_session"
        | "ssl_session_cache_api"
        | "ssl_srp_primes"
        | "ssl_sup"
        | "ssl_trace"
        | "ssl_upgrade_server_session_cache_sup"
        | "standard_error"
        | "string"
        | "supervisor"
        | "supervisor_bridge"
        | "sys"
        | "sys_core_alias"
        | "sys_core_bsm"
        | "sys_core_fold"
        | "sys_core_fold_lists"
        | "sys_core_inline"
        | "sys_core_prepare"
        | "sys_messages"
        | "sys_pre_attributes"
        | "system_information"
        | "systools"
        | "systools_lib"
        | "systools_make"
        | "systools_rc"
        | "systools_relup"
        | "tags"
        | "test_server"
        | "test_server_ctrl"
        | "test_server_gl"
        | "test_server_io"
        | "test_server_node"
        | "test_server_sup"
        | "tftp"
        | "tftp_app"
        | "tftp_binary"
        | "tftp_engine"
        | "tftp_file"
        | "tftp_lib"
        | "tftp_logger"
        | "tftp_sup"
        | "timer"
        | "tls_bloom_filter"
        | "tls_client_connection_1_3"
        | "tls_client_ticket_store"
        | "tls_connection"
        | "tls_connection_sup"
        | "tls_dist_server_sup"
        | "tls_dist_sup"
        | "tls_dtls_connection"
        | "tls_dyn_connection_sup"
        | "tls_gen_connection"
        | "tls_gen_connection_1_3"
        | "tls_handshake"
        | "tls_handshake_1_3"
        | "tls_record"
        | "tls_record_1_3"
        | "tls_sender"
        | "tls_server_connection_1_3"
        | "tls_server_session_ticket"
        | "tls_server_session_ticket_sup"
        | "tls_server_sup"
        | "tls_socket"
        | "tls_sup"
        | "tls_v1"
        | "ttb"
        | "ttb_autostart"
        | "ttb_et"
        | "typer"
        | "typer_core"
        | "unicode"
        | "unicode_util"
        | "unix_telnet"
        | "uri_string"
        | "user_drv"
        | "user_sup"
        | "v3_core"
        | "v3_kernel"
        | "v3_kernel_pp"
        | "win32reg"
        | "wrap_log_reader"
        | "wx"
        | "wxAcceleratorEntry"
        | "wxAcceleratorTable"
        | "wxActivateEvent"
        | "wxArtProvider"
        | "wxAuiDockArt"
        | "wxAuiManager"
        | "wxAuiManagerEvent"
        | "wxAuiNotebook"
        | "wxAuiNotebookEvent"
        | "wxAuiPaneInfo"
        | "wxAuiSimpleTabArt"
        | "wxAuiTabArt"
        | "wxBitmap"
        | "wxBitmapButton"
        | "wxBitmapDataObject"
        | "wxBookCtrlBase"
        | "wxBookCtrlEvent"
        | "wxBoxSizer"
        | "wxBrush"
        | "wxBufferedDC"
        | "wxBufferedPaintDC"
        | "wxButton"
        | "wxCalendarCtrl"
        | "wxCalendarDateAttr"
        | "wxCalendarEvent"
        | "wxCaret"
        | "wxCheckBox"
        | "wxCheckListBox"
        | "wxChildFocusEvent"
        | "wxChoice"
        | "wxChoicebook"
        | "wxClientDC"
        | "wxClipboard"
        | "wxClipboardTextEvent"
        | "wxCloseEvent"
        | "wxColourData"
        | "wxColourDialog"
        | "wxColourPickerCtrl"
        | "wxColourPickerEvent"
        | "wxComboBox"
        | "wxCommandEvent"
        | "wxContextMenuEvent"
        | "wxControl"
        | "wxControlWithItems"
        | "wxCursor"
        | "wxDC"
        | "wxDCOverlay"
        | "wxDataObject"
        | "wxDateEvent"
        | "wxDatePickerCtrl"
        | "wxDialog"
        | "wxDirDialog"
        | "wxDirPickerCtrl"
        | "wxDisplay"
        | "wxDisplayChangedEvent"
        | "wxDropFilesEvent"
        | "wxEraseEvent"
        | "wxEvent"
        | "wxEvtHandler"
        | "wxFileDataObject"
        | "wxFileDialog"
        | "wxFileDirPickerEvent"
        | "wxFilePickerCtrl"
        | "wxFindReplaceData"
        | "wxFindReplaceDialog"
        | "wxFlexGridSizer"
        | "wxFocusEvent"
        | "wxFont"
        | "wxFontData"
        | "wxFontDialog"
        | "wxFontPickerCtrl"
        | "wxFontPickerEvent"
        | "wxFrame"
        | "wxGBSizerItem"
        | "wxGCDC"
        | "wxGLCanvas"
        | "wxGLContext"
        | "wxGauge"
        | "wxGenericDirCtrl"
        | "wxGraphicsBrush"
        | "wxGraphicsContext"
        | "wxGraphicsFont"
        | "wxGraphicsGradientStops"
        | "wxGraphicsMatrix"
        | "wxGraphicsObject"
        | "wxGraphicsPath"
        | "wxGraphicsPen"
        | "wxGraphicsRenderer"
        | "wxGrid"
        | "wxGridBagSizer"
        | "wxGridCellAttr"
        | "wxGridCellBoolEditor"
        | "wxGridCellBoolRenderer"
        | "wxGridCellChoiceEditor"
        | "wxGridCellEditor"
        | "wxGridCellFloatEditor"
        | "wxGridCellFloatRenderer"
        | "wxGridCellNumberEditor"
        | "wxGridCellNumberRenderer"
        | "wxGridCellRenderer"
        | "wxGridCellStringRenderer"
        | "wxGridCellTextEditor"
        | "wxGridEvent"
        | "wxGridSizer"
        | "wxHelpEvent"
        | "wxHtmlEasyPrinting"
        | "wxHtmlLinkEvent"
        | "wxHtmlWindow"
        | "wxIcon"
        | "wxIconBundle"
        | "wxIconizeEvent"
        | "wxIdleEvent"
        | "wxImage"
        | "wxImageList"
        | "wxInitDialogEvent"
        | "wxJoystickEvent"
        | "wxKeyEvent"
        | "wxLayoutAlgorithm"
        | "wxListBox"
        | "wxListCtrl"
        | "wxListEvent"
        | "wxListItem"
        | "wxListItemAttr"
        | "wxListView"
        | "wxListbook"
        | "wxLocale"
        | "wxLogNull"
        | "wxMDIChildFrame"
        | "wxMDIClientWindow"
        | "wxMDIParentFrame"
        | "wxMask"
        | "wxMaximizeEvent"
        | "wxMemoryDC"
        | "wxMenu"
        | "wxMenuBar"
        | "wxMenuEvent"
        | "wxMenuItem"
        | "wxMessageDialog"
        | "wxMiniFrame"
        | "wxMirrorDC"
        | "wxMouseCaptureChangedEvent"
        | "wxMouseCaptureLostEvent"
        | "wxMouseEvent"
        | "wxMoveEvent"
        | "wxMultiChoiceDialog"
        | "wxNavigationKeyEvent"
        | "wxNotebook"
        | "wxNotificationMessage"
        | "wxNotifyEvent"
        | "wxOverlay"
        | "wxPageSetupDialog"
        | "wxPageSetupDialogData"
        | "wxPaintDC"
        | "wxPaintEvent"
        | "wxPalette"
        | "wxPaletteChangedEvent"
        | "wxPanel"
        | "wxPasswordEntryDialog"
        | "wxPen"
        | "wxPickerBase"
        | "wxPopupTransientWindow"
        | "wxPopupWindow"
        | "wxPostScriptDC"
        | "wxPreviewCanvas"
        | "wxPreviewControlBar"
        | "wxPreviewFrame"
        | "wxPrintData"
        | "wxPrintDialog"
        | "wxPrintDialogData"
        | "wxPrintPreview"
        | "wxPrinter"
        | "wxPrintout"
        | "wxProgressDialog"
        | "wxQueryNewPaletteEvent"
        | "wxRadioBox"
        | "wxRadioButton"
        | "wxRegion"
        | "wxSashEvent"
        | "wxSashLayoutWindow"
        | "wxSashWindow"
        | "wxScreenDC"
        | "wxScrollBar"
        | "wxScrollEvent"
        | "wxScrollWinEvent"
        | "wxScrolledWindow"
        | "wxSetCursorEvent"
        | "wxShowEvent"
        | "wxSingleChoiceDialog"
        | "wxSizeEvent"
        | "wxSizer"
        | "wxSizerFlags"
        | "wxSizerItem"
        | "wxSlider"
        | "wxSpinButton"
        | "wxSpinCtrl"
        | "wxSpinEvent"
        | "wxSplashScreen"
        | "wxSplitterEvent"
        | "wxSplitterWindow"
        | "wxStaticBitmap"
        | "wxStaticBox"
        | "wxStaticBoxSizer"
        | "wxStaticLine"
        | "wxStaticText"
        | "wxStatusBar"
        | "wxStdDialogButtonSizer"
        | "wxStyledTextCtrl"
        | "wxStyledTextEvent"
        | "wxSysColourChangedEvent"
        | "wxSystemOptions"
        | "wxSystemSettings"
        | "wxTaskBarIcon"
        | "wxTaskBarIconEvent"
        | "wxTextAttr"
        | "wxTextCtrl"
        | "wxTextDataObject"
        | "wxTextEntryDialog"
        | "wxToggleButton"
        | "wxToolBar"
        | "wxToolTip"
        | "wxToolbook"
        | "wxTopLevelWindow"
        | "wxTreeCtrl"
        | "wxTreeEvent"
        | "wxTreebook"
        | "wxUpdateUIEvent"
        | "wxWebView"
        | "wxWebViewEvent"
        | "wxWindow"
        | "wxWindowCreateEvent"
        | "wxWindowDC"
        | "wxWindowDestroyEvent"
        | "wxXmlResource"
        | "wx_misc"
        | "wx_object"
        | "wxe_master"
        | "wxe_server"
        | "wxe_util"
        | "xmerl"
        | "xmerl_b64Bin"
        | "xmerl_b64Bin_scan"
        | "xmerl_eventp"
        | "xmerl_html"
        | "xmerl_lib"
        | "xmerl_otpsgml"
        | "xmerl_regexp"
        | "xmerl_sax_old_dom"
        | "xmerl_sax_parser"
        | "xmerl_sax_parser_latin1"
        | "xmerl_sax_parser_list"
        | "xmerl_sax_parser_utf16be"
        | "xmerl_sax_parser_utf16le"
        | "xmerl_sax_parser_utf8"
        | "xmerl_sax_simple_dom"
        | "xmerl_scan"
        | "xmerl_sgml"
        | "xmerl_simple"
        | "xmerl_text"
        | "xmerl_ucs"
        | "xmerl_uri"
        | "xmerl_validate"
        | "xmerl_xlate"
        | "xmerl_xml"
        | "xmerl_xpath"
        | "xmerl_xpath_lib"
        | "xmerl_xpath_parse"
        | "xmerl_xpath_pred"
        | "xmerl_xpath_scan"
        | "xmerl_xs"
        | "xmerl_xsd"
        | "xmerl_xsd_type"
        | "xref"
        | "xref_base"
        | "xref_compiler"
        | "xref_parser"
        | "xref_reader"
        | "xref_scanner"
        | "xref_utils"
        | "yecc"
        | "yeccparser"
        | "yeccscan"
        | "zip"
        | "zlib" => (),
        _ => return Ok(()),
    }

    Err(Error::GleamModuleWouldOverwriteStandardErlangModule {
        name: input.name.clone(),
        path: input.path.to_owned(),
    })
}
#[derive(Debug, Default)]
pub struct StaleTracker(HashSet<EcoString>);

impl StaleTracker {
    fn add(&mut self, name: EcoString) {
        _ = self.0.insert(name);
    }

    fn includes_any(&self, names: &[(EcoString, SrcSpan)]) -> bool {
        names.iter().any(|n| self.0.contains(n.0.as_str()))
    }

    pub fn empty(&mut self) {
        let _ = self.0.drain(); // Clears the set but retains allocated memory
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

#[derive(Debug)]
pub struct Inputs<'a> {
    collection: HashMap<EcoString, (DefinedModuleOrigin, Input)>,
    already_defined_modules: &'a mut im::HashMap<EcoString, DefinedModuleOrigin>,
}

impl<'a> Inputs<'a> {
    fn new(already_defined_modules: &'a mut im::HashMap<EcoString, DefinedModuleOrigin>) -> Self {
        Self {
            collection: Default::default(),
            already_defined_modules,
        }
    }

    /// Insert a module into the hashmap. If there is already a module with the
    /// same name then an error is returned.
    fn insert(&mut self, input: Input, origin: PackageKind) -> Result<()> {
        let name = input.name().clone();

        let origin = match origin {
            PackageKind::Root => DefinedModuleOrigin::RootProject {
                path: input.source_path().to_path_buf(),
            },
            PackageKind::Dependency { package_name } => {
                DefinedModuleOrigin::Dependency { package_name }
            }
        };

        if let Some(first) = self
            .already_defined_modules
            .insert(name.clone(), origin.clone())
        {
            return Err(Error::DuplicateModule {
                module: name.clone(),
                first,
                second: origin,
            });
        }

        if let Some((first, _)) = self
            .collection
            .insert(name.clone(), (origin.clone(), input))
        {
            return Err(Error::DuplicateModule {
                module: name,
                first,
                second: origin,
            });
        }

        Ok(())
    }
}

/// A Gleam source file (`.gleam`) and the module name deduced from it
pub struct GleamFile {
    pub path: Utf8PathBuf,
    pub module_name: EcoString,
}

impl GleamFile {
    pub fn new(dir: &Utf8Path, path: Utf8PathBuf) -> Self {
        Self {
            module_name: Self::module_name(&path, &dir),
            path,
        }
    }

    /// Iterates over Gleam source files (`.gleam`) in a certain directory.
    /// Symlinks are followed.
    /// If the there is a .gleam file with a path that would be an
    /// invalid module name it should not be loaded. For example, if it
    /// has a uppercase letter in it.
    pub fn iterate_files_in_directory<'b>(
        io: &'b impl FileSystemReader,
        dir: &'b Utf8Path,
    ) -> impl Iterator<Item = Result<Self, crate::Warning>> + 'b {
        tracing::trace!("gleam_source_files {:?}", dir);
        files_with_extension(io, dir, "gleam").map(move |path| {
            if (Self::is_gleam_path(&path, &dir)) {
                Ok(Self::new(dir, path))
            } else {
                Err(crate::Warning::InvalidSource { path })
            }
        })
    }

    pub fn cache_files(&self, artefact_directory: &Utf8Path) -> CacheFiles {
        CacheFiles::new(artefact_directory, &self.module_name)
    }

    fn module_name(path: &Utf8Path, dir: &Utf8Path) -> EcoString {
        // /path/to/project/_build/default/lib/the_package/src/my/module.gleam

        // my/module.gleam
        let mut module_path = path
            .strip_prefix(dir)
            .expect("Stripping package prefix from module path")
            .to_path_buf();

        // my/module
        let _ = module_path.set_extension("");

        // Stringify
        let name = module_path.to_string();

        // normalise windows paths
        name.replace("\\", "/").into()
    }

    fn is_gleam_path(path: &Utf8Path, dir: &Utf8Path) -> bool {
        use regex::Regex;
        use std::cell::OnceCell;
        const RE: OnceCell<Regex> = OnceCell::new();

        RE.get_or_init(|| {
            Regex::new(&format!(
                "^({module}{slash})*{module}\\.gleam$",
                module = "[a-z][_a-z0-9]*",
                slash = "(/|\\\\)",
            ))
            .expect("is_gleam_path() RE regex")
        })
        .is_match(
            path.strip_prefix(dir)
                .expect("is_gleam_path(): strip_prefix")
                .as_str(),
        )
    }
}

/// The collection of cache files paths related to a module.
/// These files are not guaranteed to exist.
pub struct CacheFiles {
    pub cache_path: Utf8PathBuf,
    pub meta_path: Utf8PathBuf,
    pub warnings_path: Utf8PathBuf,
    pub inline_path: Utf8PathBuf,
}

impl CacheFiles {
    pub fn new(artefact_directory: &Utf8Path, module_name: &EcoString) -> Self {
        let file_name = module_name.replace("/", "@");
        let cache_path = artefact_directory
            .join(file_name.as_str())
            .with_extension("cache");
        let meta_path = artefact_directory
            .join(file_name.as_str())
            .with_extension("cache_meta");
        let warnings_path = artefact_directory
            .join(file_name.as_str())
            .with_extension("cache_warnings");
        let inline_path = artefact_directory
            .join(file_name.as_str())
            .with_extension("cache_inline");

        Self {
            cache_path,
            meta_path,
            warnings_path,
            inline_path,
        }
    }

    pub fn delete(&self, io: &dyn io::FileSystemWriter) -> Result<()> {
        io.delete_file(&self.cache_path)?;
        io.delete_file(&self.meta_path)?;
        io.delete_file(&self.warnings_path)?;
        io.delete_file(&self.inline_path)
    }

    /// Iterates over `.cache_meta` files in the given directory,
    /// and returns the respective module names.
    /// Symlinks are followed.
    pub fn modules_with_meta_files<'a>(
        io: &'a impl FileSystemReader,
        dir: &'a Utf8Path,
    ) -> impl Iterator<Item = EcoString> + 'a {
        tracing::trace!("CacheFiles::modules_with_meta_files {:?}", dir);
        files_with_extension(io, dir, "cache_meta").map(move |path| Self::module_name(&dir, &path))
    }

    fn module_name(dir: &Utf8Path, path: &Utf8Path) -> EcoString {
        // /path/to/artefact/dir/my@module.cache_meta

        // my@module.cache_meta
        let mut module_path = path
            .strip_prefix(dir)
            .expect("Stripping package prefix from module path")
            .to_path_buf();

        // my@module
        let _ = module_path.set_extension("");

        // my/module
        module_path.to_string().replace("@", "/").into()
    }
}
