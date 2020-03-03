use crate::error::{Error, FileIOAction, FileKind, GleamExpect};
use serde::{Deserialize, Serialize};
use std::fs::File;
use std::io::Write;
use std::path::Path;
use std::path::PathBuf;
use strum_macros::{Display, EnumString, EnumVariantNames};

#[derive(Debug, Serialize, Deserialize, Display, EnumString, EnumVariantNames)]
#[strum(serialize_all = "kebab_case")]
pub enum Template {
    Lib,
    App,
}

pub fn create(
    template: Template,
    name: String,
    description: Option<String>,
    path: Option<String>,
    version: &'static str,
) -> Result<(), Error> {
    if !regex::Regex::new("^[a-z_]+$")
        .gleam_expect("new name regex could not be compiled")
        .is_match(&name)
    {
        // TODO
        println!("error: Project name may only contain lowercase letters and underscores");
        std::process::exit(1);
    }

    let description = description.unwrap_or_else(|| String::from("A Gleam program"));
    let path = path.unwrap_or_else(|| name.clone());
    let root_dir = Path::new(&path);
    let src_dir = root_dir.join("src");
    let test_dir = root_dir.join("test");
    let github_dir = root_dir.join(".github");
    let workflows_dir = github_dir.join("workflows");

    // Create directories
    mkdir(&root_dir)?;
    mkdir(&src_dir)?;
    mkdir(&test_dir)?;
    mkdir(&github_dir)?;
    mkdir(&workflows_dir)?;

    // write files
    write(root_dir.join("LICENSE"), APACHE_2)?;
    write(root_dir.join(".gitignore"), GITIGNORE)?;
    write(root_dir.join("README.md"), &readme(&name, &description))?;
    write(root_dir.join("gleam.toml"), &gleam_toml(&name))?;
    write(test_dir.join(format!("{}_test.gleam", name)), &test(&name))?;
    write(src_dir.join(format!("{}.gleam", name)), &src(&name))?;
    write(workflows_dir.join("test.yml"), &github_ci(version))?;

    match template {
        Template::Lib => {
            write(root_dir.join("rebar.config"), &rebar_config(""))?;
            write(
                src_dir.join(format!("{}.app.src", name)),
                &app_src(&name, &description, false),
            )?;
        }

        Template::App => {
            write(root_dir.join("rebar.config"), &app_rebar_config(&name))?;
            write(src_dir.join(format!("{}_app.erl", name)), &src_app(&name))?;
            write(
                src_dir.join(format!("{}.app.src", name)),
                &app_src(&name, &description, true),
            )?;
        }
    }

    // Print success message
    println!(
        "
Your Gleam project \"{}\" has been successfully created.
The rebar3 program can be used to compile and test it.

    cd {}
    rebar3 eunit
",
        name,
        root_dir.to_str().expect("Unable to display path")
    );
    Ok(())
}

fn mkdir(path: &Path) -> Result<(), Error> {
    std::fs::create_dir(path).map_err(|err| Error::FileIO {
        kind: FileKind::Directory,
        path: path.to_path_buf(),
        action: FileIOAction::Create,
        err: Some(err.to_string()),
    })
}

fn write(path: PathBuf, contents: &str) -> Result<(), Error> {
    println!(
        "* creating {}",
        path.to_str().expect("Unable to display write path")
    );
    let mut f = File::create(&*path).map_err(|err| Error::FileIO {
        kind: FileKind::File,
        path: path.clone(),
        action: FileIOAction::Create,
        err: Some(err.to_string()),
    })?;

    f.write_all(contents.as_bytes())
        .map_err(|err| Error::FileIO {
            kind: FileKind::File,
            path,
            action: FileIOAction::WriteTo,
            err: Some(err.to_string()),
        })?;
    Ok(())
}

fn gleam_toml(name: &str) -> String {
    format!("name = \"{}\"\n", name)
}

fn readme(name: &str, description: &str) -> String {
    format!(
        r#"# {}

{}


## Quick start

```sh
# Build the project
rebar3 compile

# Run the eunit tests
rebar3 eunit

# Run the Erlang REPL
rebar3 shell
```


## Installation

If [available in Hex](https://www.rebar3.org/docs/dependencies#section-declaring-dependencies)
this package can be installed by adding `{}` to your `rebar.config` dependencies:

```erlang
{{deps, [
    {}
]}}.
```
"#,
        name, description, name, name
    )
}

fn github_ci(version: &str) -> String {
    format!(
        r#"name: test

on:
  push:
    branches:
      - master
  pull_request:

jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2.0.0
      - uses: gleam-lang/setup-erlang@v1.0.0
        with:
          otp-version: 22.1
      - uses: gleam-lang/setup-gleam@v1.0.1
        with:
          gleam-version: {}
      - run: rebar3 install_deps
      - run: rebar3 eunit
"#,
        version
    )
}

fn app_src(name: &str, description: &str, is_application: bool) -> String {
    let module = if is_application {
        format!("\n  {{mod, {{{}_app, []}}}},", name)
    } else {
        "".to_string()
    };
    format!(
        r#"{{application, {},
 [{{description, "{}"}},
  {{vsn, "1.0.0"}},
  {{registered, []}},{}
  {{applications,
   [kernel,
    stdlib
   ]}},
  {{env,[]}},
  {{modules, []}},

  {{licenses, ["Apache 2.0"]}},
  {{links, []}}
]}}.
"#,
        name, description, module,
    )
}

fn src_app(name: &str) -> String {
    format!(
        r#"-module({}_app).

-behaviour(application).
-behaviour(supervisor).

-export([start/2, stop/1, init/1]).

start(_StartType, _StartArgs) ->
    supervisor:start_link({{local, ?MODULE}}, ?MODULE, []).

stop(_State) ->
    ok.

%% child_spec() = #{{id => child_id(),       % mandatory
%%                  start => mfargs(),       % mandatory
%%                  restart => restart(),    % optional
%%                  shutdown => shutdown(),  % optional
%%                  type => worker(),        % optional
%%                  modules => modules()}}   % optional
init([]) ->
    SupFlags = #{{strategy => one_for_all,
                 intensity => 0,
                 period => 1}},
    ChildSpecs = [],
    {{ok, {{SupFlags, ChildSpecs}}}}.
"#,
        name
    )
}

fn src(name: &str) -> String {
    format!(
        r#"pub fn hello_world() {{
  "Hello, from {}!"
}}
"#,
        name
    )
}

fn test(name: &str) -> String {
    format!(
        r#"import {}
import gleam/expect

pub fn hello_world_test() {{
  {}.hello_world()
  |> expect.equal(_, "Hello, from {}!")
}}
"#,
        name, name, name
    )
}

const APACHE_2: &str = r#"                                 Apache License
                           Version 2.0, January 2004
                        http://www.apache.org/licenses/

   TERMS AND CONDITIONS FOR USE, REPRODUCTION, AND DISTRIBUTION

   1. Definitions.

      "License" shall mean the terms and conditions for use, reproduction,
      and distribution as defined by Sections 1 through 9 of this document.

      "Licensor" shall mean the copyright owner or entity authorized by
      the copyright owner that is granting the License.

      "Legal Entity" shall mean the union of the acting entity and all
      other entities that control, are controlled by, or are under common
      control with that entity. For the purposes of this definition,
      "control" means (i) the power, direct or indirect, to cause the
      direction or management of such entity, whether by contract or
      otherwise, or (ii) ownership of fifty percent (50%) or more of the
      outstanding shares, or (iii) beneficial ownership of such entity.

      "You" (or "Your") shall mean an individual or Legal Entity
      exercising permissions granted by this License.

      "Source" form shall mean the preferred form for making modifications,
      including but not limited to software source code, documentation
      source, and configuration files.

      "Object" form shall mean any form resulting from mechanical
      transformation or translation of a Source form, including but
      not limited to compiled object code, generated documentation,
      and conversions to other media types.

      "Work" shall mean the work of authorship, whether in Source or
      Object form, made available under the License, as indicated by a
      copyright notice that is included in or attached to the work
      (an example is provided in the Appendix below).

      "Derivative Works" shall mean any work, whether in Source or Object
      form, that is based on (or derived from) the Work and for which the
      editorial revisions, annotations, elaborations, or other modifications
      represent, as a whole, an original work of authorship. For the purposes
      of this License, Derivative Works shall not include works that remain
      separable from, or merely link (or bind by name) to the interfaces of,
      the Work and Derivative Works thereof.

      "Contribution" shall mean any work of authorship, including
      the original version of the Work and any modifications or additions
      to that Work or Derivative Works thereof, that is intentionally
      submitted to Licensor for inclusion in the Work by the copyright owner
      or by an individual or Legal Entity authorized to submit on behalf of
      the copyright owner. For the purposes of this definition, "submitted"
      means any form of electronic, verbal, or written communication sent
      to the Licensor or its representatives, including but not limited to
      communication on electronic mailing lists, source code control systems,
      and issue tracking systems that are managed by, or on behalf of, the
      Licensor for the purpose of discussing and improving the Work, but
      excluding communication that is conspicuously marked or otherwise
      designated in writing by the copyright owner as "Not a Contribution."

      "Contributor" shall mean Licensor and any individual or Legal Entity
      on behalf of whom a Contribution has been received by Licensor and
      subsequently incorporated within the Work.

   2. Grant of Copyright License. Subject to the terms and conditions of
      this License, each Contributor hereby grants to You a perpetual,
      worldwide, non-exclusive, no-charge, royalty-free, irrevocable
      copyright license to reproduce, prepare Derivative Works of,
      publicly display, publicly perform, sublicense, and distribute the
      Work and such Derivative Works in Source or Object form.

   3. Grant of Patent License. Subject to the terms and conditions of
      this License, each Contributor hereby grants to You a perpetual,
      worldwide, non-exclusive, no-charge, royalty-free, irrevocable
      (except as stated in this section) patent license to make, have made,
      use, offer to sell, sell, import, and otherwise transfer the Work,
      where such license applies only to those patent claims licensable
      by such Contributor that are necessarily infringed by their
      Contribution(s) alone or by combination of their Contribution(s)
      with the Work to which such Contribution(s) was submitted. If You
      institute patent litigation against any entity (including a
      cross-claim or counterclaim in a lawsuit) alleging that the Work
      or a Contribution incorporated within the Work constitutes direct
      or contributory patent infringement, then any patent licenses
      granted to You under this License for that Work shall terminate
      as of the date such litigation is filed.

   4. Redistribution. You may reproduce and distribute copies of the
      Work or Derivative Works thereof in any medium, with or without
      modifications, and in Source or Object form, provided that You
      meet the following conditions:

      (a) You must give any other recipients of the Work or
          Derivative Works a copy of this License; and

      (b) You must cause any modified files to carry prominent notices
          stating that You changed the files; and

      (c) You must retain, in the Source form of any Derivative Works
          that You distribute, all copyright, patent, trademark, and
          attribution notices from the Source form of the Work,
          excluding those notices that do not pertain to any part of
          the Derivative Works; and

      (d) If the Work includes a "NOTICE" text file as part of its
          distribution, then any Derivative Works that You distribute must
          include a readable copy of the attribution notices contained
          within such NOTICE file, excluding those notices that do not
          pertain to any part of the Derivative Works, in at least one
          of the following places: within a NOTICE text file distributed
          as part of the Derivative Works; within the Source form or
          documentation, if provided along with the Derivative Works; or,
          within a display generated by the Derivative Works, if and
          wherever such third-party notices normally appear. The contents
          of the NOTICE file are for informational purposes only and
          do not modify the License. You may add Your own attribution
          notices within Derivative Works that You distribute, alongside
          or as an addendum to the NOTICE text from the Work, provided
          that such additional attribution notices cannot be construed
          as modifying the License.

      You may add Your own copyright statement to Your modifications and
      may provide additional or different license terms and conditions
      for use, reproduction, or distribution of Your modifications, or
      for any such Derivative Works as a whole, provided Your use,
      reproduction, and distribution of the Work otherwise complies with
      the conditions stated in this License.

   5. Submission of Contributions. Unless You explicitly state otherwise,
      any Contribution intentionally submitted for inclusion in the Work
      by You to the Licensor shall be under the terms and conditions of
      this License, without any additional terms or conditions.
      Notwithstanding the above, nothing herein shall supersede or modify
      the terms of any separate license agreement you may have executed
      with Licensor regarding such Contributions.

   6. Trademarks. This License does not grant permission to use the trade
      names, trademarks, service marks, or product names of the Licensor,
      except as required for reasonable and customary use in describing the
      origin of the Work and reproducing the content of the NOTICE file.

   7. Disclaimer of Warranty. Unless required by applicable law or
      agreed to in writing, Licensor provides the Work (and each
      Contributor provides its Contributions) on an "AS IS" BASIS,
      WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
      implied, including, without limitation, any warranties or conditions
      of TITLE, NON-INFRINGEMENT, MERCHANTABILITY, or FITNESS FOR A
      PARTICULAR PURPOSE. You are solely responsible for determining the
      appropriateness of using or redistributing the Work and assume any
      risks associated with Your exercise of permissions under this License.

   8. Limitation of Liability. In no event and under no legal theory,
      whether in tort (including negligence), contract, or otherwise,
      unless required by applicable law (such as deliberate and grossly
      negligent acts) or agreed to in writing, shall any Contributor be
      liable to You for damages, including any direct, indirect, special,
      incidental, or consequential damages of any character arising as a
      result of this License or out of the use or inability to use the
      Work (including but not limited to damages for loss of goodwill,
      work stoppage, computer failure or malfunction, or any and all
      other commercial damages or losses), even if such Contributor
      has been advised of the possibility of such damages.

   9. Accepting Warranty or Additional Liability. While redistributing
      the Work or Derivative Works thereof, You may choose to offer,
      and charge a fee for, acceptance of support, warranty, indemnity,
      or other liability obligations and/or rights consistent with this
      License. However, in accepting such obligations, You may act only
      on Your own behalf and on Your sole responsibility, not on behalf
      of any other Contributor, and only if You agree to indemnify,
      defend, and hold each Contributor harmless for any liability
      incurred by, or claims asserted against, such Contributor by reason
      of your accepting any such warranty or additional liability.

   END OF TERMS AND CONDITIONS

   Copyright {{copyright_year}}, {{author_name}} <{{author_email}}>.

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.

"#;

const GITIGNORE: &str = "*.beam
*.iml
*.o
*.plt
*.swo
*.swp
*~
.erlang.cookie
.eunit
.idea
.rebar
.rebar3
_*
_build
ebin
erl_crash.dump
gen
log
logs
rebar3.crashdump
";

fn rebar_config(insert: &str) -> String {
    format!(
        r#"{{erl_opts, [debug_info]}}.
{{src_dirs, ["src", "gen/src"]}}.

{{profiles, [
    {{test, [{{src_dirs, ["src", "test", "gen/src", "gen/test"]}}]}}
]}}.
{}
{{project_plugins, [rebar_gleam]}}.

{{deps, [
    {{gleam_stdlib, "0.7.0"}}
]}}.
"#,
        insert
    )
}

fn app_rebar_config(name: &str) -> String {
    rebar_config(&format!(
        r#"
{{shell, [
    % {{config, "config/sys.config"}},
    {{apps, [{}]}}
]}}.

"#,
        name
    ))
}
