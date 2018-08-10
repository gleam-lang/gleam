-module(gleam_decompiler).

-export([module_to_erlang/1, abstract_code/1]).

module_to_erlang(Mod) ->
  Code = abstract_code(Mod),
    Forms = erl_syntax:form_list(CoreForms),
    erl_prettypr:format(Forms).

abstract_code(Mod) ->
  File = code:which(Mod),

  case beam_lib:chunks(File, [debug_info]) of
    {ok, {Mod, [{debug_info, {debug_info_v1, Backend, {_, _, _} = Metadata}}]}} ->
      {ok, AbstractCode} = Backend:debug_info(erlang_v1, Mod, Metadata, []),
      AbstractCode;

    _ ->
      case beam_lib:chunks(File, [abstract_code]) of
        {ok, {Mod, [{abstract_code, {raw_abstract_v1, AbstractCode}}]}} ->
          AbstractCode;

        Other ->
          error({unable_to_get_abstract_code, Other})
      end
  end.

%   defp abstract_code(module) do
%     file = :code.which(module)

%     case :beam_lib.chunks(file, [:debug_info]) do
%       {:ok, {^module, [{:debug_info, {:debug_info_v1, backend, {_, _, _} = metadata}}]}} ->
%         {:ok, abstract_code} = backend.debug_info(:erlang_v1, module, metadata, [])
%         abstract_code

%       _ ->
%         case :beam_lib.chunks(file, [:abstract_code]) do
%           {:ok, {^module, [{:abstract_code, {:raw_abstract_v1, abstract_code}}]}} ->
%             abstract_code

%           _ ->
%             raise "abstract code unavailable"
%         end
%     end
%   end
