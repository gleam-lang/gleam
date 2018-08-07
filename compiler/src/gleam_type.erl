-module(gleam_type).

-export([infer/1, fetch/1]).

-include("gleam_records.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


-record(env, {level = 1 :: level(), vars = #{}}).
-type env() :: #env{}.
-type var_name() :: string().

% let new_var level = TVar (ref (Unbound (next_id (), level)))

% let new_gen_var () = TVar (ref (Generic (next_id ())))

% let rec infer env level ast =
%   match ast with
%   | Var name -> (
%       try instantiate level (Env.lookup env name) with Not_found ->
%         error ("variable " ^ name ^ " not found") )
%   | Fun (param_list, body_expr) ->
%       let param_ty_list = List.map (fun _ -> new_var level) param_list in
%       let fn_env =
%         List.fold_left2
%           (fun env param_name param_ty -> Env.extend env param_name param_ty)
%           env param_list param_ty_list
%       in
%       let return_ty = infer fn_env level body_expr in
%       TArrow (param_ty_list, return_ty)
%   | Let (var_name, value_expr, body_expr) ->
%       let var_ty = infer env (level + 1) value_expr in
%       let generalized_ty = generalize level var_ty in
%       infer (Env.extend env var_name generalized_ty) level body_expr
%   | Call (fn_expr, arg_list) ->
%       let param_ty_list, return_ty =
%         match_fun_ty (List.length arg_list) (infer env level fn_expr)
%       in
%       List.iter2
%         (fun param_ty arg_expr -> unify param_ty (infer env level arg_expr))
%         param_ty_list arg_list ;
%       return_ty

-spec infer(ast_expression()) -> {ok, ast_expression()}.
infer(Ast) ->
  try
    {NewAst, _Env} = infer(Ast, new_env()),
    {ok, NewAst}
  catch
    throw:{gleam_type_error, Error} -> {error, Error}
  end.

-spec infer(ast_expression(), env()) -> {ast_expression(), env()}.
infer(Ast = #ast_var{name = Name}, Env) ->
  case env_lookup(Name, Env) of
    {ok, Type} ->
      {InstantiatedType, NewEnv} = instantiate(Type, Env),
      AnnotatedAst = Ast#ast_var{type = {ok, InstantiatedType}},
      {AnnotatedAst, NewEnv};

    error ->
      fail({var_not_found, Ast})
  end;

infer(#ast_assignment{name = Name, value = Value, then = Then}, Env) ->
  {InferredValue, Env2} = infer(Value, increment_env_level(Env)),
  GeneralizedType = generalize(Env#env.level, fetch(InferredValue)),
  ExtendedEnv = env_extend(Name, GeneralizedType, Env2),
  infer(Then, ExtendedEnv);

infer(Ast = #ast_tuple{elems = Elems}, Env) ->
  {AnnotatedElems, NewEnv} = gleam:thread_map(fun infer/2, Elems, Env),
  AnnotatedAst = Ast#ast_tuple{elems = AnnotatedElems},
  {AnnotatedAst, NewEnv};

infer(Ast = #ast_int{}, Env) ->
  {Ast, Env};

infer(Ast = #ast_float{}, Env) ->
  {Ast, Env};

infer(Ast = #ast_string{}, Env) ->
  {Ast, Env};

infer(Ast = #ast_atom{}, Env) ->
  {Ast, Env}.

-spec fetch(ast_expression()) -> type().
fetch(#ast_var{type = {ok, Type}}) ->
  Type;

fetch(#ast_tuple{elems = Elems}) ->
  ElemsTypes = lists:map(fun fetch/1, Elems),
  #type_tuple{elems = ElemsTypes};

fetch(#ast_int{}) ->
  #type_const{type = int};

fetch(#ast_atom{}) ->
  #type_const{type = atom};

fetch(#ast_float{}) ->
  #type_const{type = float};

fetch(#ast_string{}) ->
  #type_const{type = string}.

-spec new_env() -> env().
new_env() ->
  #env{}.

-spec fail(tuple()) -> no_return().
fail(Error) ->
  throw({gleam_type_error, Error}).

-spec increment_env_level(env()) -> env().
increment_env_level(Env = #env{level = Level}) ->
  Env#env{level = Level + 1}.

-spec env_level(env()) -> level().
env_level(#env{level = Level}) ->
  Level.

-ifdef(TEST).
increment_env_level_test() ->
  Env = new_env(),
  Env2 = Env#env{level = 42},
  Env3 = increment_env_level(Env2),
  ?assertEqual(43, Env3#env.level).
-endif.

-spec env_extend(var_name(), type(), env()) -> env().
env_extend(Name, GeneralizedType, Env = #env{vars = Vars}) ->
  NewVars = maps:put(Name, GeneralizedType, Vars),
  Env#env{vars = NewVars}.

-spec env_lookup(var_name(), env()) -> error | {ok, type()}.
env_lookup(Name, #env{vars = Vars}) ->
  maps:find(Name, Vars).

% let rec generalize level tvar =
%   match tvar with
%   | TVar {contents= Unbound (id, other_level)} when other_level > level ->
%       TVar (ref (Generic id))
%   | TApp (ty, ty_arg_list) ->
%       TApp (generalize level ty, List.map (generalize level) ty_arg_list)
%   | TArrow (param_ty_list, return_ty) ->
%       let params = List.map (generalize level) param_ty_list in
%       let ret = generalize level return_ty in
%       TArrow (params, ret)
%   | TVar {contents= Link ty} -> generalize level ty
%   | (TVar {contents= Generic _} | TVar {contents= Unbound _} | TConst _) as ty ->
%       ty

-spec generalize(level(), type()) -> type().
generalize(_Level, Type) ->
  Type.

% let instantiate level ty =
%   let id_var_map = Hashtbl.create 10 in
%   let rec f ty =
%     match ty with
%     | TConst _ ->
%         ty
%     | TVar {contents= Link ty} ->
%         f ty
%     | TVar {contents= Generic id} -> (
%         try Hashtbl.find id_var_map id with Not_found ->
%           let var = new_var level in
%           Hashtbl.add id_var_map id var ;
%           var )
%     | TVar {contents= Unbound _} ->
%         ty
%     | TApp (ty, ty_arg_list) ->
%         TApp (f ty, List.map f ty_arg_list)
%     | TArrow (param_ty_list, return_ty) ->
%         TArrow (List.map f param_ty_list, f return_ty)
%   in
%   f ty

-spec instantiate(type(), env()) -> {type(), env()}.
instantiate(Type = #type_const{}, Env) ->
  {Type, Env}.

% let occurs_check_adjust_levels tvar_id tvar_level ty =
%   let rec f = function
%     | TVar {contents= Link ty} -> f ty
%     | TVar {contents= Generic _} -> assert false
%     | TVar ({contents= Unbound (other_id, other_level)} as other_tvar) ->
%         if other_id = tvar_id then error "recursive types"
%         else if other_level > tvar_level then
%           other_tvar := Unbound (other_id, tvar_level)
%         else ()
%     | TApp (ty, ty_arg_list) -> f ty ; List.iter f ty_arg_list
%     | TArrow (param_ty_list, return_ty) ->
%         List.iter f param_ty_list ; f return_ty
%     | TConst _ -> ()
%   in
%   f ty

% let rec unify ty1 ty2 =
%   if ty1 == ty2 then ()
%   else
%     match (ty1, ty2) with
%     | TConst name1, TConst name2 when name1 = name2 -> ()
%     | TApp (ty1, ty_arg_list1), TApp (ty2, ty_arg_list2) ->
%         unify ty1 ty2 ;
%         List.iter2 unify ty_arg_list1 ty_arg_list2
%     | TArrow (param_ty_list1, return_ty1), TArrow (param_ty_list2, return_ty2) ->
%         List.iter2 unify param_ty_list1 param_ty_list2 ;
%         unify return_ty1 return_ty2
%     | TVar {contents= Link ty1}, ty2 | ty1, TVar {contents= Link ty2} ->
%         unify ty1 ty2
%     | TVar {contents= Unbound (id1, _)}, TVar {contents= Unbound (id2, _)}
%       when id1 = id2 ->
%         assert false
%         (* There is only a single instance of a particular type variable. *)
%     | TVar ({contents= Unbound (id, level)} as tvar), ty
%      |ty, TVar ({contents= Unbound (id, level)} as tvar) ->
%         occurs_check_adjust_levels id level ty ;
%         tvar := Link ty
%     | _, _ ->
%         error
%           ( "cannot unify types " ^ string_of_ty ty1 ^ " and "
%           ^ string_of_ty ty2 )

% let rec match_fun_ty num_params tvar =
%   match tvar with
%   | TArrow (param_ty_list, return_ty) ->
%       if List.length param_ty_list <> num_params then
%         error "unexpected number of arguments"
%       else (param_ty_list, return_ty)
%   | TVar {contents= Link ty} -> match_fun_ty num_params ty
%   | TVar ({contents= Unbound (id, level)} as tvar) ->
%       let param_ty_list =
%         let rec f = function 0 -> [] | n -> new_var level :: f (n - 1) in
%         f num_params
%       in
%       let return_ty = new_var level in
%       tvar := Link (TArrow (param_ty_list, return_ty)) ;
%       (param_ty_list, return_ty)
%   | _ -> error "expected a function"
