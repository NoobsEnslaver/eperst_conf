-module(eperst_conf_utils).

-export([get_sys_conf/0,
         get_app_conf/1,
         validate_conf/1,
         merge_env/2,
         merge_app_env/2,
         get_opt/3,
         with_sys_config/1,
         with_app_config/2]).

with_sys_config(Fun) when is_function(Fun, 1) ->
    try get_sys_conf() of
        {ok, SysConf} ->
            case validate_conf(SysConf) of
                ok    -> Fun(SysConf);
                Error -> Error
            end;
        Error -> Error
    catch
        _:Error -> Error
    end.

with_app_config(Apps, Fun) when is_list(Apps), is_function(Fun, 1) ->
    Confs = lists:foldl(fun(_, {error, _} = Error)-> Error;
                           (App, Acc) ->
                                case with_app_config(App, fun id/1) of
                                    {ok, Env} ->
                                        [{App, Env} | Acc];
                                    Error ->
                                        Error
                                end
                        end, [], Apps),
    Fun(Confs);
with_app_config(App, Fun) when is_atom(App), is_function(Fun, 1) ->
    try get_app_conf(App) of
        {ok, AppConf} ->
            case validate_conf([{App, AppConf}]) of
                ok    -> Fun(AppConf);
                Error -> Error
            end;
        Error -> Error
    catch
        _:Error -> Error
    end.

-spec get_app_conf(atom()) -> {ok, proplists:proplist()} | {error, term()} | no_return().
get_app_conf(Name) when is_atom(Name) ->
    FName = atom_to_list(Name) ++ ".app",
    case code:where_is_file(FName) of
        non_existing ->
            {error, non_existing};
        FullName ->
            case prim_consult(FullName) of
                {ok, [{application, Name, Opts}]} ->
                    {ok, get_opt(env, Opts, [])};
                {error, Reason} ->
                    {error, {file:format_error(Reason), FName}};
                error ->
                    {error, bad_encoding}
            end
    end.

-spec get_sys_conf() -> {ok, proplists:proplist()} | {error, non_existing} | no_return().
get_sys_conf() ->
    case init:get_argument(config) of
        {ok, Files} ->
            {ok, lists:foldl(
                   fun([File], Env) ->
                           BFName = filename:basename(File,".config"),
                           FName = filename:join(filename:dirname(File),
                                                 BFName ++ ".config"),
                           case load_file(FName) of
                               {ok, NewEnv} ->
                                   case BFName of
                                       "sys" ->
                                           DName = filename:dirname(FName),
                                           {ok, SysEnv, Errors} =
                                               check_conf_sys(NewEnv, [], [], DName),
                                           case Errors of
                                               [] ->
                                                   merge_env(Env, SysEnv);
                                               [{error, {SysFName, Line, Str}}|_] ->
                                                   throw({error, {SysFName, Line, Str}})
                                           end;
                                       _ ->
                                           merge_env(Env, NewEnv)
                                   end;
                               {error, {Line, _Mod, Str}} ->
                                   throw({error, {FName, Line, Str}})
                           end
                   end, [], Files)};
        _ -> {error, non_existing}
    end.

-spec validate_conf(proplists:proplist()) -> ok | {error, term()}.
validate_conf([]) ->
    ok;
validate_conf([Application | ConfDataRem]) ->
    case Application of
        {AppName, List} when is_atom(AppName), is_list(List) ->
            case lists:keymember(AppName, 1, ConfDataRem) of
                true ->
                    {error, "duplicate application config: " ++ atom_to_list(AppName)};
                false ->
                    case check_para(List, AppName) of
                        ok -> validate_conf(ConfDataRem);
                        Error -> Error
                    end
            end;
        {AppName, List} when is_list(List)  ->
            ErrMsg = "application: "
                ++ lists:flatten(io_lib:format("~tp",[AppName]))
                ++ "; application name must be an atom",
            {error, ErrMsg};
        {AppName, _List} ->
            ErrMsg = "application: "
                ++ lists:flatten(io_lib:format("~tp",[AppName]))
                ++ "; parameters must be a list",
            {error, ErrMsg};
        Else ->
            ErrMsg = "invalid application config: "
                ++ lists:flatten(io_lib:format("~tp",[Else])),
            {error, ErrMsg}
    end;
validate_conf(_ConfData) ->
    {error, "configuration must be a list ended by <dot><whitespace>"}.

-spec merge_env(proplists:proplist(), proplists:proplist()) -> proplists:proplist().
merge_env(Env1, Env2) ->
    merge_env(Env1, Env2, []).

merge_env([{App, AppEnv1} | T], Env2, Res) ->
    case get_env_key(App, Env2) of
        {value, AppEnv2, RestEnv2} ->
            NewAppEnv = merge_app_env(AppEnv1, AppEnv2),
            merge_env(T, RestEnv2, [{App, NewAppEnv} | Res]);
        _ ->
            merge_env(T, Env2, [{App, AppEnv1} | Res])
    end;
merge_env([], Env2, Res) ->
    Env2 ++ Res.

-spec merge_app_env(proplists:proplist(), proplists:proplist()) -> proplists:proplist().
merge_app_env(Env1, Env2) ->
    merge_app_env(Env1, Env2, []).

merge_app_env([{Key, Val} | T], Env2, Res) ->
    case get_env_key(Key, Env2) of
        {value, NewVal, RestEnv} ->
            merge_app_env(T, RestEnv, [{Key, NewVal}|Res]);
        _ ->
            merge_app_env(T, Env2, [{Key, Val} | Res])
    end;
merge_app_env([], Env2, Res) ->
    Env2 ++ Res.

-spec get_opt(term(), proplists:proplist(), term()) -> term().
get_opt(Key, List, Default) ->
    case lists:keyfind(Key, 1, List) of
        {_Key, Val} -> Val;
        _ -> Default
    end.

%% --------------- Internal ----------------------
prim_consult(FullName) ->
    case erl_prim_loader:get_file(FullName) of
        {ok, Bin, _} ->
            case file_binary_to_list(Bin) of
                {ok, String} ->
                    case erl_scan:string(String) of
                        {ok, Tokens, _EndLine} ->
                            prim_parse(Tokens, []);
                        {error, Reason, _EndLine} ->
                            {error, Reason}
                    end;
                error ->
                    error
            end;
        error ->
            {error, enoent}
    end.

prim_parse(Tokens, Acc) ->
    case lists:splitwith(fun(T) -> element(1,T) =/= dot end, Tokens) of
        {[], []} ->
            {ok, lists:reverse(Acc)};
        {Tokens2, [{dot,_} = Dot | Rest]} ->
            case erl_parse:parse_term(Tokens2 ++ [Dot]) of
                {ok, Term} ->
                    prim_parse(Rest, [Term | Acc]);
                {error, _R} = Error ->
                    Error
            end;
        {Tokens2, []} ->
            case erl_parse:parse_term(Tokens2) of
                {ok, Term} ->
                    {ok, lists:reverse([Term | Acc])};
                {error, _R} = Error ->
                    Error
            end
    end.

check_para([], _AppName) ->
    ok;
check_para([{Para, Val} | ParaList], AppName) when is_atom(Para) ->
    case lists:keymember(Para, 1, ParaList) of
        true ->
            ErrMsg =  "application: " ++ atom_to_list(AppName)
                ++ "; duplicate parameter: " ++ atom_to_list(Para),
            {error, ErrMsg};
        false ->
            case check_para_value(Para, Val, AppName) of
                ok -> check_para(ParaList, AppName);
                {error, _} = Error -> Error
            end
    end;
check_para([{Para, _Val} | _ParaList], AppName) ->
    {error, "application: " ++ atom_to_list(AppName) ++ "; invalid parameter name: " ++
     lists:flatten(io_lib:format("~tp",[Para]))};
check_para([Else | _ParaList], AppName) ->
    {error, "application: " ++ atom_to_list(AppName) ++ "; invalid parameter: " ++
     lists:flatten(io_lib:format("~tp",[Else]))}.

check_para_value(distributed, Apps, kernel) -> check_distributed(Apps);
check_para_value(_Para, _Val, _AppName) -> ok.

check_distributed([]) ->
    ok;
check_distributed([{App, List} | Apps]) when is_atom(App), is_list(List) ->
    check_distributed(Apps);
check_distributed([{App, infinity, List} | Apps]) when is_atom(App), is_list(List) ->
    check_distributed(Apps);
check_distributed([{App, Time, List} | Apps]) when is_atom(App), is_integer(Time), is_list(List) ->
    check_distributed(Apps);
check_distributed(_Else) ->
    {error, "application: kernel; erroneous parameter: distributed"}.

check_conf_sys([File|T], SysEnv, Errors, DName) when is_list(File),is_list(DName) ->
    BFName = filename:basename(File, ".config"),
    FName = filename:join(filename:dirname(File), BFName ++ ".config"),
    LName = case filename:pathtype(FName) of
               relative when (DName =/= []) ->
                  RName = filename:join(DName, FName),
                  case erl_prim_loader:read_file_info(RName) of
                     {ok, _} -> RName ;
                     error   -> FName
                  end;
                _          -> FName
            end,
    case load_file(LName) of
        {ok, NewEnv} ->
            check_conf_sys(T, merge_env(SysEnv, NewEnv), Errors, DName);
        {error, {Line, _Mod, Str}} ->
            check_conf_sys(T, SysEnv, [{error, {LName, Line, Str}}|Errors], DName)
    end;
check_conf_sys([Tuple|T], SysEnv, Errors, DName) ->
    check_conf_sys(T, merge_env(SysEnv, [Tuple]), Errors, DName);
check_conf_sys([], SysEnv, Errors, _) ->
    {ok, SysEnv, lists:reverse(Errors)}.

load_file(File) ->
    case erl_prim_loader:get_file(File) of
        {ok, Bin, _FileName} ->
            case file_binary_to_list(Bin) of
                {ok, String} ->
                    scan_file(String ++ " ");
                error ->
                    {error, {none, scan_file, "bad encoding"}}
            end;
        error ->
            {error, {none, open_file, "configuration file not found"}}
    end.

scan_file(Str) ->
    case erl_scan:tokens([], Str, 1) of
        {done, {ok, Tokens, _}, Left} ->
            case erl_parse:parse_term(Tokens) of
                {ok,L}=Res when is_list(L) ->
                    case only_ws(Left) of
                        true ->
                            Res;
                        false ->
                            config_error()
                    end;
                {ok,_} ->
                    config_error();
                Error ->
                    Error
            end;
        {done, Result, _} ->
            {error, {none, parse_file, tuple_to_list(Result)}};
        {more, _} ->
            {error, {none, load_file, "no ending <dot> found"}}
    end.

only_ws([C|Cs]) when C =< $\s -> only_ws(Cs);
only_ws([$%|Cs]) -> only_ws(strip_comment(Cs));   % handle comment
only_ws([_|_]) -> false;
only_ws([]) -> true.

strip_comment([$\n|Cs]) -> Cs;
strip_comment([_|Cs]) -> strip_comment(Cs);
strip_comment([]) -> [].

config_error() ->
    {error,
     {none, load_file,
      "configuration file must contain ONE list ended by <dot>"}}.

file_binary_to_list(Bin) ->
    Enc = case epp:read_encoding_from_binary(Bin) of
              none -> epp:default_encoding();
              Encoding -> Encoding
          end,
    case catch unicode:characters_to_list(Bin, Enc) of
        String when is_list(String) ->
            {ok, String};
        _ ->
            error
    end.

get_env_key(Key, Env) -> get_env_key(Env, Key, []).
get_env_key([{Key, Val} | T], Key, Res) ->
    {value, Val, T ++ Res};
get_env_key([H | T], Key, Res) ->
    get_env_key(T, Key, [H | Res]);
get_env_key([], _Key, Res) -> Res.

id({error, _} = E) ->
    E;
id(X) ->
    {ok, X}.
