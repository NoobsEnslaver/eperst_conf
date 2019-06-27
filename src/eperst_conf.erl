-module(eperst_conf).

-export([get_env/2,
         get_env/3,
         get_all_env/0,
         get_all_env/1,
         set_env/3,
         reload/0,
         reload/1,
         notify_app_conf_changed/2]).

-define(APP_NAME, '$eperst_conf').
-define(MAKE_KEY(App, Key), {?APP_NAME, App, Key}).

-spec get_env(atom(), term()) -> term().
get_env(App, Key) ->
    get_env(App, Key, undefined).

-spec get_env(atom(), term(), term()) -> term().
get_env(App, Key, Default) ->
    persistent_term:get(?MAKE_KEY(App, Key), Default).

-spec set_env(atom(), term(), term()) -> ok.
set_env(App, Key, Value) ->
    persistent_term:put(?MAKE_KEY(App, Key), Value).

-spec reload() -> {ok, proplists:proplist()} | {error, term()}.
reload() ->
    eperst_conf_utils:with_sys_config(
      fun(SysConf)->
              Apps = [App || {App,_,_} <- application:which_applications()],
              eperst_conf_utils:with_app_config(Apps,
                fun(AppConfs) ->
                        NewEnv = eperst_conf_utils:merge_env(AppConfs, SysConf),
                        _ = [begin
                                 OldEnv = get_all_env(App),
                                 _ = [set_env(App, K, V) || {K, V} <- Env],
                                 notify_app_conf_changed(App, OldEnv)
                             end || {App, Env} <- NewEnv],
                        {ok, NewEnv}
                end)
      end).

-spec reload(atom()) -> {ok, proplists:proplist()} | {error, term()}.
reload(App) ->
    eperst_conf_utils:with_sys_config(
      fun(SysConf)->
              AppSysEnv = eperst_conf_utils:get_opt(App, SysConf, []),
              eperst_conf_utils:with_app_config(App,
                fun(AppEnv)->
                        OldEnv = get_all_env(App),
                        NewEnv = eperst_conf_utils:merge_app_env(AppEnv, AppSysEnv),
                        _ = [set_env(App, K, V) || {K, V} <- NewEnv],
                        notify_app_conf_changed(App, OldEnv),
                        {ok, NewEnv}
                end)
      end).

-spec get_all_env() -> proplists:proplist().
get_all_env() ->
    lists:foldl(fun({?MAKE_KEY(App, Key), Value}, Acc) ->
                        eperst_conf_utils:merge_env(Acc, [{App, [{Key, Value}]}]);
                    (_, Acc) -> Acc
                end, [], persistent_term:get()).

-spec get_all_env(atom()) -> proplists:proplist().
get_all_env(App) ->
    lists:foldl(fun({?MAKE_KEY(A, Key), Value}, Acc) when A == App ->
                        eperst_conf_utils:merge_env(Acc, [{App, [{Key, Value}]}]);
                    (_, Acc) -> Acc
                end, [], persistent_term:get()).


-spec notify_app_conf_changed(atom(), proplists:proplist()) -> ok | {error, term()}.
notify_app_conf_changed(App, OldEnv) ->
    case application:get_key(App, mod) of
        {ok, {Mod, _Para}} ->
            NowEnv = get_all_env(App),
            case eperst_conf_utils:config_diff(NowEnv, OldEnv) of
                {[], [], []} ->
                    ok;
                {Changed, New, Removed} ->
                    try Mod:config_change(Changed, New, Removed) of
                        ok ->
                            ok;
                        Else ->
                            {error, Else}
                    catch
                        error:undef ->
                            ok;
                        C:R ->
                            {error, {C,R}}
                    end
            end;
        {ok, []} ->
            {error, {module_not_defined, App}};
        undefined ->
            {error, {application_not_found, App}}
    end.
