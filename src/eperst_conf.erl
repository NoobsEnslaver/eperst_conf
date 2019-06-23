-module(eperst_conf).

-export([get_env/2,
         get_env/3,
         set_env/3,
         reload/0,
         reload/1]).

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

-spec reload() -> {ok, proplists:proplist()}.
reload() ->
    {ok, SysConf} = eperst_conf_utils:get_sys_conf(),
    AppConfs = [{App, Env} || {App,_,_} <- application:which_applications(),
                              {ok, Env} <- [eperst_conf_utils:get_app_conf(App)]],
    NewEnv = eperst_conf_utils:merge_app_env(AppConfs, SysConf),
    _ = [set_env(App, K, V) || {App, Env} <- NewEnv,
                               {K, V} <- Env],
    {ok, NewEnv}.

-spec reload(atom()) -> {ok, proplists:proplist()}.
reload(App) ->
    {ok, AppEnv} = eperst_conf_utils:get_app_conf(App),
    {ok, SysConf} = eperst_conf_utils:get_sys_conf(),
    AppSysEnv = eperst_conf_utils:get_opt(App, SysConf, []),
    NewEnv = eperst_conf_utils:merge_env(AppEnv, AppSysEnv),
    _ = [set_env(App, K, V) || {K, V} <- NewEnv],
    {ok, NewEnv}.
