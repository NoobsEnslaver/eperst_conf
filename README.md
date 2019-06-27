eperst_conf
=====

This lib provides fast replacement of OTP `application_controller` through using `persistent_term` storage.
Main API keeps comatibe with `application`, like `get_env`, `set_env` etc.
Support of `sys.config` and `.app` files provides, so as `change_config/3` OTP callback.

API
-----

| Function | Description |
-----------|-------------|
| get_env(App,Key) | The same as `get_env(App,Key,undefined)`|
| get_env(App,Key,Def) | Read value of Key from App enviroment storage, if it doesn't exists return Def |
| get_all_env() | Return all envitoment in format [{App, Env}, ..] |
| get_all_env(App) | Return enviroment of application App |
| set_env(App,Key,Val) | Associate value Val with Key for application App |
| reload() | Read all `.config` and `.app` files, merge, store it in persistent_term storage and call callback `change_config/3` for all apps with not empty config diff |
| reload(App) | Same as `reload/0`, but only for one application App |
| notify_app_conf_changed(App, OldEnv) | Calculate diff with current enviroment for App and call `change_config/3` |