-include_lib("astranaut/include/macro.hrl").
-ifndef(ERLANDO_GEN_FUN).
-define(ERLANDO_GEN_FUN, true).
-import_macro(gen_fun_macro).
%% -use_macro({gen_fun_macro, gen_fun/2, [{inject_attrs, [erlando_type]}, {as_attr, gen_fun}, 
%%                                        {auto_export, true}, {merge_function, head}]}).
-use_macro({gen_fun_macro, gen_fun/2, []}).
-endif.
