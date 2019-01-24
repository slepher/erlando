-include_lib("astranaut/include/macro.hrl").
-ifndef(ERLANDO_GEN_FUN).
-define(ERLANDO_GEN_FUN, true).
-use_macro({gen_fun_macro, gen_fun/2, [{attrs, [erlando_type]}, {as_attr, gen_fun}, 
                                       {auto_export, true}, {merge_function, head}]}).
-endif.
