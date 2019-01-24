-include_lib("astranaut/include/macro.hrl").
-use_macro({gen_fun_macro, gen_fun/2, [{attrs, [erlando_type]}, {as_attr, gen_fun}, 
                                       {auto_export, true}, {merge_function, head}]}).
%-compile({parse_transform, function_generator}).
