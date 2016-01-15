% vim: ts=4 sw=4 et
-module (common).
-include_lib ("nitrogen_core/include/wf.hrl").
-compile(export_all).

main() -> #template { file="./templates/grid.html" }.

home_breadcrumb() ->
	#listitem{body = 
				  [#link{url = "/",
						 body =
							 [
							  "<i class=\"fa fa-dashboard\"></i>",
							  " Home"
							  ]
						}
				  ]
			 }.  
