% vim: ts=4 sw=4 et
-module (index_test).
-include_lib ("nitrogen_core/include/wf.hrl").
-include("../include/records.hrl").

-compile(export_all).


-import(utils, [percent_to_colour/1]).

main() -> #template { file="./templates/basic.html" }.

title() -> "GPIO setup".

-define(HEADER( Tools, Icon, Title),
		[#panel{
				class = "box-header",  
			   body = [
						#panel{class = "pull-right box-tools",
							   body = Tools
							  },
						"<i class = \"" ++ Icon ++ "\"></i>",
						#h3{ class = "box-title", body = Title}

					  ]
			 }
		]
		).

-define(BOX_WITH_TYPE(Type, Header, Body, Footer), 
		#panel{class = "box " ++ Type,
			   body = [Header,
					   Body,
					   Footer
					  ]
			  }
			).

content_header() ->
	[
		title(),
		 "<small>", "Overview", "</small>"
	].

breadcrumb() ->
	[
		common:home_breadcrumb(),
		#listitem{class = "acitve", text = "Dashboard"}
	].

body() ->
	wait_for_msg(),
	Header = ?HEADER(
			[
			 ],
			 "fa fa-icon-bar-chart", 
			 ["GPIO Setup",
			  "<small>" ++ atom_to_list(node()) ++ "</small>"
			 ]
			),
	
	 ?BOX_WITH_TYPE("fa-danger",
						 Header,
						 [

						 get_gpio_status(),
	#dropdown{value=apple, options=[
	   #option{value=banana, text="Banana"},
	   #option{value=apple, text="Apple"},
	   #option{value=orange, text="Orange"}
	]},
	#dropdown{ options=[
	   #option{ text="Banana"},
	   #option{selected = true, text="Apple"},
	   #option{ text="Orange"}
   ]}					 
						 ], 
						 []).





get_gpio_status() ->
	
	#table{header = [
					 
					 #tableheader{text = "Pin#"} ,
					 #tableheader{text = "Name"} ,
					 #tableheader{text = "Type"} ,
					 #tableheader{text = "Value"} ,
					 #tableheader{text = ""}	 ,
					 #tableheader{text = ""}	,
					 #tableheader{text = "Value"} ,
					 #tableheader{text = "Type"} ,
					 #tableheader{text = "Name"} ,
					 #tableheader{text = "Pin#"} 
 
					],
		   rows = [#tablerow{cells = 
								 [row(LeftPin),
								  lists:reverse(row(RightPin))
								 ]
							}
				  ||
				   {LeftPin,RightPin} <- pins()]
		  }.

event({changed_drop, Pin}) ->
	ToggleName = "toggle" ++ integer_to_list(Pin),
	 DropName = "drop_down" ++ integer_to_list(Pin),
	DropValue = wf:q( DropName),
	case DropValue of
		"not set" ->
%% 			wf:wire(#add_class{class = "disabled"});
			wf:disable(ToggleName);
		"input" ->
%% 			wf:wire(#remove_class{remove_class = "disabled"});
			gpio_proc:set_type(Pin, input),
			wf:disable(ToggleName);
		"output" ->
%% 			wf:wire(#add_class{class = "disabled"});
			gpio_proc:set_type(Pin, output),
			wf:enable(ToggleName)
	end;

event({toggled, Pin}) ->
	ToggleName = "toggle" ++ integer_to_list(Pin),
	ToggleValue = wf:q(ToggleName),
	error_logger:info_msg("ToggleValue:~p", [ToggleValue]),
	case ToggleValue of
		[] ->
			gpio_proc:set_status(Pin, 1);
		_ ->
			gpio_proc:set_status(Pin, 0)
	end;

event(reconnect) ->
    wait_for_msg();



event(Any) ->
	error_logger:info_msg("~p ~p ~p", [?MODULE, ?LINE, Any]).

wait_for_msg() ->
    wf:wire(#comet{
        scope=global,
        pool=input_pins_comet,
        function=fun() -> loop() end,
        reconnect_actions=[
            #event{postback= reconnect}
        ]
    }).


loop() ->
	error_logger:info_msg("loop~n"),
	receive 
		{input_changed, PinNum, Status} ->
			ToggleName = "toggle" ++ integer_to_list(PinNum),
								error_logger:info_msg("~p ~p", [?MODULE, ?LINE]),

			Value = case Status of
				0 ->
					"off";
				1 ->
					"on"
			end,
			 					error_logger:info_msg("~p ~p", [?MODULE, ?LINE]),
			 wf:wire("toggleOn('"++"#wfid_"++ ToggleName++"')"), 
%% 			wf:set(ToggleName, Value),
								error_logger:info_msg("~p ~p", [?MODULE, ?LINE]),

			wf:flush();
		Msg ->
			error_logger:info_msg("Msg:~p", [Msg])
	end,
	loop().
	


to_binary(Value) when is_atom(Value) ->
    list_to_binary(atom_to_list(Value));
to_binary(Value)when is_integer(Value) ->
    list_to_binary(integer_to_list(Value)).

pid_to_binary(Pid) ->
    list_to_binary(pid_to_list(Pid)).


to_l(Int) when is_integer(Int) ->
	integer_to_list(Int);
to_l(Float) when is_float(Float) ->
	io_lib:format("~2.f", [Float]);
to_l(Any) ->
	Any.


pins() ->
	create_tuple_list([X || X<- lists:seq(1,40) , X rem 2 == 1],
					  [X || X<- lists:seq(1,40) , X rem 2 == 0]).

create_tuple_list([], []) ->
	[];
create_tuple_list([E|Tail], [First|Rest]) ->
	[{E, First}|
		create_tuple_list(Tail,Rest)].
 


-record(pin_info, {
				   gpio_id,
				   name,
				   radio_postback,
				   toggle_postback,
				   rg_enabled		= false,
			  set_type  = none,
			  status = 0, 
				   cb_shown			= false,
				   curr_status = 0
  				  }).


pin_info(1) ->
	#pin_info{
			  name = "3.3V <small>DC Power</small>",
			  rg_enabled = false,
			  cb_shown = false
			 };
pin_info(2) ->
%% 	{Type, Status} = gpio_proc:get_pin_status(2),
	#pin_info{
			  name = "5V <small>DC Power</small>"
			 };
pin_info(3) ->
%% 	{Type, Status} = gpio_proc:get_pin_status(2),
	#pin_info{
			  gpio_id = 2,
			  name = "GPIO 02 <small>(SDA1, I2C)</small>",
			  rg_enabled = true,
			  cb_shown = true
%% 			 ,
%% 			  set_type = Type,
%% 			  curr_status = Status
			 };
pin_info(4) ->
	#pin_info{name = "5V <small>DC Power</small>"
  			};
pin_info(5) ->
%% 	{Type, Status} = gpio_proc:get_pin_status(3),
	#pin_info{
			  gpio_id = 3,
			  name = "GPIO 03 <small>(SCL1, I2C)</small>",
			  rg_enabled = true,
			  cb_shown = true
%% ,
%% 			  set_type = Type,
%% 			  curr_status = Status
			 };
pin_info(6) ->
	#pin_info{name = "<small>Ground</small>"
			  };
pin_info(7) ->
%% 	{Type, Status} = gpio_proc:get_pin_status(4),
	#pin_info{
			  name = "GPIO 04<small>(GPIO_GCLK)</small>",
			  gpio_id = 4,
			  rg_enabled = true,
			  cb_shown = true
%% ,
%% 			  set_type = Type,
%% 			  curr_status = Status
			  };
pin_info(8) ->
%% 	{Type, Status} = gpio_proc:get_pin_status(14),
	#pin_info{
			  gpio_id = 14,
			  name = "GPIO 14<small>TXD0</small>",
			  rg_enabled = true,
			  cb_shown = true
%% 			 ,
%% 			  set_type = Type,
%% 			  curr_status = Status
			  };
pin_info(9) ->
	#pin_info{name = "<small>Ground</small>"
			 
			 };
pin_info(10) ->
%% 	{Type, Status} = gpio_proc:get_pin_status(15),
	#pin_info{name = "GPIO 15 <small>RXDO</small>",
			  gpio_id = 15,
			  rg_enabled = true,
			  cb_shown = true
%% ,
%% 			  set_type = Type,
%% 			  curr_status = Status
			 };
pin_info(11) ->
%% 	{Type, Status} = gpio_proc:get_pin_status(17),
	#pin_info{name = "GPIO 17 <small>GPIO_GEN0</small>",
			  gpio_id = 17,
			  rg_enabled = true,
			  cb_shown = true
%% 			 ,
%% 			  set_type = Type,
%% 			  curr_status = Status
			 };
pin_info(12) ->
%% 	{Type, Status} = gpio_proc:get_pin_status(18),
	#pin_info{name = "GPIO 18 <small>GPIO_GEN1</small>",
			  gpio_id = 18, 
			  rg_enabled = true,
			  cb_shown = true
%% 			 ,
%% 			  set_type = Type,
%% 			  curr_status = Status
			 };
pin_info(13) ->
%% 	{Type, Status} = gpio_proc:get_pin_status(27),
	#pin_info{name ="GPIO 27 <small>GPIO GEN2</small>",
			  gpio_id = 27,
			  rg_enabled =true,
			  cb_shown = true
%% 			 ,
%% 			  set_type = Type,
%% 			  curr_status = Status
			 };
pin_info(14) ->
	#pin_info{name = "<small>Ground</small>"
			 
			 };
pin_info(15) ->
%% 	{Type, Status} = gpio_proc:get_pin_status(22),
	#pin_info{name = "GPIO 22 <small>GPIO GEN3</small>",
			  gpio_id = 22, 
			  rg_enabled = true,
			  cb_shown = true
%% 			 ,
%% 			  set_type = Type,
%% 			  curr_status = Status
			 };
pin_info(16) ->
%% 	{Type, Status} = gpio_proc:get_pin_status(23),
	#pin_info{name = "GPIO 23 <small>GPIO GEN4</small>",
			  gpio_id = 23,
			  rg_enabled = true,
			  cb_shown = true
%% 			 ,
%% 			  set_type = Type,
%% 			  curr_status = Status
			 };
pin_info(17) ->
	#pin_info{
			  name = "3.3V <small>DC Power</small>"
			  };
pin_info(18) ->
%% 	{Type, Status} = gpio_proc:get_pin_status(24),
	#pin_info{
			  name = "GPIO 24 <small>GPIO GEN5</small>",
			  gpio_id = 24,
			  rg_enabled = true,
			  cb_shown = true
%% 			 ,
%% 			  set_type = Type,
%% 			  curr_status = Status
			  };
pin_info(19) ->
%% 	{Type, Status} = gpio_proc:get_pin_status(10),
	#pin_info{name = "GPIO 10 <smll>SPI_MOZI</small>",
			  gpio_id = 10,
			  rg_enabled = true,
			  cb_shown = true
%% 			 ,
%% 			  set_type = Type,
%% 			  curr_status = Status
			 };
pin_info(20) ->
	#pin_info{name = "<small>Ground</small>"};
pin_info(21) ->
%% 	{Type, Status} = gpio_proc:get_pin_status(9),
	#pin_info{
			  name = "GPIO 09 <small>SPI_MISO</small>",
			  gpio_id = 9,
			  rg_enabled =true,
			  cb_shown = true
%% ,
%% 			  set_type = Type,
%% 			  curr_status = Status
			  };
pin_info(22) ->
%% 	{Type, Status} = gpio_proc:get_pin_status(25),
	#pin_info{
			  name = "GPIO 25 <small>GPIO_GEN6</small>",
			  gpio_id = 25,
			  rg_enabled = true,
			  cb_shown = true
%% 			 ,
%% 			  set_type = Type,
%% 			  curr_status = Status
			 };
pin_info(23) ->
%% 	{Type, Status} = gpio_proc:get_pin_status(11),
	#pin_info{name = "GPIO 11 <small>SPI_CLK</small>",
			  gpio_id = 11,
			  rg_enabled = true,
			  cb_shown = true
%% ,
%% 			  set_type = Type,
%% 			  curr_status = Status
			 };
pin_info(24) ->
%% 	{Type, Status} = gpio_proc:get_pin_status(8),
	#pin_info{
			  name = "GPIO 08 <small>SPI_CE0_N</small>",
			  gpio_id = 8,
			  rg_enabled = true,
			  cb_shown = true
%% 			 ,
%% 			  set_type = Type,
%% 			  curr_status = Status
			 };
pin_info(25) ->
	#pin_info{name = "<small>Ground</small>"};
pin_info(26) ->
%% 	{Type, Status} = gpio_proc:get_pin_status(7),
	#pin_info{name = "GPIO 07 <small>SPI_CE1_N</small>",
			  gpio_id = 7,
			  rg_enabled = true,
			  cb_shown = true
%% ,
%% 			  set_type = Type,
%% 			  curr_status = Status
			 };
pin_info(27) ->
	#pin_info{name = "ID_SD <small>I2C ID EEPROM</small>"};
pin_info(28) ->
	#pin_info{name = "ID_SC <small>I2C ID EEPROM</small>"};
pin_info(29) ->
%% 	{Type, Status} = gpio_proc:get_pin_status(5),
	#pin_info{name = "GPIO 05",
			  gpio_id = 5, 
			  rg_enabled = true,
			  cb_shown = true
%% 			 ,
%% 			  set_type = Type,
%% 			  curr_status = Status
			 };
pin_info(30) ->
	#pin_info{name = "<small>Ground</small>"};
pin_info(31) ->
%% 	{Type, Status} = gpio_proc:get_pin_status(6),
	#pin_info{name = "GPIO 06",
			  gpio_id = 6,
			  rg_enabled = true,
			  cb_shown = true
%% ,
%% 			  set_type = Type,
%% 			  curr_status = Status
			  
			  };
pin_info(32) ->
%% 	{Type, Status} = gpio_proc:get_pin_status(12),
	#pin_info{name = "GPIO 12",
			  gpio_id = 12,
			  rg_enabled = true,
			  cb_shown = true
%% 			 ,
%% 			  set_type = Type,
%% 			  curr_status = Status
			  
			  };
pin_info(33) ->
%% 	{Type, Status} = gpio_proc:get_pin_status(13),
	#pin_info{name = "GPIO 13",
			  gpio_id = 13,
			  rg_enabled = true,
			  cb_shown = true
%% 			 ,
%% 				set_type = Type,
%% 			  curr_status = Status
			  
			  };			  			  
pin_info(34) ->
	#pin_info{name = "<small>Ground</small>"};	
pin_info(35) ->
%% 	{Type, Status} = gpio_proc:get_pin_status(19),
	#pin_info{name = "GPIO 19",
			  gpio_id = 19,
			  rg_enabled = true,
			  cb_shown = true
%% 			 ,
%% 			  set_type = Type,
%% 			  curr_status = Status
			  };
pin_info(36) ->
%% 	{Type, Status} = gpio_proc:get_pin_status(16),
	#pin_info{name = "GPIO 16",
			  gpio_id = 16,
			  rg_enabled = true
%% 			 ,
%% 			  cb_shown = true,set_type = Type,
%% 			  curr_status = Status
			  };
pin_info(37) ->
%% 	{Type, Status} = gpio_proc:get_pin_status(26),
	#pin_info{name = "GPIO 26",
			  gpio_id = 26,
			  rg_enabled = true,
			  cb_shown = true
%% 			 ,
%% 			  set_type = Type,
%% 			  curr_status = Status
			  };
pin_info(38) ->
%% 	{Type, Status} = gpio_proc:get_pin_status(20),
	#pin_info{name = "GPIO 20",
			  gpio_id = 20,
			  rg_enabled = true,
			  cb_shown = true
%% 			 ,
%% 			  set_type = Type,
%% 			  curr_status = Status
			  };
pin_info(39) ->
	#pin_info{name = "<small>Ground</small>"};	
pin_info(40) ->
%% 	{Type, Status} = gpio_proc:get_pin_status(21),
	#pin_info{name = "GPIO 21",
			  gpio_id = 21,
			  rg_enabled = true,
			  cb_shown = true
%% ,
%% 			  set_type = Type,
%% 			  curr_status = Status
			  };

pin_info(_) ->
	[].
				   
-define(LIST_OF_NAMES_LEFT_HAND_SIDE,
		["3.3V <small>DC Power</small>", "02 <small>(SDA1, I2C)</small>", "03 <small>(SCL1, I2C)</small>", "04<small>(GPIO_GCLK)</small>", "Ground", 
		 "17", 			"27", "22", "3.3V", "10", 
		 "09", "11", "Ground", "ID_SD", "05", 
		 "06", "13", "19", "26", "Ground"]
	   ).

row(Pin) ->
	#pin_info{name = PinName,
			  gpio_id = GPIOPin,
%% 			  radio_postback = RadioPostback,
%% 			  toggle_postback = RogglePostback,
			  rg_enabled = RGEnabled,
			  cb_shown = CBShown,
			  set_type 		= Type,
			  status = Status
			 } =  
		pin_info(Pin),
	set_pin_status(GPIOPin,Status),
[		#tablecell{ text = io_lib:format("~2..0B", [Pin])},
		#tablecell{body = PinName},
		#tablecell{body =
						case RGEnabled of
							true ->
								[
									#dropdown{id = "drop_down" ++ integer_to_list(GPIOPin),
												postback = {changed_drop, GPIOPin},
												options =[
														#option{text =   "not set", 
															   selected = if (Type == none) -> true;
																			 true -> undefined
																		  end
															  },
														#option{text =  "output", 
															   selected = if Type == output -> true;
																			 true -> undefined
																		  end
															  },
														#option{text =   "input", 
															   selected = if Type == input -> true;
																			 true -> undefined
																		  end
															  }	 
													]
											}
								];
							_ ->
								[]
						end
				  },
		#tablecell{body =
				case CBShown of
					true ->
						[
							#toggle_box{
										id = "toggle" ++ integer_to_list(GPIOPin),
										html_id = "toggle" ++ integer_to_list(GPIOPin),
%% 										value = " ",
										type = "checkbox", 
										class = "toggle-off",
										data_fields = [{"toggle", "toggle"}]
									   ,
                                        
										postback = {toggled, GPIOPin}
%% 									   ,
%% 										disabled = true,
%% 										delegate = ?MODULE
										}																	  
						];
					_ ->
						[]
				end
				},
		#tablecell{
					body = [
							
							]
				  }
	].





set_pin_status(PinNum, Status) ->
	ok.