%% @author stuart
%% @doc @todo Add description to gpio_proc.


-module(gpio_proc).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/0]).


-export([init/1,
		 set_type/2,
		 set_status/2,
		 save_cfg/0
		]).

-export([state/0,
		 dict/0,
		 get_pin_status/1]).

-define(GPIO_ON, 	1).
-define(GPIO_OFF,	0).

-define(CFG_FILE,  "gpio.conf").

-define(TYPE_NOT_SET, none).
-define(TYPE_OUTPUT, output).
-define(TYPE_INPUT,  input).


-define(SAVE_CFG_TIMEOUT, 5 * 60 * 1000).

-record(pin, {
			  pin_num,
			  type = none,
			  status = ?GPIO_OFF,
			  comet_pool_name
			  }).

-record(state, {
				ppid, 
				tref,
				cfg
  				}).
get_pin_status(Pin) ->
	?MODULE ! {{get_pin_status, Pin}, Ref = make_ref(), self()},
	receive 
		{Ref, Reply} ->
			Reply
	after 5000 -> timeout
	end.

set_type(Pin, Type) ->
	?MODULE ! { {set_type, Pin, Type},Ref = make_ref(), self()},
	receive
		{Ref, Reply} -> Reply
		after 5000 -> timeout
	end.

set_status(Pin, Status) ->
	?MODULE ! { {set_status, Pin, Status},Ref = make_ref(), self()},
	receive
		{Ref, Reply} -> Reply
		after 5000 -> timeout
	end.

dict() ->
	?MODULE ! { dict,Ref = make_ref(), self()},
	receive
		{Ref, Reply} -> Reply
		after 5000 -> timeout
	end.

save_cfg() ->
	?MODULE ! save_cfg,
	sent.

state() ->
	?MODULE ! { state,Ref = make_ref(), self()},
	receive
		{Ref, Reply} -> Reply
		after 5000 -> timeout
	end.


%% ====================================================================
%% Internal functions
%% ====================================================================

start_link() ->
	proc_lib:start_link(?MODULE, init, [self()]).

init(PPid) ->
	Cfg = case file:consult(?CFG_FILE) of
		{ok, Config} ->
			Config;
		{error, _} ->
				create_empty_config()
	end,
	proc_lib:init_ack(PPid, {ok, self()}),
	register(?MODULE, self()),
	process_flag(trap_exit, true),
	
	NewCfg =try  do_startup_of_pins(Cfg,[])
	catch
		_Exit:_Reson ->
			error_logger:warning_msg("You are probaby starting this up on dev machine NOT PI if not this error is important:~n~p ~p ~p:~p",
									 [?MODULE, ?LINE, _Exit, _Reson]),
			Cfg
	end,
	{ok, Tref} = timer:send_after(?SAVE_CFG_TIMEOUT, save_cfg),
	loop(#state{cfg = NewCfg,
					 tref = Tref,
					 ppid = PPid
					}).

loop(State) ->
	NS = receive 
		Msg ->
			process_msg(Msg, State) 
	end,
	loop(NS).

process_msg(save_cfg, State = #state{cfg = Cfg}) ->
	save_config(Cfg),
	State;

process_msg({{get_pin_status, PinNum}, Ref, From}, State = #state{cfg = Cfg}) ->
	case lists:keysearch(PinNum, 2, Cfg) of
		{value, #pin{status = Status,
						  type = Type
						 }} ->
			From ! {Ref, {Type, Status}};
		_ ->
			From ! {Ref, invalid_pin}
	end,
	State;

process_msg({{set_type, PinNum, Type}, Ref, From}, State = #state{cfg = Cfg} ) ->
	case lists:keytake(PinNum, 2, Cfg) of
		{value, Pin = #pin{pin_num = PinNum,
						   status = Status
						  }, OtherCfg} ->
%% 			send_to_web({pin_changed,PinNum, Status, Type}),  
			case get(PinNum) of
				undefined ->
					ok;
				_OldPid ->
					erase(PinNum),
					gpio:stop(_OldPid),
					%% Give a little time for the pin to stop before starting a new one.
					receive 50 -> void end
			end,
			case Type of
				input ->
					{ok, Pid} = gpio:start_link(PinNum, input),
 					Status = gpio:read(Pid), 
					gpio:register_int(Pid),
					gpio:set_int(Pid, both),
					put(PinNum, Pid),
					NewPin = Pin#pin{type = ?TYPE_INPUT},
					From ! {Ref, {ok, Status}},
					State#state{ cfg = [NewPin|OtherCfg]};
				output ->
					{ok, Pid} = gpio:start_link(PinNum, output),
					put(PinNum, Pid),
					From ! {Ref, ok},
					State#state{cfg = [Pin#pin{type = ?TYPE_OUTPUT}|OtherCfg]};
				_ ->
					From ! {Ref, ok},
					State#state{cfg = [Pin#pin{type = ?TYPE_NOT_SET}|OtherCfg]}
			end
	end;
	
process_msg({{set_status, PinNum, Status}, Ref, From}, State = #state{cfg = Cfg}) ->
	
	case get(PinNum) of
		Pid when is_pid(Pid) ->
			case lists:keytake(PinNum, 2, Cfg) of
				{value, Pin = #pin{type = Type = ?TYPE_OUTPUT}, RestCfg} ->
%% 					send_to_web({pin_changed,PinNum, Status, Type}),
					gpio:write(Pid, Status),
					From ! {Ref, ok},
					State#state{cfg = [Pin#pin{status = Status}|RestCfg]};
				_ ->
					From ! {Ref, not_output_pin},
					State
			end;
		_ ->
			From ! {Ref, not_started},
			State
	end;

process_msg(_IMsg = {gpio_interrupt, PinNum, FailingRising}, 
	State
  	) ->
%% 	error_logger:info_msg("GPIO INPUT:~p", [IMsg]),
	Status = case FailingRising of
				 
		rising -> 0;
		_ -> 1
	end,

	Msg = {input_changed, PinNum, Status},

	try send_to_web(Msg) 
	catch _E:_A -> error_logger:info_msg("E~p A~p", [_E, _A])
	end,

	State;


process_msg({dict, Ref, From}, State) ->
	From ! {Ref, get()},
	State;

process_msg({state, Ref, From}, State) ->
	From !{Ref, State},
	State;

process_msg({'EXIT', PPid, _},  #state{ ppid =PPid}) ->
	exit(shutdown);
process_msg(_ExitRec = {'EXIT', _,_}, State) ->
	
	State;
process_msg(_Msg, State) ->
	State.
	

do_startup_of_pins([], NewCfg) ->
	NewCfg;
do_startup_of_pins([Rec = #pin{type = none}|Rest], Cfg) ->
	do_startup_of_pins(Rest, [Rec|Cfg]);
do_startup_of_pins([Rec = #pin{
						 pin_num = PinNum,
						 type = ?TYPE_INPUT
						} | Rest], Cfg) ->
	{ok, Pid} = gpio:start_link(PinNum, input),
	Status = gpio:read(Pid), 
	gpio:register_int(Pid),
	gpio:set_int(Pid, both),
	put(PinNum, Pid),
	do_startup_of_pins(Rest, [Rec#pin{status = Status}|Cfg]);
	
do_startup_of_pins([Rec = #pin{pin_num = PinNum,
						 type = ?TYPE_OUTPUT,
						 status = GPIOStatus
						}|Rest], Cfg) ->
	{ok, Pid} = gpio:start_link(PinNum, output),
	gpio:write(Pid, GPIOStatus),
	put(PinNum, Pid),
	do_startup_of_pins(Rest, [Rec|Cfg]); 
do_startup_of_pins([_Invalid|Rest], Cfg) ->
	do_startup_of_pins(Rest, Cfg).
					


create_empty_config() ->
	Cfg = [#pin{pin_num = X} || X <- lists:seq(1, 40)],
	IOList = [io_lib:format("~p.~n", [Y]) || Y <- Cfg],
	file:write_file(?CFG_FILE, iolist_to_binary(IOList)),
	Cfg.
	
save_config(Cfg) ->
	IOList = [io_lib:format("~p.~n", [Y]) || Y <- Cfg],
	file:write_file(?CFG_FILE, iolist_to_binary(IOList)),
	Cfg.
	

send_to_web(Msg) ->
  %Fake a context
  case catch wf_context:data() of
	  [] ->
  		void;
	  _ ->
		  wf_context:init_context(undefined)
  end,
%%   error_logger:info_msg("Msg:~p", [Msg]),
  %%Send Msg
  wf:send_global(input_pins_comet, Msg).