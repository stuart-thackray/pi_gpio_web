%% @author stuart
%% @doc @todo Add description to gpio_proc.


-module(eft_proc).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/1,
		stop/0]).


-export([init/2
		]).

-export([state/0
		 ]).

-define(GPIO_ON, 	1).
-define(GPIO_OFF,	0).



-record(state, {
				ppid, 
				pin_list = [21,20,16],
				timeout 
  				}).
%% 
%% -record(state, {
%% 				ppid, 
%% 				pin_list = [13,26],
%% 				timeout 
%%   				}).
stop() ->
	?MODULE ! stop,
	ok.	

state() ->
	?MODULE ! { state,Ref = make_ref(), self()},
	receive
		{Ref, Reply} -> Reply
		after 5000 -> timeout
	end.


%% ====================================================================
%% Internal functions
%% ====================================================================

start_link(Timeout) ->
	proc_lib:start_link(?MODULE, init, [self(), Timeout]).

init(PPid,Timeout) ->
%% 	Cfg = case file:consult(?CFG_FILE) of
%% 		{ok, Config} ->
%% 			Config;
%% 		{error, _} ->
%% 				create_empty_config()
%% 	end,
	proc_lib:init_ack(PPid, {ok, self()}),
	register(?MODULE, self()),
	process_flag(trap_exit, true),
	
%% 	NewCfg =try  do_startup_of_pins(Cfg,[])
%% 	catch
%% 		_Exit:_Reson ->
%% 			error_logger:warning_msg("You are probaby starting this up on dev machine NOT PI if not this error is important:~n~p ~p ~p:~p",
%% 									 [?MODULE, ?LINE, _Exit, _Reson]),
%% 			Cfg
%% 	end,
	loop(#state{timeout = Timeout,
					 ppid = PPid
					}).


loop(State = #state{timeout = Timeout,
					pin_list = [E|[T]] 
				   })
  ->
%% 	error_logger:info_msg("~p ~p ~p", [E, H, T]),
	NS = receive
			 Msg ->
				 process_msg(Msg, State)
		after Timeout ->
			gpio_proc:set_status(E, 1),
			gpio_proc:set_status(T, 0),
			State#state{pin_list = [T,E]}
	end,
	loop(NS);


loop(State) ->
	NS = receive 
		Msg ->
			process_msg(Msg, State) 
	end,
	loop(NS).

process_msg(stop, State) ->
	exit(shutdown);

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
	

