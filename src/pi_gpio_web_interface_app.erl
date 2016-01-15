-module(pi_gpio_web_interface_app).
-behaviour(application).
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
	application:start(sasl),
	application:start(os_mon),
    pi_gpio_web_interface_sup:start_link(),
 	gpio_proc:start_link().

stop(_State) ->
    ok.
