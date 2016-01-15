
-module (element_togglebox).
-include_lib ("nitrogen_core/include/wf.hrl").
-include_lib ("../include/records.hrl").
-export([
    reflect/0,
    render_element/1
]).

reflect() -> record_info(fields, toggle_box).


-spec render_element(#toggle_box{}) -> body().
render_element(Record) ->
    ID = Record#toggle_box.id,
    Anchor = Record#toggle_box.anchor,
    case Record#toggle_box.postback of
        undefined -> ignore;
        Postback ->
%% 			error_logger:info_msg("PostBack:~p~nAnchor~p~n~n", [Postback, Anchor]),
            wf:wire(Anchor, #event {
                type=change,
                validation_group=ID,
                postback=Postback,
                handle_invalid=Record#toggle_box.handle_invalid,
                on_invalid=Record#toggle_box.on_invalid,
                delegate=Record#toggle_box.delegate
            })
    end,
 



    Text = wf:html_encode(Record#toggle_box.text, Record#toggle_box.html_encode), 
%%     Image = format_image(Record#toggle_box.image),
%%     Body = case {Image,Record#toggle_box.body} of
%%         {[], []} -> [];
%%         {I, B} -> [I, B]
%%     end,
	Body = [],
    UniversalAttributes = [
        {id, Record#toggle_box.html_id},
        {class, [toggle_box, Record#toggle_box.class]},
        {title, Record#toggle_box.title},
        {style, Record#toggle_box.style},
        {data_fields, Record#toggle_box.data_fields},
        ?WF_IF(Record#toggle_box.disabled, disabled)
    ],

    case Body of
        [] -> 
            wf_tags:emit_tag(input, [
                {type, Record#toggle_box.type},
                {value, Text}
                | UniversalAttributes
            ]);
        _ ->
            wf_tags:emit_tag(toggle_box, [Body, Text], UniversalAttributes)
    end.

wire_enter_clicks(Targetid, Triggerids) when is_list(Triggerids) ->
    [wire_enter_click(Targetid, Triggerid) || Triggerid <- Triggerids].

wire_enter_click(Targetid, Triggerid) ->
    wf:wire(Triggerid, #event{type=enterkey, actions=#click{target=Targetid}}).

format_image(undefined) -> [];
format_image([]) -> [];
format_image(Path) -> [#image{image=Path}," "].
%% render_element(Record) -> 
%%     ID = Record#toggle_box.id,
%%     Anchor = Record#toggle_box.anchor,
%% %%     Delegate = Record#toggle_box.delegate,
%% %%     Postback = Record#toggle_box.postback,
%%     Disabled = Record#toggle_box.disabled,
%%     Readonly = Record#toggle_box.readonly,
%% %%     HandleInvalid = Record#toggle_box.handle_invalid,
%% %%     OnInvalid = Record#toggle_box.on_invalid,
%% 
%%     action_event:maybe_wire_next(Anchor, Record#toggle_box.next),
%%     case Record#toggle_box.postback of
%%         undefined -> ignore;
%%         Postback ->
%% 			error_logger:info_msg("Anchor", [Anchor]),
%%             wf:wire(Anchor, #event {
%%                 type=click,
%%                 validation_group=ID,
%%                 postback=Postback,
%%                 handle_invalid=Record#toggle_box.handle_invalid,
%%                 on_invalid=Record#toggle_box.on_invalid,
%%                 delegate=Record#toggle_box.delegate
%%             })
%%     end,
%%     Value = wf:html_encode(Record#toggle_box.text, Record#toggle_box.html_encode),
%%     Placeholder  = wf:html_encode(Record#toggle_box.placeholder, true),
%% 
%%     Attributes = [
%%         {id, Record#toggle_box.html_id},
%%         {type, Record#toggle_box.type}, 
%%         {class, [input, Record#toggle_box.class]},
%%         {title, Record#toggle_box.title},
%%         {style, Record#toggle_box.style},
%%         {name, Record#toggle_box.html_name},
%%         {placeholder, Placeholder},
%%         ?WF_IF(Disabled,disabled,undefined),
%%         ?WF_IF(Readonly,readonly,undefined),
%%         {value, Value},
%%         {data_fields, Record#toggle_box.data_fields}
%%     ],
%% 
%%     wf_tags:emit_tag(input, Attributes).

%% wire_postback(_, _, _, _, _, undefined) ->
%%     do_nothing;
%% wire_postback(Anchor, ID, HandleInvalid, OnInvalid, Delegate, Postback) ->
%%     wf:wire(Anchor, #event {
%%         type=enterkey,
%%         postback=Postback,
%%         validation_group=ID,
%%         handle_invalid=HandleInvalid,
%%         on_invalid=OnInvalid,
%%         delegate=Delegate
%%     }).
