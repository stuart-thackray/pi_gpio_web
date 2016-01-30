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
            wf:wire(Anchor, #event {
                type=change,
                validation_group=ID,				
                postback=Postback,
				handle_invalid=Record#toggle_box.handle_invalid,
				on_invalid=Record#toggle_box.on_invalid,
                delegate=Record#toggle_box.delegate
            })
    end,
	
	case Record#toggle_box.click of
		undefined -> ignore;
		ClickActions -> wf:wire(Anchor, #event { type=click, actions=ClickActions })
	end,
    Text = wf:html_encode(Record#toggle_box.text, Record#toggle_box.html_encode), 
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
