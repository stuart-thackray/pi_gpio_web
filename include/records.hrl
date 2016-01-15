-define(E_BASE(Module),
        %% attribute is_element should only ever be the atom `is_element` but
        %% because Nitrogen copies the base element when it needs to, it's
        %% possible it might accidentally copy the wrong kind of element that
        %% it may think is an element but its not. In such an event, we allow
        %% any() here, so that it passes the dialyzer warning, and will throw
        %% an error appropriately during runtime.
        %%
        %% If we kept it as just `is_element`, that may be just fine, but then
        %% users would probably have to use dialyzer to debug the "is not an
        %% element" error
        is_element=is_element   :: is_element | any(), 
        module=Module           :: atom(),
        id                      :: id(),
        anchor                  :: id(),
        actions                 :: actions(),
        show_if=true            :: boolean(),
        class=""                :: class() | [class()],
        style=""                :: text(),
        html_id=""              :: id(),
        title=""                :: undefined | text(),
        data_fields=[]          :: data_fields()        
    ).


-record(toggle_box, {?E_BASE(element_togglebox),
        text=""                 :: text(),
        body=""                 :: body(),
        html_encode=true        :: html_encode(),
%% 		value,
        next                    :: id(),
        click                   :: actions(),
        enter_clicks=[]         :: [id()],
        postback                :: term(),
        disabled=false          :: boolean(),
        handle_invalid=false    :: boolean(),
        on_invalid              :: undefined | actions(),
        delegate                :: module(),
					 type = text
    }).

%% -record(toggle_box, {?E_BASE(element_togglebox),
%%         text=""                 :: text(),
%%         maxlength=""            :: integer() | string(),
%%         size=""                 :: integer() | string(),
%%         placeholder=""          :: text(),
%%         html_encode=true        :: html_encode(),
%%         disabled=false          :: boolean(),
%%         readonly=false          :: boolean(),
%%         next                    :: id(),
%%         postback                :: term(),
%%         handle_invalid=false    :: boolean(),
%%         on_invalid              :: undefined | actions(),
%%         delegate                :: module(),
%%         html_name               :: html_name(),
%%         type=text               :: string() | atom()
%%     }).