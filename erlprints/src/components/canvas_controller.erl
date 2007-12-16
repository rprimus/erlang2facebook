-module(canvas_controller).

-export([index/1, render_prints/2]).

index(A) ->
    case yaws_arg:method(A) of
	'POST' ->
	    case yaws_api:parse_post(A) of
		Params when length(Params) > 1 ->
		    case facebook:verifyParams(Params) of
			{ok, Parsed} ->
			    case lists:keysearch("user", 1, Parsed) of
				{value, _} ->
				    show_index(A, Parsed);
				_ ->
				    % require login
				    {data, {redirect,
					    facebook:login_path("/", true)}}
			    end;
			_ ->
			    % not in canvas
			    {response, [{redirect, facebook:app_path()}]}
		    end;
		[] ->
		    % not in canvas
		    {response, [{redirect, facebook:app_path()}]}
	    end;
	'GET' ->
	    % canvas page never GETs your page - always a POST
	    % so, user is not in a canvas page
	    {response, [{redirect, facebook:app_path()}]}
    end.

show_index(A, Parsed) ->
    {value, {_, User}} = lists:keysearch("user", 1, Parsed),
    {value, {_, Key}} = lists:keysearch("session_key", 1, Parsed),
    case yaws_api:postvar(A, "to") of
	{ok, ToId} ->
	    do_step(User, Key, ToId);
	_ ->
	    case yaws_api:queryvar(A, "to") of
		{ok, ToId} ->
		    Prints = print:find({target, '=', ToId});
		_ ->
		    ToId = User,
		    Prints = print:find({target, '=', User})
	    end,
	    {data, {show_prints, User, ToId, Prints}}
    end.

do_step(From, Key, To) ->
    print:save(print:new_with([{stepper, From},
			       {target, To},
			       {time, epoch_time()}])),

    % set profile fbml
    ToPrints = print:find({target, '=', To}, [{order_by, [{time, desc}]}]),
    Fbml = [render_profile_action(To, length(ToPrints)),
	    render_profile_box(To, ToPrints)],
    facebook:profile_setFBML(To, Fbml, [], [], Key),
    
    % publish story
    FeedTitle = ["<fb:userlink uid=\"", From, "\" shownetwork=\"false\"/>",
		 " stepped on <fb:name uid=\"", To, "\"/>."],
    FeedBody = ["Check out <a href=\"", facebook:app_path(["?to=", To]), "\">",
		"<fb:name uid=\"", To,
		"\" firstnameonly=\"true\" possessive=\"true\"/>",
		"Erlprints</a>."],
    facebook:feed_publishActionOfUser(FeedTitle, FeedBody, [], "1", Key),

    % send notification email
    case facebook:notifications_send(To, render_step_mail(), [], Key) of
	{notifications_send_reply, [], [Url]} ->
	    {data, {redirect, Url}};
	_ ->
	    {data, {show_prints, From, To, ToPrints}}
    end.

render_profile_action(Id, Count) ->
    ["<fb:profile-action url=\"", facebook:app_path(["?to=", Id]), "\">",
     "<fb:name uid=\"", Id, "\" firstnameonly=\"true\" captialize=\"true\"/>",
     " has been stepped on ", integer_to_list(Count), " times.",
     "</fb:profile-action>"].

render_profile_box(To, Prints) ->
    [render_prints(Prints, 5),
     "<fb:wide>", render_prints(lists:sublist(Prints, 5), 5), "</fb:wide>",
     "<div style=\"clear:both;\">", render_step_link(To), "</div>"].

render_prints([Print|Rest], N) when N > 0 ->
    ["<div style=\"clear:both; padding:3px;\">",
     "<fb:profile-pic style=\"float:left;\" uid=\"",
     print:stepper(Print), "\" size=\"square\"/>",
     "<fb:name uid=\"", print:stepper(Print), "\" capitalize=\"true\"/>",
     " stepped on <fb:name uid=\"", print:target(Print), "\"/>",
     " at <fb:time t=\"", integer_to_list(print:time(Print)), "\"/>.",
     "<br/>", render_step_link(print:stepper(Print)), "<br/>",
     "</div>"
     | render_prints(Rest, N-1)];
render_prints(_, _) ->
    [].

render_step_link(Id) ->
    ["<a href=\"", facebook:app_path(["?to=", Id]), "\">",
     "Step on <fb:name uid=\"", Id, "\" firstnameonly=\"true\"/></a>"].

render_step_mail() ->
    ["<fb:notif-subject>You have been stepped on...</fb:notif-subject>",
     "<a href=\"", facebook:app_path(), "\">Check out your Erlprints!</a>"].

epoch_time() ->
    {M, S, _} = now(),
    M*1000000+S.

