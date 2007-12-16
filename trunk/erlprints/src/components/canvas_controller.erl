-module(canvas_controller).

-export([index/1]).

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
    Action = render:profile_action(To, length(ToPrints)),
    Profile = render:profile_box(To, ToPrints),
    facebook:profile_setFBML(To, Profile, Action, [], Key),
    
    % publish story
    FeedTitle = render:feed_title(From, To),
    FeedBody = render:feed_body(To),
    facebook:feed_publishActionOfUser(FeedTitle, FeedBody, [], "1", Key),

    % send notification email
    case facebook:notifications_send(To, render:step_mail(), [], Key) of
	{notifications_send_reply, [], [Url]} ->
	    {data, {redirect, Url}};
	_ ->
	    {data, {show_prints, From, To, ToPrints}}
    end.

epoch_time() ->
    {M, S, _} = now(),
    M*1000000+S.

