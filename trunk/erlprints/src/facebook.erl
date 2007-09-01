%% Copyright (c) 2007 Bryan Fink

%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:

%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.

%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.

%% file: facebook.erl
%% description: Implementation of the public Facebook API
%%
%% This module makes an imediate departure from other popular Facebook API
%% implementations: it is not object-oriented.  In general, most functions in
%% this module have a /N call and a /N-1 call.  Use the /N call when you want
%% to specify the session key that will be used.  Use the /N-1 call when you
%% want to use the default infinite key.
%%
%% The default infinite key is usually the session key for the developer, once
%% she has logged in with the "Keep me logged into This App" box checked.
%%
%% To use this module, first open your copy of facebook_app.hrl and set the
%% parameters to your specific application.  (API_KEY and SECRET will be
%% required, the other three are helpful).
%%
%% The first method you're likely to find useful is verifyParams/1.  Pass to
%% this method the list of POST parameters that the Facebook servers send your
%% app.  verifyParams/1 will find all parameters that should affect the
%% signature of the call, verify the signature, and return those params without
%% their "fb_sig_" prefix.
%%
%% The only other methods that do not send a request to the Facebook REST
%% server are those that end in "_path".  These are utility functions to help
%% you not to have to remember where everything is on Facebook's side.
%%
%% All other functions will send an HTTP POST to Facebook's REST server.  Their
%% return values are a simplified Erlang representation of XML.  The basic
%% structure is:
%%    xml_entity() = {tag(), [attribute()], [xml_entity()]}
%%           tag() = atom representation of the XML entity's name
%%     attribute() = {name(), value()}
%%          name() = string representation of the attribute's name
%%         value() = string representation of the attribute's value
%%
%% So, for example, if a Facebook REST call returned
%%     <facebook_reply>
%%       <user>
%%         <name nick="yes">The Dude</name>
%%         <email>dude@the.com</email>
%%       </user>
%%     </facebook_reply>
%%
%% The return from the function that accepted that reply would be
%%     {facebook_reply, [],
%%      [{user, [],
%%       [{name, [{"nick","yes"}], ["The Dude"]},
%%        {email, [], ["dude@the.com"]}]}]}

-module(facebook).

-author('bryanfink A alum D mit D edu').

-export([app_path/0, app_path/1,
	 login_path/0, login_path/1, login_path/2,
	 add_path/0, profile_path/1]).

-export([call/2]).

-export([auth_getSession/1]).

-export([events_get/5, events_get/6,
	 events_getMembers/1, events_getMembers/2]).

-export([fql_query/1, fql_query/2]).

-export([feed_publishStoryToUser/4, feed_publishStoryToUser/5,
	 feed_publishActionOfUser/4, feed_publishActionOfUser/5]).

-export([friends_areFriends/2, friends_areFriends/3,
	 friends_get/1, friends_get/2,
	 friends_getAppUsers/1, friends_getAppUsers/2]).

-export([groups_get/2, groups_get/3,
	 groups_getMembers/1, groups_getMembers/2]).

-export([notifications_get/1, notifications_get/2,
	 notifications_send/3, notifications_send/4,
	 notifications_sendRequest/5, notifications_sendRequest/6]).

-export([photos_get/3,
	 photos_getAlbums/2,
	 photos_getTags/1]).

-export([users_getInfo/2, users_getInfo/3,
	 users_getLoggedInUser/1, users_getLoggedInUser/2,
	 users_isAppAdded/1, users_isAppAdded/2]).

-export([profile_setFBML/2, profile_setFBML/3,
	 profile_getFBML/1, profile_getFBML/2]).

-export([fbml_refreshImgSrc/1, fbml_refreshImgSrc/2,
	 fbml_refreshRefUrl/1, fbml_refreshRefUrl/2,
	 fbml_setRefHandle/2, fbml_setRefHandle/3]).

%callback utilities
-export([verifyParams/1]).

%% debug export
-export([parseResponse/1, makeParams/2, xmlToSimple/1, generate_sig/2]).

-include_lib("xmerl/include/xmerl.hrl").
-include("facebook_app.hrl").

%% the path to your app's canvas page
app_path() ->
    ["http://apps.facebook.com/", ?CANVAS_PATH, "/"].
app_path(Path) ->
    [app_path(), Path].

%% the path to your app's login page
login_path() ->
    ["http://www.facebook.com/login.php?api_key=", ?API_KEY, "&v=1.0"].
login_path(Path) ->
    [login_path(), "&next=", Path].
login_path(Path, true) ->
    [login_path(Path), "&canvas"];
login_path(Path, _) ->
    login_path(Path).

%% the path to your app's add page
add_path() ->
    ["http://www.facebook.com/add.php?api_key=", ?API_KEY, "&v=1.0"].

%% the path to the user's profile page
profile_path(Id) ->
    ["http://www.facebook.com/profile.php?id=", Id].

%% Call the Facebook server with the given method and args
%% returns {ok, response()} on success where
%%    response() = one of the facebook response types
%% may also return any of the http:request/4 errors
call(Method, Args) ->
    case http:request(post, {"http://api.facebook.com/restserver.php",
			     [],
			     "application/x-www-form-urlencoded",
			     list_to_binary(makeParams(Method, Args))},
		      [{timeout, 60000}], [{sync, true}]) of
	{ok, {{_Httpver, 200, _Reason}, _Headers, Body}} ->
	    parseResponse(Body);
	{ok, {{_Httpver, 200, _Reason}, Body}} ->
	    parseResponse(Body);
	Error ->
	    Error
    end.

%% Append a unique "call_id" to the argument list before calling FB
call_with_id(Method, Args) ->
    call(Method, [{"call_id", call_id()}|Args]).

call_id() ->
    {M, S, Mi} = now(),
    io_lib:format("~4..0b~6..0b~6..0b", [M, S, Mi]).

%% create the params string for the http header
%% returns and iolist
makeParams(Method, Args) ->
    AllArgs = lists:keysort(1, [ {"api_key", ?API_KEY},
				 {"v", "1.0"},
				 {"method", Method}
				 | Args ]),
    Sig = generate_sig(AllArgs, ?SECRET),
    [ "sig=", Sig,
      [ ["&", Key, "=", yaws_api:url_encode(lists:flatten(io_lib:format("~s",[Val])))] || {Key, Val} <- AllArgs ] ].

%% check that the params taken from an HTTP post are valid
%%   (that the md5 sum checks out)
%% returns {ok, [param()]}
%%    param() = {name(), value()} of any fb_sig_ params
%%    name() = the name of the params, without fb_sig_ prepended
%%    value() = the value that param has
verifyParams(Params) ->
    {P, S} = lists:foldl(fun({K, V}, {L, S}) ->
				    case K of
					"fb_sig" ->
					    {L, V};
					[$f,$b,$_,$s,$i,$g,$_|Name] ->
					    {[{Name, V}|L], S};
					_ ->
					    {L, S}
				    end
			    end, {[], []}, Params),
    case generate_sig(lists:keysort(1, P), ?SECRET) of
	S ->
	    {ok, P};
	_ ->
	    error_bad_sig
    end.
 
%% create the md5 signature for an api call
generate_sig(Params, Secret) ->
    ParamList = lists:map(fun({K,undefined}) -> [K, "=", ""];
			     ({K,V}) -> [K, "=", V]
			  end, Params),
    NumSig = binary_to_list(crypto:md5([ParamList,
					Secret])),
    lists:flatten(io_lib:format("~2.16.0b~2.16.0b~2.16.0b~2.16.0b~2.16.0b~2.16.0b~2.16.0b~2.16.0b~2.16.0b~2.16.0b~2.16.0b~2.16.0b~2.16.0b~2.16.0b~2.16.0b~2.16.0b", NumSig)).

%% translate the response of an FB API call into Erlang elements
%% returns {error, {facebook_error, int()}}|{ok, simple_xml()}
parseResponse(Body) ->
    {Xml, _} = xmerl_scan:string(Body),
    case xmlToSimple(Xml) of
	{error_response, _, Children} ->
	    {value, {error_code, _, [Code]}} =
		lists:keysearch(error_code, 1, Children),
	    {error, {facebook_error, list_to_integer(Code)}};
	Other ->
	    {ok, Other}
    end.

%% convert the output of xmerl:scan to a useful Erlang structure
%% returns simple xml, ex.:
%%  <tag1>
%%    <tag2 hello="world">
%%      <tag3>Check</tag3>
%%      <tag4>Alright</tag4>
%%    </tag2>
%%  </tag1>
%% becomes:
%%  {tag1, [],
%%   [{tag2, [{hello, "world"}],
%%     [{tag3, [], ["Check"]},
%%      {tag4, [], ["Alright"]}]}]}
xmlToSimple(#xmlElement{name=Name, attributes=Attrs, content=Content}) ->
    {Name,
     [ {K, V} || #xmlAttribute{name=K, value=V} <- Attrs ],
     lists:foldr(fun(E, L) -> case xmlToSimple(E) of
				  nothing -> L;
				  V -> [V | L]
			      end end,
		 [], Content)};
xmlToSimple(#xmlText{value=Value}) ->
    case lists:all(fun(10) -> true;
		      (13) -> true;
		      (32) -> true;
		      (9) -> true;
		      (_) -> false end,
		   Value) of
	true ->
	    nothing;
	_ -> 
	    Value
    end.

%% Get a session for an auth token
auth_getSession(AuthToken) ->
    call("facebook.auth.getSession", [{"auth_token", AuthToken}]).

%% returns: {events_get_response, Attrs, [{event, ...}|_]
events_get(Uid, Eids, Start, End, Rsvp) ->
    events_get(Uid, Eids, Start, End, Rsvp, ?INFINITE_SESSION).
events_get(Uid, Eids, Start, End, Rsvp, Key) ->
    call_with_id("facebook.events.get", [{"uid", Uid}, {"eids", Eids},
					 {"start_time", Start},
					 {"end_time", End},
					 {"rsvp_status", Rsvp},
					 {"session_key", Key}]).

%% returns: {events_getMembers_response, Attrs,
%%           [{attending, Attrs, [{uid, _, [Uid]}|_]},
%%            {unsure, Attrs, [{uid, _, [Uid]}|_},
%%            {declined, ...}
%%            {not_replied, ...}]}
events_getMembers(Eid) ->
    events_getMembers(Eid, ?INFINITE_SESSION).
events_getMembers(Eid, Key) ->
    call_with_id("facebook.events.getMembers",
		 [{"eid", Eid}, {"session_key", Key}]).

%% returns: {fql_query_response, Attrs,
%%           [{Table, Attrs, [{Column, Attrs, Data}|_]}|_]}
fql_query(Query) ->
    fql_query(Query, ?INFINITE_SESSION).
fql_query(Query, Key) ->
    call_with_id("facebook.fql.query",
		 [{"query", Query}, {"session_key", Key}]).

feed_publish(Method, Title, Body, Images, Priority, Key) ->
    {_Cnt, Images} = 
		  lists:foldl(fun({Img, Lnk}, {Cnt, Lst}) ->
				      {Cnt+1,
				       [{["image_", integer_to_list(Cnt)], Img},
					{["image_", integer_to_list(Cnt),
					  "_link"], Lnk}
					| Lst]}
			      end, {0, []}, Images),
    call_with_id(Method,
		 [{"title", Title}, {"body", Body},
		  {"priority", Priority},
		  {"session_key", Key} |
                  Images]).

%% returns: {feed_publishStoryToUser_response, Attrs, [Result]}
%%    Result == "1" on success, "0" on failure
feed_publishStoryToUser(Title, Body, Images, Priority) ->
    feed_publishStoryToUser(Title, Body, Images, Priority, ?INFINITE_SESSION).
feed_publishStoryToUser(Title, Body, Images, Priority, Key) ->
    feed_publish("facebook.feed.publishStoryToUser",
		 Title, Body, Images, Priority, Key).

%% returns: {feed_publishActionOfUser_response, Attrs, [Result]}
%%    Result == "1" on success, "0" on failure
feed_publishActionOfUser(Title, Body, Images, Priority) ->
    feed_publishActionOfUser(Title, Body, Images, Priority, ?INFINITE_SESSION).
feed_publishActionOfUser(Title, Body, Images, Priority, Key) ->
    feed_publish("facebook.feed.publishActionOfUser",
		 Title, Body, Images, Priority, Key).

-define(MERGE_LIST(List, Sep),
	lists:foldl(fun(I, []) ->
			    [I];
		       (I, L) ->
			    [I, Sep | L]
		    end, [], List)).

%% returns: {friends_areFriends_response, Attrs,
%%           [{friend_info, Attrs, [{uid1, Attrs, [Uid]},
%%                                  {uid2, Attrs, [Uid]},
%%                                  {are_friends, Attrs, [Result]}]}|_]}
%%    Result == "1" when friends, "0" when not
friends_areFriends(Uids1, Uids2) ->
    friends_areFriends(Uids1, Uids2, ?INFINITE_SESSION).
friends_areFriends(Uids1, Uids2, Key) ->
    call_with_id("facebook.friends.areFriends",
		 [{"uids1", ?MERGE_LIST(Uids1, ",")},
		  {"uids2", ?MERGE_LIST(Uids2, ",")},
		  {"session_key", Key}]).

%% returns: {friends_get_response, Attrs,
%%           [{uid, Attrs, [Uid]}]}
friends_get(Uid) ->		 
    friends_get(Uid, ?INFINITE_SESSION).
friends_get(Uid, Key) ->
    call_with_id("facebook.friends.get",
		 [{"uid", Uid}, {"session_key", Key}]).

%% returns: {friends_getAppUsers_response, Attrs,
%%           [{uid, Attrs, [Uid]}]}
friends_getAppUsers(Uid) ->
    friends_getAppUsers(Uid, ?INFINITE_SESSION).
friends_getAppUsers(Uid, Key) ->
    call_with_id("facebook.friends.getAppUsers",
		 [{"uid", Uid}, {"session_key", Key}]).

%% returns {groups_get_response, Attrs, [{group, ...}|_]}
groups_get(Uid, Gids) ->
    groups_get(Uid, Gids, ?INFINITE_SESSION).
groups_get(Uid, [], Key) ->
    call_with_id("facebook.groups.get",
		 [{"uid", Uid}, {"session_key", Key}]);
groups_get(Uid, Gids, Key) ->
    call_with_id("facebook.groups.get",
		 [{"uid", Uid}, {"gids", ?MERGE_LIST(Gids, ",")},
		  {"session_key", Key}]).

%% returns {groups_getMembers_response, Attrs,
%%          [{members, _, [{uid, _, [Uid]}|_]},
%%           {admins, _, [{uid, _, [Uid]}|_]},
%%           {officers, _, [{uid, _, [Uid]}|_]},
%%           {not_replied, ...}]}
groups_getMembers(Gid) ->
    groups_getMembers(Gid, ?INFINITE_SESSION).
groups_getMembers(Gid, Key) ->
    call_with_id("facebook.groups.getMembers",
		 [{"gid", Gid}, {"session_key", Key}]).

%% returns: {notifications_get_respons, Attrs, [Items]}
%%    Items == Message|Poke|Share|Friend|Group|Event
%%    Message == {messages, Attrs, [{unread, Attrs, [Flag]},
%%                                  {most_recent, Attrs, [Index]}]}
%%    Poke == {pokes, Attrs, [{unread, Attrs, [Flag]},
%%                            {most_recent, Attrs, [Index]}]}
%%    Share == {shares, Attrs, [{unread, Attrs, [Flag]},
%%                              {most_recent, Attrs, [Index]}]}
%%    Friend == {friend_requests, Attrs, [{uid, Attrs, [Uid]}|_]}
%%    Group == {group_invites, Attrs, []}
%%    Event == {event_invites, Attrs, []}
%%    Flag = "1" for unread, "0" for read
%%    Index = Id of the latest unread message, or "0" if no unread
notifications_get(Uid) ->
    notifications_get(Uid, ?INFINITE_SESSION).
notifications_get(Uid, Key) ->
    call_with_id("facebook.notifications.get",
		 [{"uid", Uid}, {"session_key", Key}]).

%% returns: {notifications_send_response, Attrs, [Url]}
%%    Url == a URL if the user needs to confirm the message (redirect the user
%%              to this URL for confirmation)
%%           nothing if no confirmation was needed, or there was an error
notifications_send(ToIds, Notification, Email) ->
    notifications_send(ToIds, Notification, Email, ?INFINITE_SESSION).
notifications_send(ToIds, Notification, [], Key) ->
    call_with_id("facebook.notifications.send",
		 [{"to_ids", ToIds}, {"notification", Notification},
		  {"session_key", Key}]);
notifications_send(ToIds, Notification, Email, Key) ->
    call_with_id("facebook.notifications.send",
		 [{"to_ids", ToIds}, {"notification", Notification},
		  {"email", Email}, {"session_key", Key}]).

%% returns: {notifications_sendRequest_response, Attrs, [Url]}
%%    Url == a URL if the user needs to confirm the message (redirect the user
%%              to this URL for confirmation)
%%           nothing if no confirmation was needed, or there was an error
notifications_sendRequest(ToIds, Type, Content, Image, Invite) ->
    notifications_sendRequest(ToIds, Type, Content, Image, Invite,
			      ?INFINITE_SESSION).
notifications_sendRequest(ToIds, Type, Content, Image, Invite, Key) ->
    call_with_id("facebook.notifications.sendRequest",
		 [{"to_ids", ?MERGE_LIST(ToIds, ",")},
                  {"type", Type}, {"content", Content},
		  {"image", Image}, {"invite", Invite},
		  {"session_key", Key}]).

%%TODO PHOTOS
photos_get(SubjId, Aid, Pids) ->
    call("facebook.photos.get",
	 [{"subj_id", SubjId}, {"aid", Aid},
	  {"pids", ?MERGE_LIST(Pids, ",")}]).

photos_getAlbums(Uid, Aids) ->
    call("facebook.photos.getAlbums",
	 [{"uid", Uid}, {"aids", ?MERGE_LIST(Aids, ",")}]).

photos_getTags(Pids) ->
    call("facebook.photos.getTags",
	 [{"pids", ?MERGE_LIST(Pids, ",")}]).

%% returns: {users_getInfo_response, Attrs,
%%           [{user, Attrs, [Items]}|_]}
%%    Items = whole bunch of stuff like
%%            {uid, Attrs, [Uid]}, {about_me, Attrs, [About]},
%%            {name, Attrs, [Name]}, etc.
users_getInfo(Uids, Fields) ->
    users_getInfo(Uids, Fields, ?INFINITE_SESSION).
users_getInfo(Uids, Fields, SessionKey) ->
    call_with_id("facebook.users.getInfo",
		 [{"uids", ?MERGE_LIST(Uids, ",")},
		  {"fields", ?MERGE_LIST(Fields, ",")},
		  {"session_key", SessionKey}]).

%% returns: {users_getLoggedInUser_response, Attrs, [Uid]}
%%    Uid == the id of the user for the given session key
%%           (so it makes little sense to use the default key)
users_getLoggedInUser(Uid) ->
    users_getLoggedInUser(Uid, ?INFINITE_SESSION).
users_getLoggedInUser(Uid, Key) ->
    call_with_id("facebook.users.getLoggedInUser",
		 [{"uid", Uid}, {"session_key", Key}]).

%% Undocumented?  assumed to return {users_isAppAdded_response...?
users_isAppAdded(Uid) ->
    users_isAppAdded(Uid, ?INFINITE_SESSION).
users_isAppAdded(Uid, Key) ->
    call_with_id("facebook.users.isAppAdded",
		 [{"uid", Uid}, {"session_key", Key}]).

%% returns: {profile_setFBML_response, Attrs, ["1"]}
profile_setFBML(Uid, Markup) ->
    profile_setFBML(Uid, Markup, ?INFINITE_SESSION).
profile_setFBML(Uid, Markup, Key) ->
    call_with_id("facebook.profile.setFBML",
		 [{"uid", Uid}, {"markup", Markup},
		  {"session_key", Key}]).

%% returns: {profile_getFBML_response, Attrs, [Fbml]}
%%    Note that Fbml with be html-ized, that is "&lt;" instead of "<"
profile_getFBML(Uid) ->
    profile_getFBML(Uid, ?INFINITE_SESSION).
profile_getFBML(Uid, Key) ->
    call_with_id("facebook.fbml.getFBML",
		 [{"uid", Uid}, {"session_key", Key}]).

%% returns: {fbml_refreshImgSrc_response, Attrs, ["1"]}
fbml_refreshImgSrc(Url) ->
    fbml_refreshImgSrc(Url, ?INFINITE_SESSION).
fbml_refreshImgSrc(Url, Key) ->
    call_with_id("facebook.fbml.refreshImgSrc",
		 [{"url", Url},
		  {"session_key", Key}]).

%% returns: {fbml_refreshRefUrl_response, Attrs, ["1"]}
fbml_refreshRefUrl(Url) ->
    fbml_refreshRefUrl(Url, ?INFINITE_SESSION).
fbml_refreshRefUrl(Url, Key) ->
    call_with_id("facebook.fbml.refreshRefUrl",
		 [{"url", Url},
		  {"session_key", Key}]).

%% returns: {fbml_setRefHandle_response, Attrs, ["1"]}
fbml_setRefHandle(Handle, Fbml) ->
    fbml_setRefHandle(Handle, Fbml, ?INFINITE_SESSION).
fbml_setRefHandle(Handle, Fbml, Key) ->
    call_with_id("facebook.fbml.setRefHandle",
		 [{"handle", Handle},
		  {"fbml", Fbml},
		  {"session_key", Key}]).

