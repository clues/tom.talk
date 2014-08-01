-module(greeting_views).
-compile(export_all).
-import(greeting_shortcuts, [render_ok/3, render_ok/4, get_cookie_value/3]).

urls() -> [
      {"^hello/?$", hello},
      {"^hello/(.+?)/?$", hello},
	    {"^create/?$", create},
      {"^join/?$", join}
    ].

get_username(Req, InputData) ->
    proplists:get_value("username", InputData,
        get_cookie_value(Req, "username", "Anonymous")).


make_cookie(Username) ->
    mochiweb_cookies:cookie("username", Username, [{path, "/"}]).

handle_hello(Req, InputData) ->
    Username = get_username(Req, InputData),
    Cookie = make_cookie(Username),
    render_ok(Req, [Cookie], greeting_dtl, [{username, Username}]).

hello('GET', Req) ->
    handle_hello(Req, Req:parse_qs());
hello('POST', Req) ->
    handle_hello(Req, Req:parse_post()).

hello('GET', Req, Username) ->
    Cookie = make_cookie(Username),
    render_ok(Req, [Cookie], greeting_dtl, [{username, Username}]);
hello('POST', Req, _) ->
  hello('POST', Req).

create('POST',Req) ->
	Username = proplists:get_value("username", Req:parse_post()),
	case broad_server:find_host_by_name(Username) of
		not_found ->
			render_ok(Req,[],chat_dtl,[{username,Username},{req,"/reg/" ++ Username},{roomname,Username}]);
		_ ->
			render_ok(Req,[],error_dtl,[{msg,"User name '"++Username++"' have been used,please try other!"}])
	end.

join('GET',Req) ->
  RoomName = proplists:get_value("roomname", Req:parse_qs()),
  case broad_server:find_host_by_name(RoomName) of
    not_found ->
      render_ok(Req,[],error_dtl,[{msg,"User room: '"++RoomName++"' not found!"}]);
    _ ->
      GuestName = integer_to_list(random:uniform(9999)),
      render_ok(Req,[],chat_dtl,[{username,GuestName},{req,"/join/" ++RoomName++"/"++ GuestName},{roomname,RoomName}])
  end.