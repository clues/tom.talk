-module(greeting_views).
-compile(export_all).
-import(greeting_shortcuts, [render_ok/3, render_ok/4, get_cookie_value/3]).
-include("tomrtc.hrl").
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
	RoomName = proplists:get_value("roomname", Req:parse_post()),
	UserName = proplists:get_value("username", Req:parse_post()),
	  GuestName =case UserName of
		  undefined ->
			  "Host"++integer_to_list(broad_server:get_counter());
		  _ ->
			UserName
	  end,	
	RoomList = broad_server:find_all_room(),
	if
		length(RoomList)  < ?MAX_ROOM_NUM ->
			render_ok(Req,[],chat_dtl,[{username,GuestName},{req,"/reg/" ++ RoomName ++ "/" ++ GuestName},{roomname,RoomName}]);
		true ->
			render_ok(Req,[],error_dtl,[{msg,"No enough room allocate!!"}])
	end.

join('GET',Req) ->
  RoomId = proplists:get_value("roomid", Req:parse_qs()),
  UserName = proplists:get_value("username", Req:parse_qs()),
  GuestName =case UserName of
	  undefined ->
		  "Guest"++integer_to_list(broad_server:get_counter());
	  _ ->
		UserName
  end,
  case broad_server:find_room_by_room_id(RoomId) of
    not_found ->
      render_ok(Req,[],error_dtl,[{msg,"User room: '"++RoomId++"' not found!"}]);
    Room ->
      render_ok(Req,[],chat_dtl,[{username,GuestName},{req,"/join/" ++RoomId++"/"++ GuestName},{roomname,Room#room.name}])
  end.