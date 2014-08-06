%% @author caolinong
%% @doc @todo Add description to tomrtc_server.


-module(tomrtc).

%% ====================================================================
%% API functions
%% ====================================================================
-compile(export_all).

-define(LOOP, {?MODULE, loop}).

-include("tomrtc.hrl").

t()->
    broad_server:start_link(),
	start([{port,8080}]).

start() ->
    start([{port,?DEFAULT_PORT}]).

start(Options = [{port, _Port}]) ->
	application:start(sasl),
    mochiweb_http:start([
						{name, ?MODULE},
						{loop, {?MODULE, loop, [broad_server]}} |Options
						]).


loop(Req,Broadcaster) ->
	H = mochiweb_request:get_header_value("Upgrade", Req),
	loop(Req,Broadcaster,H =/= undefined andalso string:to_lower(H) =:= "websocket").

loop(Req,Broadcaster,true) ->
    {ReentryWs, ReplyChannel} = mochiweb_websocket:upgrade_connection(
                                  Req, fun ws_loop/3),
	case Req:get(path) of
        "/reg" ++ Rest ->
            M = re:run(Rest, "^/(.+?)/(.+?)/?$", [global, {capture, all_but_first, list}]),
            error_logger:info_msg("~p --~p req error:~p~n",[?MODULE,?LINE,M]),
            case M of
                {match,[[RoomName,UserName]]} ->
                    broad_server:register({RoomName,UserName},self(),ReplyChannel);
                _ ->
                    ok
            end;
        "/join" ++ Rest ->
            M = re:run(Rest, "^/(.+?)/(.+?)/?$", [global, {capture, all_but_first, list}]),
            error_logger:info_msg("~p --~p req error:~p~n",[?MODULE,?LINE,M]),
            case M of
                {match,[[RoomId,UserName]]} ->
                    broad_server:join({UserName,RoomId},self(),ReplyChannel);
                _ ->
                    ok
            end
    end,
    ReentryWs(Broadcaster);

loop(Req,_Broadcaster,false) ->
    "/" ++ Path = Req:get(path),   
	try	
        case dispatch(Req, greeting_views:urls()) of
            none -> 
                % No request handler found
                case filelib:is_file(filename:join([?DOCROOT, Path])) of
                    true -> 
                        % If there's a static file, serve it
                        Req:serve_file(Path, ?DOCROOT);
                    false ->
                        % Otherwise the page is not found
                        Req:not_found()
                end;
            Response ->
                Response
        end
    catch
        _Er:_Rn ->
            error_logger:info_msg("~p -- req error:~p,~p~n",[?MODULE,_Er,_Rn]),
			throw({thisError, "Oop,Server hung!!"})
    end.

% Iterate recursively on our list of {Regexp, Function} tuples
dispatch(_, []) -> none;
dispatch(Req, [{Regexp, Function}|T]) -> 
    "/" ++ Path = Req:get(path),
    Method = Req:get(method),
    Match = re:run(Path, Regexp, [global, {capture, all_but_first, list}]),
    case Match of
        {match,[MatchList]} -> 
            % We found a regexp that matches the current URL path
            case length(MatchList) of 
				0 -> 
                    % We didn't capture any URL parameters
                    greeting_views:Function(Method, Req);
                Length when Length > 0 -> 
                    % We pass URL parameters we captured to the function
                    Args = lists:append([[Method, Req], MatchList]),
                    apply(greeting_views, Function, Args)
            end;
        _ -> 
            dispatch(Req, T)
    end.


ws_loop(Payload, Broadcaster, _ReplyChannel) ->
    Received = list_to_binary(Payload),
	JSON = json:decode(Received),
	case JSON of
		 {ok,{[{<<"type">>,<<"CHATMSG">>}, {<<"value">>, Value} ]}} ->
			 broad_server:broadcast_to_all(Value,self());
		 _ ->
			 ignored
	end,
    Broadcaster.

to_hex([]) ->
    [];
to_hex(Bin) when is_binary(Bin) ->
    to_hex(binary_to_list(Bin));
to_hex([H|T]) ->
    [to_digit(H div 16), to_digit(H rem 16) | to_hex(T)].

to_digit(N) when N < 10 -> $0 + N;
to_digit(N)             -> $a + N-10.

base64UUID() ->
	Now = {_, _, Micro} = now(),
	Nowish = calendar:now_to_universal_time(Now),
	Nowsecs = calendar:datetime_to_gregorian_seconds(Nowish),
	Then = calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}),
	Prefix = io_lib:format("~14.16.0b", [(Nowsecs - Then) * 1000000 + Micro]),
	U=list_to_binary(to_hex(crypto:rand_bytes(9))),
    base64:encode_to_string(U).