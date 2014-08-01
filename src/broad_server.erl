%% @author caolinong
%% @doc @todo Add description to db_server.


-module(broad_server).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("tomrtc.hrl").
%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/0,
	    find_user_by_name/1,
		find_host_by_name/1,
		% find_all_user_by_room_name/1,
	    register/3,
	    join/3,
		register_sync/3,
		broadcast_to_all/2]).

start_link() ->
	gen_server:start_link({local,?MODULE}, ?MODULE, [], []).

find_user_by_name(UserName) ->
	gen_server:call(?MODULE,{find_by_name,UserName}).

find_host_by_name(UserName) ->
	gen_server:call(?MODULE,{find_host_by_name,UserName}).

register_sync(UserName,Pid,Channel) ->
	gen_server:call(?MODULE,{register,UserName,Pid,Channel}).
%%async
register(UserName,Pid,Channel) ->
	gen_server:cast(?MODULE,{register,UserName,Pid,Channel}).

join(Info,Pid,Channel) ->
	gen_server:cast(?MODULE,{join,Info,Pid,Channel}).

broadcast_to_all(Msg,From) ->
	gen_server:cast(?MODULE,{broadcast_to_all,Msg,From}).


%% ====================================================================
%% Behavioural functions 
%% ====================================================================


%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:init-1">gen_server:init/1</a>
-spec init(Args :: term()) -> Result when
	Result :: {ok, State}
			| {ok, State, Timeout}
			| {ok, State, hibernate}
			| {stop, Reason :: term()}
			| ignore,
	State :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
init([]) ->
    {ok, #state{user_list=[],last_msg_queue=queue:new()}}.


%% handle_call/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_call-3">gen_server:handle_call/3</a>
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: term()) -> Result when
	Result :: {reply, Reply, NewState}
			| {reply, Reply, NewState, Timeout}
			| {reply, Reply, NewState, hibernate}
			| {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason, Reply, NewState}
			| {stop, Reason, NewState},
	Reply :: term(),
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity,
	Reason :: term().
%% ====================================================================
handle_call({find_by_name,UserName}, From, State) ->
	Reply = find_by_name(UserName,State#state.user_list),
    {reply, Reply, State};

handle_call({find_host_by_name,UserName}, From, State) ->
	Reply = find_host_by_name(UserName,State#state.user_list),
    {reply, Reply, State};

handle_call({register,UserName,Pid,Channel}, From, State) ->
	Result = proplists:get_value(UserName, State#state.user_list),
	case Result of
		undefined ->
			MRef = erlang:monitor(process, Pid),
			User = #user{username=UserName,pid=Pid,ref=MRef,channel=Channel},		
			broadcast_to_all("===&lt;"++UserName ++"&gt; join in===",User),
			{reply,ok,State#state{user_list=[]}};
		_ ->
			{reply, {error,user_exist}, State}
	end;

handle_call(Request, From, State) ->
    Reply = ok,
    {reply, Reply, State}.


%% handle_cast/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_cast-2">gen_server:handle_cast/2</a>
-spec handle_cast(Request :: term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================

handle_cast({join,{UserName,RoomName},Pid,Channel}, State) ->
    UserList = find_all_user_by_room_name(RoomName,State#state.user_list),
    if
    	length(UserList) =< ?MAX_CONNECTION_ONE_ROOM ->
    		gen_server:cast(?MODULE,{join_1,{UserName,RoomName},Pid,Channel});
    	true ->
			FormatMsg = format_stand_msg(#msg{username=?ADMIN_NAME,value="Opp,room full,please try later!",type="GETROOM"}),
			Channel(FormatMsg),
			exit(Pid,kill)    		
    end,
	{noreply,State};

handle_cast({join_1,{UserName,RoomName},Pid,Channel}, State) ->
	error_logger:info_msg("user <~p> want join room <~p> ~n",[UserName,RoomName]),
	Result = find_host_by_name(RoomName, State#state.user_list),
	case Result of
		not_found ->
			FormatMsg = format_stand_msg(#msg{username=?ADMIN_NAME,
											  value="There is no room &lt;"++ RoomName ++"&gt,please try other!",
											  type="GETROOM"}),
			Channel(FormatMsg),
			exit(Pid,kill),		
			{noreply, State};		
		_ ->
			MRef = erlang:monitor(process, Pid),
			User = #user{username=UserName,pid=Pid,ref=MRef,channel=Channel,room_name=RoomName},
			
			broadcast_to_all("===&lt;"++UserName ++"&gt; join in===",User#user{username=?ADMIN_NAME}),
			% lists:foreach(fun(BinMsg)->Channel(BinMsg) end,queue:to_list(State#state.last_msg_queue)),
			{noreply,State#state{user_list=[User|State#state.user_list]}}
	end;

handle_cast({register,UserName,Pid,Channel}, State) ->
	RoomList = find_all_room(State#state.user_list),
	if
		length(RoomList) =< ?MAX_ROOM_NUM ->
			gen_server:cast(?MODULE,{register_1,UserName,Pid,Channel});
		true ->
			FormatMsg = format_stand_msg(#msg{username=?ADMIN_NAME,
											  value="Opp,no room allocate,please try later!",
											  type="GETROOM_ERR"}),
			Channel(FormatMsg),
			exit(Pid,kill)	
	end,
	{noreply,State};

handle_cast({register_1,UserName,Pid,Channel}, State) ->
	error_logger:info_msg("user <~p> want register pid <~p>~n",[UserName,Pid]),
	Result = find_host_by_name(UserName, State#state.user_list),
	case Result of
		not_found ->
			MRef = erlang:monitor(process, Pid),
			User = #user{username=UserName,pid=Pid,ref=MRef,channel=Channel,host=true,room_name=UserName},
			FormatMsg = format_stand_msg(#msg{username=?ADMIN_NAME,
											  value=UserName,
											  type="GETROOM_OK"}),
			Channel(FormatMsg),				
			{noreply,State#state{user_list=[User|State#state.user_list]}};
		_ ->
			FormatMsg = format_stand_msg(#msg{username=?ADMIN_NAME,
											  value="username &lt;"++ UserName ++"&gt; already be used,<br>please use other name to login!",
											  type="GETROOM_ERR"}),
			Channel(FormatMsg),
			exit(Pid,kill),		
			{noreply, State}
	end;

handle_cast({broadcast_to_all,Msg,Pid},State) when is_pid(Pid)->
	FromUser = find_by_pid(Pid,State#state.user_list),
	broadcast_to_all(Msg,FromUser),
	{noreply,State};

handle_cast({broadcast_to_all,Msg,FromUser},State) when FromUser =/= 'not_found'->
	%%error_logger:info_msg(" user:~p will broad msg: ~p~n",[FromUser#user.username,Msg]),
    FormatMsg = format_stand_msg(#msg{username=FromUser#user.username,
									  value=Msg,
									  type="CHATMSG"}),	
	RoomUserList = find_all_user_by_room_name(FromUser#user.room_name,State#state.user_list),
	ok=lists:foreach(fun(#user{channel=Channel}) ->
		Channel(FormatMsg) end,RoomUserList),
	% CurrentMsgBoxLen = queue:len(State#state.last_msg_queue),
	% LastMsgQueue = if
	% 	CurrentMsgBoxLen >= ?MAX_LAST_MSG_LEN ->
	% 		{_,NewQ}=queue:out(State#state.last_msg_queue),
	% 		NewQ;
	% 	true ->
	% 		State#state.last_msg_queue
	% 	end,
	% NewMsgQueue = queue:in(FormatMsg,LastMsgQueue),
	{noreply,State};

handle_cast(Msg, State) ->
    {noreply, State}.


%% handle_info/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_info-2">gen_server:handle_info/2</a>
-spec handle_info(Info :: timeout | term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
%% user crash or exit
handle_info({'DOWN', MRef, process, Pid, _Reason},State) ->
	error_logger:info_msg("receive : ~p crash with reason ~p~n",[Pid,_Reason]),
	case find_by_ref(MRef,State#state.user_list) of
		not_found ->
			{noreply, State};
		User ->
			error_logger:info_msg("user: ~p exit with reason ~p~n",[User#user.username,_Reason]),
			NewUserList = delete_by_ref(User#user.ref,State#state.user_list),
			broadcast_to_all("===&lt;"++ User#user.username ++"&gt; sign out==",User#user{username=?ADMIN_NAME}),
    		{noreply, State#state{user_list=NewUserList}}
    end;

handle_info(Info, State) ->
    {noreply, State}.


%% terminate/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:terminate-2">gen_server:terminate/2</a>
-spec terminate(Reason, State :: term()) -> Any :: term() when
	Reason :: normal
			| shutdown
			| {shutdown, term()}
			| term().
%% ====================================================================
terminate(Reason, State) ->
    ok.


%% code_change/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:code_change-3">gen_server:code_change/3</a>
-spec code_change(OldVsn, State :: term(), Extra :: term()) -> Result when
	Result :: {ok, NewState :: term()} | {error, Reason :: term()},
	OldVsn :: Vsn | {down, Vsn},
	Vsn :: term().
%% ====================================================================
code_change(OldVsn, State, Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================

find_by_pid(_,[]) ->
	not_found;
find_by_pid(Pid,[#user{pid=Pid}=User|T]) ->
	User;
find_by_pid(Pid,[_|T]) ->
	find_by_pid(Pid,T).

find_by_ref(_,[]) ->
	not_found;
find_by_ref(Mref,[#user{ref=Mref}=User|T]) ->
	User;
find_by_ref(Mref,[_|T]) ->
	find_by_ref(Mref,T).


find_host_by_name(_,[]) ->
	not_found;
find_host_by_name(UserName,[#user{username=UserName,host=true}=User|T]) ->
	User;
find_host_by_name(UserName,[_|T]) ->
	find_host_by_name(UserName,T).

find_all_room(UserList) ->
	find_all_room(UserList,[]).
find_all_room([],R) ->
	R;
find_all_room([#user{host=true}=User|T],R) ->
	find_all_room(T,[User|R]);
find_all_room([User|T],R) ->
	find_all_room(T,R).	

find_by_name(_,[]) ->
	not_found;
find_by_name(UserName,[#user{username=UserName}=User|T]) ->
	User;
find_by_name(UserName,[_|T]) ->
	find_by_name(UserName,T).

find_all_user_by_room_name(RoomName,UserList) ->
	find_all_user_by_room_name(RoomName,UserList,[]).

find_all_user_by_room_name(RoomName,[],R) ->
	R;
find_all_user_by_room_name(RoomName,[#user{room_name=RoomName}=User|T],R) ->
	find_all_user_by_room_name(RoomName,T,[User|R]);
find_all_user_by_room_name(RoomName,[User|T],R) ->
	find_all_user_by_room_name(RoomName,T,R).

delete_by_name(UserName,UserList) ->
	delete_by_name(UserName,UserList,[]).
delete_by_name(UserName,[],DestList) ->
	DestList;
delete_by_name(UserName,[#user{username=UserName}=User|T],DestList) ->
	delete_by_name(UserName,T,DestList);
delete_by_name(UserName,[User|T],DestList) ->
	delete_by_name(UserName,T,[User|DestList]).

delete_by_ref(Mref,UserList) ->
	delete_by_ref(Mref,UserList,[]).
delete_by_ref(Mref,[],DestList) ->
	DestList;
delete_by_ref(Mref,[#user{ref=Mref}=User|T],DestList) ->
	delete_by_ref(Mref,T,DestList);
delete_by_ref(Mref,[User|T],DestList) ->
	delete_by_ref(Mref,T,[User|DestList]).

format_stand_msg(Msg) ->
	{_,{H, M, S}} =  calendar:now_to_local_time(erlang:now()),
    iolist_to_binary(["{\"username\":\"",Msg#msg.username,"\",\"type\":\"", Msg#msg.type, "\",\"value\":\"", Msg#msg.value,"\",\"time\":\"",
						  integer_to_list(H),"-",
						  integer_to_list(M),"-",
						  integer_to_list(S),"\"}"]).