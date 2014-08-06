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
		find_all_room/0,
		find_room_by_room_id/1,
	    register/3,
	    join/3,
		get_counter/0,
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
register({RoomName,UserName},Pid,Channel) ->
	gen_server:cast(?MODULE,{register,{RoomName,UserName},Pid,Channel}).

join(Info,Pid,Channel) ->
	gen_server:cast(?MODULE,{join,Info,Pid,Channel}).

broadcast_to_all(Msg,From) ->
	gen_server:cast(?MODULE,{broadcast_to_all,Msg,From}).
find_all_room() ->
	gen_server:call(?MODULE,find_all_room).

find_room_by_room_id(RoomId) ->
	gen_server:call(?MODULE,{find_room_by_room_id,RoomId}).

get_counter() ->
	gen_server:call(?MODULE,get_counter).

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
    {ok, #state{}}.


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
handle_call(find_all_room, From, #state{room_list=RoomList}=State) ->
    Reply = RoomList,
    {reply, Reply, State};

handle_call({find_room_by_room_id,RoomId}, From, #state{room_list=RoomList}=State) ->
    Reply = find_room_by_room_id(RoomId,RoomList),
    {reply, Reply, State};

handle_call(Request, From, State) ->
    Reply = State#state.counter,
    {reply, Reply, State};

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

handle_cast({join,{UserName,RoomId},Pid,Channel}, State) ->
	Room = find_room_by_room_id(RoomId,State#state.room_list),
	case Room of
		not_found ->
			FormatMsg = format_stand_msg(#msg{username=?ADMIN_NAME,
											  value="There is no room &lt;"
											  ++ RoomId 
											  ++"&gt,please try other!",
											  type="GETROOM"}),
			Channel(FormatMsg),
			exit(Pid,kill),		
			{noreply, State};	
		_ ->
			Users =find_all_user_by_room_id(RoomId,State#state.user_list),
		    if
		    	length(Users) =< ?MAX_CONNECTION_ONE_ROOM ->
		    		gen_server:cast(?MODULE,{join_1,{UserName,Room},Pid,Channel});
		    	true ->
					FormatMsg = format_stand_msg(#msg{username=?ADMIN_NAME,value="Opp,room full,please try later!",type="GETROOM"}),
					Channel(FormatMsg),
					exit(Pid,kill)    		
		    end
	end,
	{noreply,State};

handle_cast({join_1,{UserName,Room},Pid,Channel}, State) ->
	error_logger:info_msg("user <~p> want join room <~p> ~n",[UserName,Room#room.name]),
	
	UserId = tomrtc:base64UUID(),

	MRef = erlang:monitor(process, Pid),
	User = #user{id=UserId,name=UserName,pid=Pid,ref=MRef,channel=Channel,room_id=Room#room.id},
	
	gen_server:cast(?MODULE, {broadcast_to_all,#msg{type=?MSG_TYPE_INOUT,
						  value="===&lt;"++UserName ++"&gt; join in==="},
							  User#user{name=?ADMIN_NAME}}),
	lists:foreach(fun(BinMsg)->Channel(BinMsg) end,queue:to_list(Room#room.last_mq)),
	{noreply,State#state{user_list=[User|State#state.user_list],counter=State#state.counter+1}};

handle_cast({register,{RoomName,UserName},Pid,Channel}, State) ->
	if
		length(State#state.room_list) =< ?MAX_ROOM_NUM ->
			gen_server:cast(?MODULE,{register_1,{RoomName,UserName},Pid,Channel});
		true ->
			FormatMsg = format_stand_msg(#msg{username=?ADMIN_NAME,
											  value="Opp,no room allocate,please try later!",
											  type="GETROOM_ERR"}),
			Channel(FormatMsg),
			exit(Pid,kill)	
	end,
	{noreply,State};

handle_cast({register_1,{RoomName,UserName},Pid,Channel}, #state{counter=Counter,
																 room_list=RoomList,
																 user_list=UserList}=State) ->
	error_logger:info_msg(" user:<~p> want create room: <~p>~n",[UserName,RoomName]),
	
	RoomId = tomrtc:base64UUID(),
	UserId = tomrtc:base64UUID(),
	MRef = erlang:monitor(process, Pid),
	User = #user{id= UserId,
				 name=UserName,
				 pid=Pid,
				 ref=MRef,
				 channel=Channel,
				 host=true,
				 room_id=RoomId},
	
	FormatMsg = format_stand_msg(#msg{username=?ADMIN_NAME,
											  value=RoomId,
											  type="GETROOM_OK"}),

	Room = #room{name=RoomName,id=RoomId},
	Channel(FormatMsg),		
	{noreply,State#state{counter=Counter+1,room_list=[Room|RoomList],user_list=[User|UserList]}};

handle_cast({broadcast_to_all,Msg,Pid},State) when is_pid(Pid)->
	FromUser = find_user_by_pid(Pid,State#state.user_list),
	broadcast_to_all(#msg{value=Msg},FromUser),
	{noreply,State};

handle_cast({broadcast_to_all,Msg,FromUser},State) when FromUser =/= 'not_found'->
	%%error_logger:info_msg(" user:~p will broad msg: ~p~n",[FromUser#user.username,Msg]),
    FormatMsg = format_stand_msg(Msg#msg{username=FromUser#user.name}),
	Room = find_room_by_room_id(FromUser#user.room_id,State#state.room_list),
	case Room of
		not_found ->
			{noreply,State};
		_ ->
			
			Users =find_all_user_by_room_id(Room#room.id,State#state.user_list),
			ok=lists:foreach(fun(#user{channel=Channel}) ->
				Channel(FormatMsg) end,Users),
			UpdatedRoom =case Msg#msg.type of
				?MSG_TYPE_CHAT ->
					put_msg_to_mq(FormatMsg,Room);
				_ ->
					Room
			end,
			RoomList =lists:keyreplace(Room#room.id,2,State#state.room_list,UpdatedRoom),
			{noreply,State#state{room_list=RoomList}}
	end;

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
	case find_user_by_ref(MRef,State#state.user_list) of
		not_found ->
			{noreply, State};
		User ->
			error_logger:info_msg("user: ~p exit with reason ~p~n",[User#user.name,_Reason]),
			NewUserList = delete_user_by_ref(User#user.ref,State#state.user_list),
			
			% if room no user,the room will be clear
			UserList =find_all_user_by_room_id(User#user.room_id,NewUserList),
			NewRoomList =case UserList of
				[] ->
					delete_room_by_room_id(User#user.room_id,State#state.room_list);
				_ ->
				    State#state.room_list
			end,
			broadcast_to_all(#msg{value="===&lt;"++ User#user.name ++"&gt; sign out==",
								  type=?MSG_TYPE_INOUT},User#user{name=?ADMIN_NAME}),
    		{noreply, State#state{user_list=NewUserList,room_list=NewRoomList}}
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

find_user_by_ref(_,[]) ->
	not_found;
find_user_by_ref(Mref,[#user{ref=Mref}=User|T]) ->
	User;
find_user_by_ref(Mref,[_|T]) ->
	find_user_by_ref(Mref,T).


find_host_by_room_id(_,[]) ->
	not_found;
find_host_by_room_id(RoomId,[#user{room_id=RoomId,host=true}=User|T]) ->
	User;
find_host_by_room_id(RoomId,[_|T]) ->
	find_host_by_room_id(RoomId,T).

find_room_by_room_id(RoomId,[]) ->
	not_found;
find_room_by_room_id(RoomId,[#room{id=RoomId}=Room|T]) ->
	Room;
find_room_by_room_id(RoomId,[H|T]) ->
	find_room_by_room_id(RoomId,T).

find_user_by_id(_,[]) ->
	not_found;
find_user_by_id(UserId,[#user{id=UserId}=User|T]) ->
	User;
find_user_by_id(UserId,[_|T]) ->
	find_user_by_id(UserId,T).

find_user_by_pid(_,[]) ->
	not_found;
find_user_by_pid(Pid,[#user{pid=Pid}=User|T]) ->
	User;
find_user_by_pid(Pid,[_|T]) ->
	find_user_by_pid(Pid,T).

find_all_user_by_room_id(RoomId,UserList) ->
	find_all_user_by_room_id(RoomId,UserList,[]).

find_all_user_by_room_id(RoomId,[],R) ->
	R;
find_all_user_by_room_id(RoomId,[#user{room_id=RoomId}=User|T],R) ->
	find_all_user_by_room_id(RoomId,T,[User|R]);
find_all_user_by_room_id(RoomId,[User|T],R) ->
	find_all_user_by_room_id(RoomId,T,R).

delete_room_by_room_id(RoomId,[]) ->
	[];
delete_room_by_room_id(RoomId,[#room{id=RoomId}|T]) ->
	T;
delete_room_by_room_id(RoomId,[H|T]) ->
	delete_room_by_room_id(RoomId,T).

delete_user_by_ref(Mref,UserList) ->
	delete_user_by_ref(Mref,UserList,[]).
delete_user_by_ref(Mref,[],DestList) ->
	DestList;
delete_user_by_ref(Mref,[#user{ref=Mref}=User|T],DestList) ->
	delete_user_by_ref(Mref,T,DestList);
delete_user_by_ref(Mref,[User|T],DestList) ->
	delete_user_by_ref(Mref,T,[User|DestList]).

format_stand_msg(Msg) ->
	{_,{H, M, S}} =  calendar:now_to_local_time(erlang:now()),
    iolist_to_binary(["{\"username\":\"",Msg#msg.username,"\",\"type\":\"", Msg#msg.type, "\",\"value\":\"", Msg#msg.value,"\",\"time\":\"",
						  integer_to_list(H),"-",
						  integer_to_list(M),"-",
						  integer_to_list(S),"\"}"]).

put_msg_to_mq(FormatMsg,Room) ->
	MQ =Room#room.last_mq,
	CurrentMsgBoxLen = queue:len(MQ),
	LastMsgQueue = if
		CurrentMsgBoxLen >= ?MAX_LAST_MSG_LEN ->
			{_,NewQ}=queue:out(MQ),
			NewQ;
		true ->
			MQ
		end,
	NewMq =queue:in(FormatMsg,LastMsgQueue),
	Room#room{last_mq=NewMq}.