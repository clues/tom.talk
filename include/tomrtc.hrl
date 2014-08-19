-define(DOCROOT,"priv/www").
-define(DEFAULT_PORT,8080).
-record(state,{room_list=[],user_list=[],counter=1}).
-record(user,{id,name,pid,channel,ref,in_time,host=false,room_id}).
-define(MAX_USER,100).%% user number limit.
-define(MAX_ROOM_NUM,100).
-define(MAX_CONNECTION_ONE_ROOM,50).
-define(MAX_LAST_MSG_LEN,50). %%keep lastest message in history box
-define(WELCOME_MSG,"").
-define(ADMIN_NAME,"admin.tom").

-record(room,{id,name,last_mq=queue:new(),pwd}).


-define(SIG_TYPE_GETROOM_OK,"get_room_ok").
-define(SIG_TYPE_GETROOM_ERR,"get_room_err").
-define(SIG_TYPE_JOINROOM_OK,"join_room_ok").
-define(SIG_TYPE_JOINROOM_ERR,"join_room_err").
-define(SIG_TYPE_CHAT,"chat").
-define(SIG_TYPE_INOUT,"inout").
-define(SIG_TYPE_OFFER,"offer").
-define(SIG_TYPE_ANSWER,"answer").
-define(SIG_TYPE_CANDIDATE,"candidate").
-define(SIG_TYPE_BYE,"bye").
-record(msg,{type=?SIG_TYPE_CHAT,time,username,value}).