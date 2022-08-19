%%%===================================================================
%%% Records
%%%===================================================================

-record(channel, {id              :: channel_id(),
                  name            :: string(),
                  topic           :: string(),
                  owner           :: client_id(),
                  moderators = [] :: [client_id()],
                  clients    = [] :: [client_id()]}).

-record(client, {id               :: client_id(),
                 name             :: string(),
                 joined           :: calendar:time(),
                 channels    = [] :: [channel_id()],
                 protocol         :: socket_connection()}).

%%%===================================================================
%%% Types
%%%===================================================================

-type client()            :: #client{}.
-type channel()           :: #channel{}.
-type channel_id()        :: uuid:uuid().
-type client_id()         :: uuid:uuid().
-type socket_connection() :: tcp_connection() | udp_connection().
-type tcp_connection()    :: inet:socket().
-type udp_connection()    :: {inet:socket(), inet:ip_address(), inet:port_number()}.

%%%===================================================================
%%% Macros
%%%===================================================================
