%%%===================================================================
%%% Types
%%%===================================================================

-type socket_connection() :: tcp_connection() | udp_connection().
-type tcp_connection() :: inet:socket().
-type udp_connection() :: {inet:socket(), inet:ip_address(), inet:port_number()}.

%%%===================================================================
%%% Records
%%%===================================================================

-record(channel, {id              :: uuid:uuid(),
                  name            :: string(),
                  topic           :: string(),
                  owner           :: uuid:uuid(),
                  moderators = [] :: [uuid:uuid()],
                  clients    = [] :: [uuid:uuid()]}).

-record(client, {id               :: uuid:uuid(),
                 name             :: string(),
                 joined           :: calendar:time(),
                 channels    = [] :: [uuid:uuid()],
                 protocol         :: socket_connection()}).

%%%===================================================================
%%% Macros
%%%===================================================================

-define(CLIENT_SOCKET_TIMEOUT, 3600).
