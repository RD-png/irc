%%%===================================================================
%%% Records
%%%===================================================================

-record(channel, {name        = <<>> :: binary(),
                  topic       = <<>> :: binary(),
                  owner              :: client_id(),
                  subscribers = []   :: [{client_id(), pid()}]}).

-record(client, {id                       :: client_id(),
                 name        = <<"User">> :: binary(),
                 joined                   :: {calendar:time(), calendar:date()},
                 owned       = []         :: [channel_name()],
                 protocol                 :: socket_connection()}).

%%%===================================================================
%%% Types
%%%===================================================================

-type client()            :: #client{}.
-type channel()           :: #channel{}.
-type channel_name()      :: binary().
-type client_id()         :: uuid:uuid().
-type socket_connection() :: tcp_connection() | udp_connection().
-type tcp_connection()    :: inet:socket().
-type udp_connection()    :: {inet:socket(),
                              inet:ip_address() | inet:hostname(),
                              inet:port_number()}.

%%%===================================================================
%%% Macros
%%%===================================================================
