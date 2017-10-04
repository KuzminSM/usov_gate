-module(usg_comp).

-import(gproc, [reg_or_locate/3]).

-export([start_link/1, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-export([worker/0]).

start_link(Port) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, Port, []).

stop() ->
  gen_server:cast(?MODULE, stop).

init(Port) ->
  {ok, Sock} = gen_udp:open(Port,[binary,{active,once}]),
  {ok, {server, Sock}}.

terminate(_Reason, {server, Sock}) ->
  gen_udp:close(Sock).

handle_cast(stop, {server, Sock}) ->
  {stop, normal, {server, Sock}}.

handle_info({udp, _Socket, Host, Port, Bin}, {server, Sock}) ->
  inet:setopts(Sock, [{active, once}]),
  process(Host, Port, Bin),
  {noreply, {server, Sock}};

handle_info(_Msg, {server, Sock}) ->
  {noreply, {server, Sock}}.
  
send(Host, Port, Bin) ->
  gen_server:call(?MODULE, {message, Host, Port, Bin}).

handle_call({message, Host, Port, Bin}, _From, {server, Sock}) ->
  gen_udp:send(Sock, Host, Port, Bin),
  {reply, ok, {server, Sock}}.

% Обработка сетевого пакеиа
process(Host, Port, <<_,Len:16/integer-little,_,_,From:16/integer-little,_,_,_/binary>> = Bin) when Len == byte_size(Bin) ->
  {Pid, _} = reg_or_locate({n,l,{comp, From}}, undefined, fun worker/0),
  Pid ! {request, Host, Port, Bin};

% Не попадающие в формат - игнорируем
process(_,_,_) ->
  false.


worker() ->
  worker(undefined, 0).

worker(Host, Port) ->
  receive
    {request, NewHost, NewPort, Bin} ->
      process_request(Bin),
      worker(NewHost, NewPort);

    {response, Bin} ->
      process_response(Host, Port, Bin),
      worker(Host, Port)

  after 600000 -> % 10 минут
    false
  end.


process_request(<<_:56,To:16/integer-little,_/binary>> = Bin) ->
  {Pid, _} = reg_or_locate({n,l,{plc, To}}, undefined, fun usg_plc:worker/0),
  Pid ! {request, Bin}.


process_response(undefined, 0, _) ->
  false;

process_response(Host, Port, Bin) ->
  send(Host, Port, Bin).
