-module(usg_plc).

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


% пакет с регистрацией контроллера
process(Host, Port, <<2,7,0,Plc_addr:16/integer-little,_:16>> = Bin) ->
  case crc16:calc(Bin) of
    0 -> {Pid, _} = reg_or_locate({n,l,{plc, Plc_addr}}, undefined, fun worker/0), 
         Pid ! {register, Host, Port};
    _Else -> false
  end;

% обработка сетевого пакета
process(Host, Port, <<_,Len:16/integer-little,_,_,From:16/integer-little,_,_,_/binary>> = Bin) when Len == byte_size(Bin) ->
  {Pid, _} = reg_or_locate({n,l,{plc, From}}, undefined, fun worker/0),
  Pid ! {response, Host, Port, Bin};

% игнорировать все не по формату
process(_,_,_) ->
  false.

worker() ->
  worker(undefined, 0, empty).

worker(Host, Port, LastReq) ->
  receive
    {register, NewHost, NewPort} ->
      case {NewHost, NewPort} of % Если совпадают с предидущими
      	{Host, Port} -> false;      % ничего не делать 
      	_Else -> resending(NewHost, NewPort, LastReq) % Иначе повторная отпрвка запроса
      end,
      worker(NewHost, NewPort, LastReq);

    {request, Bin} ->
      process_request(Host, Port, Bin),   
      worker(Host, Port, Bin);

    {response, NewHost, NewPort, Bin} ->
      process_response(Bin),
      worker(NewHost, NewPort, LastReq)

  after 600000 -> % Завершить если 10 минут ничего не происходило
    false
  end.

resending(_Host, _Port, empty) ->  
  false;
resending(Host, Port, Bin) ->  
  send(Host, Port, Bin).


process_request(undefined, 0, Bin) -> % Не зарегистрирован, ответить по порту FF ошибкой FF
  <<_:24,Id,_,From:16/integer-little,To:16/integer-little,_/binary>> = Bin,
  process_response(<<1,11:16/integer-little,Id,1,To:16/integer-little,From:16/integer-little,16#FF,16#FF>>);

process_request(Host, Port, Bin) ->  
  send(Host, Port, Bin).


process_response(<<_:56,To:16/integer-little,_/binary>> = Bin) ->
  {Pid, _} = gproc:reg_or_locate({n,l,{comp, To}}, undefined, fun usg_comp:worker/0),
  Pid ! {response, Bin}.
