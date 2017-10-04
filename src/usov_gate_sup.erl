%%%-------------------------------------------------------------------
%% @doc usov_gate top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(usov_gate_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link(Args) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, Args).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([CompPort, PlcPort]) ->
    SupervisorSpecification = #{
        strategy => one_for_one,
        intensity => 10,
        period => 60},

    ChildSpecifications =
        [#{id => usg_comp,
           start => {usg_comp, start_link, [CompPort]},
           restart => permanent,
           shutdown => 2000,
           type => worker,
           modules => [usg_comp]},
         #{id => usg_plc,
           start => {usg_plc, start_link, [PlcPort]},
           restart => permanent,
           shutdown => 2000,
           type => worker,
           modules => [usg_plc]}
        ],
    {ok, {SupervisorSpecification, ChildSpecifications}}.

%%====================================================================
%% Internal functions
%%====================================================================
