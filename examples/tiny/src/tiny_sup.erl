-module(tiny_sup).

-behaviour(application).
-behaviour(supervisor).

-export([start_link/0, init/1, start/2, stop/1]).

-define(SERVER, ?MODULE).

start(_StartType, _StartArgs) ->
    ?MODULE:start_link().

stop(_State) ->
    ok.

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    io:fwrite("Starting web server on localhost:3000~n", []),

    Config = #{strategy => one_for_all,
               intensity => 0,
               period => 1},
    Children = [db_child_spec([]),
                web_child_spec([])],
    {ok, {Config, Children}}.

web_child_spec(_) ->
    #{id => tiny_web,
      start => {elli, start_link, [[{callback, tiny_web}, {port, 3000}]]},
      restart => permanent,
      shutdown => 5000,
      type => worker}.

db_child_spec(_) ->
    #{id => tiny_db,
      start => {gen_server, start_link, [{local, tiny_db}, tiny_db, [], []]},
      restart => permanent,
      shutdown => 5000,
      type => worker}.
