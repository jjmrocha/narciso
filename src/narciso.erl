%%
%% Copyright 2016 Joaquim Rocha <jrocha@gmailbox.org>
%% 
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%

-module(narciso).

%% ====================================================================
%% API functions
%% ====================================================================
-export([uuid/0, id/0]).
-export([id_to_uuid/1]).

-spec uuid() -> binary().
uuid() ->
	% xxxxxxxx-xxxx-4xxx-8xxx-xxxxxxxxxxxx
	V4 = v4(),
	to_string(V4).
	
-spec id() -> binary().
id() ->
	[A, B, C, D, E] = v4(),
	<<A:32, B:16, C:16, D:16, E:48>>.

-spec id_to_uuid(Id :: binary()) -> binary().	
id_to_uuid(<<A:32, B:16, C:16, D:16, E:48>>) ->
	to_string([A, B, C, D, E]).

%% ====================================================================
%% Internal functions
%% ====================================================================

to_string([A, B, C, D, E]) ->
	Hex = io_lib:format("~8.16.0b-~4.16.0b-~4.16.0b-~4.16.0b-~12.16.0b", [A, B, C, D, E]),
	list_to_binary(Hex).

v4() ->
	Time = erlang:phash2(utc_date(), 16#100000000),
	Host = erlang:phash2(host_process(), 16#10000),
	Rand1 = 16#4000 + rand:uniform(16#fff),
	Rand2 = 16#8000 + rand:uniform(16#fff),
	Rand3 = rand:uniform(16#ffffffffffff),
	[Time, Host, Rand1, Rand2, Rand3].

utc_date() ->
	TS = {_,_, Micro} = os:timestamp(),
	{Date, Time} = calendar:now_to_universal_time(TS),
	{Date, Time, Micro}.

host_process() ->
	{node(), self()}.
