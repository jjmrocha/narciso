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
-export([uuid/0]).

-spec uuid() -> binary().
uuid() ->
	% xxxxxxxx-xxxx-4xxx-8xxx-xxxxxxxxxxxx
	Time = erlang:phash2(utc_date(), 16#100000000),
	Host = erlang:phash2(host_process(), 16#10000),
	Rand1 = 16#4000 + rand:uniform(16#fff),
	Rand2 = 16#8000 + rand:uniform(16#fff),
	Rand3 = rand:uniform(16#ffffffffffff),
	Hex = io_lib:format("~8.16.0b-~4.16.0b-~4.16.0b-~4.16.0b-~12.16.0b", [Time, Host, Rand1, Rand2, Rand3]),
	list_to_binary(Hex).

%% ====================================================================
%% Internal functions
%% ====================================================================

utc_date() ->
	TS = {_,_, Micro} = os:timestamp(),
	{Date, Time} = calendar:now_to_universal_time(TS),
	{Date, Time, Micro}.

host_process() ->
	{node(), self()}.