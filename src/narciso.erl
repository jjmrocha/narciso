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
	Time = erlang:phash2(os:timestamp(), 16#100000000),
	Host = erlang:phash2(node(), 16#10000),
	Pid = 16#4000 + erlang:phash2(self(), 16#1000),
	Rand1 = 16#8000 + rand:uniform(16#fff),
	Rand2 = rand:uniform(16#ffffffffffff),
	Hex = io_lib:format("~8.16.0b-~4.16.0b-~4.16.0b-~4.16.0b-~12.16.0b", [Time, Host, Pid, Rand1, Rand2]),
	list_to_binary(Hex).

%% ====================================================================
%% Internal functions
%% ====================================================================


