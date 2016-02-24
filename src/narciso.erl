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
-export([uuid/0, unique/0, token/0, id/0]).
-export([unique_to_uuid/1, uuid_to_unique/1]).
-export([unique_to_token/1, token_to_unique/1]).
-export([unique_to_id/1, id_to_unique/1]).

-spec uuid() -> binary().
uuid() ->
	V4 = v4(),
	to_string(V4).
	
-spec unique() -> binary().
unique() ->
	[A, B, C, D, E] = v4(),
	<<A:32, B:16, C:16, D:16, E:48>>.

-spec token() -> binary().
token() ->
	unique_to_token(unique()).

-spec id() -> integer().
id() ->
	unique_to_id(unique()).
	
-spec unique_to_uuid(Id :: binary()) -> binary().	
unique_to_uuid(<<A:32, B:16, C:16, D:16, E:48>>) ->
	to_string([A, B, C, D, E]).

-spec uuid_to_unique(Uuid :: binary()) -> binary().	
uuid_to_unique(<<AA:64, _:8, BB:32, _:8, CC:32, _:8, DD:32, _:8, EE:96>>) ->
	A = binary_to_integer(<<AA:64>>, 16),
	B = binary_to_integer(<<BB:32>>, 16),
	C = binary_to_integer(<<CC:32>>, 16),
	D = binary_to_integer(<<DD:32>>, 16),
	E = binary_to_integer(<<EE:96>>, 16),
	<<A:32, B:16, C:16, D:16, E:48>>.

-spec unique_to_token(Unique :: binary()) -> binary().	
unique_to_token(Unique) ->
	base64:encode(Unique).

-spec token_to_unique(Token :: binary()) -> binary().	
token_to_unique(Token) ->
	base64:decode(Token).
	
-spec unique_to_id(Unique :: binary()) -> integer().	
unique_to_id(Unique) ->
	<<Id:128>> = Unique,
	Id.

-spec id_to_unique(Id :: integer()) -> binary().	
id_to_unique(Id) ->
	<<Id:128>>.	

%% ====================================================================
%% Internal functions
%% ====================================================================

to_string([A, B, C, D, E]) ->
	%% xxxxxxxx-xxxx-4xxx-8xxx-xxxxxxxxxxxx
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
