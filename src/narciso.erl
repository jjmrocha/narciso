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
-export([unique/0, uuid/0, token/0, id/0]).
-export([uuid/1, token/1, id/1]).
-export([uuid_to_unique/1, token_to_unique/1, id_to_unique/1]).
-export([is_unique/1, is_uuid/1, is_token/1, is_id/1]).

-spec unique() -> binary().
unique() ->
	[A, B, C, D, E] = v4(),
	<<A:32, B:16, C:16, D:16, E:48>>.

-spec uuid() -> binary().
uuid() -> uuid(v4()).

-spec token() -> binary().
token() -> token(unique()).

-spec id() -> integer().
id() -> id(unique()).

-spec uuid(Unique :: binary()) -> binary().	
uuid(<<A:32, B:16, C:16, D:16, E:48>>) -> uuid([A, B, C, D, E]);
uuid([A, B, C, D, E]) ->
	UUID = io_lib:format("~8.16.0b-~4.16.0b-~4.16.0b-~4.16.0b-~12.16.0b", [A, B, C, D, E]),
	list_to_binary(UUID).

-spec token(Unique :: binary()) -> binary().	
token(<<Id:128>>) -> 
	Base36 = io_lib:format("~.36.0b", [Id]),
	list_to_binary(Base36).	

-spec id(Unique :: binary()) -> integer().	
id(<<Id:128>>) -> Id.

-spec uuid_to_unique(Uuid :: binary()) -> binary().	
uuid_to_unique(<<AA:64, _:8, BB:32, _:8, CC:32, _:8, DD:32, _:8, EE:96>>) ->
	A = binary_to_integer(<<AA:64>>, 16),
	B = binary_to_integer(<<BB:32>>, 16),
	C = binary_to_integer(<<CC:32>>, 16),
	D = binary_to_integer(<<DD:32>>, 16),
	E = binary_to_integer(<<EE:96>>, 16),
	<<A:32, B:16, C:16, D:16, E:48>>.

-spec token_to_unique(Token :: binary()) -> binary().	
token_to_unique(Token) -> 
	Id = binary_to_integer(Token, 36),
	<<Id:128>>.

-spec id_to_unique(Id :: integer()) -> binary().	
id_to_unique(Id) -> <<Id:128>>.	

-spec is_unique(Unique :: binary()) -> boolean().	
is_unique(<<_:48, C:16, D:16, _:48>>) ->
	between(C, 16#4000, 16#4fff) andalso between(D, 16#8000, 16#8fff);
is_unique(_) -> false.

-spec is_uuid(UUID :: binary()) -> boolean().
is_uuid(UUID = <<_:64, $-, _:32, $-, _:32, $-, _:32, $-, _:96>>) ->
	case re:run(UUID, <<"[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-8[0-9a-f]{3}-[0-9a-f]{12}">>) of
		{match, _} -> true;
		_ -> false
	end;
is_uuid(_) -> false.

-spec is_token(Token :: binary()) -> boolean().
is_token(Token) when is_binary(Token) ->
	case re:run(Token, <<"^([0-9a-z]+)$">>) of
		{match, _} -> 
			Id = binary_to_integer(Token, 36),
			is_id(Id);
		_ -> false
	end;
is_token(_) -> false.

-spec is_id(Id :: integer()) -> boolean().
is_id(Id) when is_integer(Id) andalso Id =< 16#ffffffffffffffffffffffffffffffff -> is_unique(<<Id:128>>);
is_id(_) -> false.

%% ====================================================================
%% Internal functions
%% ====================================================================
v4() ->
	Time = erlang:phash2(utc_date(), 16#100000000),
	Host = erlang:phash2(host_process(), 16#10000),
	Rand1 = crypto:rand_uniform(16#4000, 16#5000),
	Rand2 = crypto:rand_uniform(16#8000, 16#9000),
	Rand3 = crypto:rand_uniform(0, 16#1000000000000),
	[Time, Host, Rand1, Rand2, Rand3].

utc_date() ->
	TS = {_,_, Micro} = os:timestamp(),
	{Date, Time} = calendar:now_to_universal_time(TS),
	{Date, Time, Micro}.

host_process() ->
	{node(), self()}.

between(Value, Lo, Hi) when Value >= Lo andalso Value =< Hi -> true;
between(_, _, _) -> false.