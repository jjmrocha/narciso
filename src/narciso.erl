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

-define(UUID_VERSION, 2#0100).
-define(UUID_VARIANT, 2#10).

%% ====================================================================
%% API
%% ====================================================================
-export([unique/0, uuid/0, token/0, id/0]).
-export([uuid/1, token/1, id/1]).
-export([uuid_to_unique/1, token_to_unique/1, id_to_unique/1]).
-export([is_unique/1, is_uuid/1, is_token/1, is_id/1]).

%% ====================================================================
%% Generation functions
%% ====================================================================
-spec unique() -> binary().
unique() ->
	<<AB:48, _:4, C:12, _:2, DE:62>> = crypto:rand_bytes(16),
	<<AB:48, ?UUID_VERSION:4, C:12, ?UUID_VARIANT:2, DE:62>>.

-spec uuid() -> binary().
uuid() -> uuid(unique()).

-spec token() -> binary().
token() -> token(unique()).

-spec id() -> integer().
id() -> id(unique()).

%% ====================================================================
%% Generation of variants using unique
%% ====================================================================
-spec uuid(Unique :: binary()) -> binary().	
uuid(<<A:32, B:16, C:16, D:16, E:48>>) ->
	UUID = io_lib:format("~8.16.0b-~4.16.0b-~4.16.0b-~4.16.0b-~12.16.0b", [A, B, C, D, E]),
	list_to_binary(UUID).

-spec token(Unique :: binary()) -> binary().	
token(<<Id:128>>) -> integer_to_binary(Id, 36).

-spec id(Unique :: binary()) -> integer().	
id(<<Id:128>>) -> Id.

%% ====================================================================
%% Parsing from variants to unique
%% ====================================================================
-spec uuid_to_unique(Uuid :: binary()) -> binary().	
uuid_to_unique(<<AA:64, _:8, BB:32, _:8, CC:32, _:8, DD:32, _:8, EE:96>>) ->
	Hex = <<AA:64, BB:32, CC:32, DD:32, EE:96>>,
	Id = binary_to_integer(Hex, 16),
	<<Id:128>>.

-spec token_to_unique(Token :: binary()) -> binary().	
token_to_unique(Token) -> 
	Id = binary_to_integer(Token, 36),
	<<Id:128>>.

-spec id_to_unique(Id :: integer()) -> binary().	
id_to_unique(Id) -> <<Id:128>>.	

%% ====================================================================
%% Validation functions
%% ====================================================================
-spec is_unique(Unique :: binary()) -> boolean().	
is_unique(<<_:48, Version:4, _:12, Variant:2, _:62>>) -> Version =:= ?UUID_VERSION andalso Variant =:= ?UUID_VARIANT;
is_unique(_) -> false.

-spec is_uuid(UUID :: binary()) -> boolean().
is_uuid(UUID = <<_:64, $-, _:32, $-, _:32, $-, _:32, $-, _:96>>) ->
	case re:run(UUID, <<"[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[0-9a-f]{4}-[0-9a-f]{12}">>) of
		{match, _} -> 
			Unique = uuid_to_unique(UUID),
			is_unique(Unique);
		_ -> false
	end;
is_uuid(_) -> false.

-spec is_token(Token :: binary()) -> boolean().
is_token(Token) when is_binary(Token) ->
	case re:run(Token, <<"^([0-9A-Z]+)$">>) of
		{match, _} -> 
			Unique = token_to_unique(Token),
			is_unique(Unique);
		_ -> false
	end;
is_token(_) -> false.

-spec is_id(Id :: integer()) -> boolean().
is_id(Id) when is_integer(Id) andalso Id =< 16#ffffffffffffffffffffffffffffffff -> 
	Unique = id_to_unique(Id),
	is_unique(Unique);
is_id(_) -> false.
