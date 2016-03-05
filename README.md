narciso
=======
*Unique ID generator*

Narciso is a unique ID generator using a 128 random bits.
Narciso provides 4 flavors of ID: UUID, Integer, Token that can be used on URL (base 36 encoding in binary format) and 128 bits binary. 


Installation
------------

Using rebar:

```erlang
{deps, [
	{narciso, ".*", {git, "https://github.com/jjmrocha/narciso.git", "master"}}
]}.
```


Usage
-----

Generation of unique IDs:
```erlang
% 128 bits binary
1> narciso:unique().
<<189,185,221,128,122,46,74,138,182,161,126,32,222,97,156,118>>

% uuid
2> narciso:uuid().
<<"1475bfc9-1b99-44b0-a990-358e1db44d53">>

% token
3> narciso:token().
<<"3z389ykb061d068ux5iy6gbva">>

% 128 bits integer
4> narciso:id().
59486245756825984547942457398141853665
```

128 bits ID in diferent formats:
```erlang
% 128 bits binary
1> Unique = narciso:unique().
<<65,201,173,96,12,171,78,27,175,200,207,151,251,185,70,98>>

% uuid from unique
2> narciso:uuid(Unique).     
<<"41c9ad60-0cab-4e1b-afc8-cf97fbb94662">>

% token from unique
3> narciso:token(Unique).    
<<"3w7nni025418b96lydzqxyb8i">>

% 128 bits integer from unique
4> narciso:id(Unique).       
87446987861270980488766438974793795170
```

Converting diferent formats back to 128 bits unique:
```erlang
% From uuid
1> narciso:uuid_to_unique(<<"41c9ad60-0cab-4e1b-afc8-cf97fbb94662">>).
<<65,201,173,96,12,171,78,27,175,200,207,151,251,185,70,98>>

% From token
2> narciso:token_to_unique(<<"3w7nni025418b96lydzqxyb8i">>).
<<65,201,173,96,12,171,78,27,175,200,207,151,251,185,70,98>>

% From a 128 bits integer
3> narciso:id_to_unique(87446987861270980488766438974793795170).
<<65,201,173,96,12,171,78,27,175,200,207,151,251,185,70,98>>
```

Format validation:
```erlang
% Validation of 128 bits uniques
1> narciso:is_unique(false).
false
2> narciso:is_unique(123).  
false
3> narciso:is_unique(<<"1475bfc9-1b99-44b0-a990-358e1db44d53">>).
false
4> narciso:is_unique(<<123,45,6>>).                              
false
5> narciso:is_unique(<<65,201,173,96,12,171,78,27,175,200,207,151,251,185,70,98>>).
true

% Validation of UUIDs
6> narciso:is_uuid("teste").
false
7> narciso:is_uuid(123).    
false
8> narciso:is_uuid(<<65,201,173,96,12,171,78,27,175,200,207,151,251,185,70,98>>).
false
9> narciso:is_uuid(<<"1475bfc9-1b99-44b0-358e1db44d53">>).                       
false
10> narciso:is_uuid(<<"1475bfc9-1b99-44b0-a990-358e1db44d53">>).
true

% Validation of tokens
11> narciso:is_token("teste").
false
12> narciso:is_token(123).    
false
13> narciso:is_token(<<"1475bfc9-1b99-44b0-358e1db44d53">>).
false
14> narciso:is_token(<<"1475bfc9358e1db44d53">>).           
false
15> narciso:is_token(<<"3w7nni025418b96lydzqxyb8i">>).
true

% Validation of 128 bits integers
16> narciso:is_id("teste").
false
17> narciso:is_id(<<65,201,173,96,12,171,78,27,175,200,207,151,251,185,70,98>>).
false
18> narciso:is_id(123).                                                         
false
19> narciso:is_id(<<"1475bfc9-1b99-44b0-358e1db44d53">>).
false
20> narciso:is_id(87446987861270980488766438974793795170).
true
```
