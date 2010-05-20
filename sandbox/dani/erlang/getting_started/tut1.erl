%% nome del modulo,
%% deve avere stesso del file.
-module(tut1).

%% funzione del modulo,
%% nome e numero e numero di parametri.
-export([double/1]).
-export([fac/1, mul/2]).
-export([convert/1]).
-export([list_length/1]).
-export([format_temps/1]).
-export([list_max/1]).
-export([reverse/1]).
-export([test_if/2]).

double(X) ->
    2*X.

fac(1) ->
    1;

fac(N) ->
    N * fac(N-1).

mul(X, Y) ->
    X*Y.

%% variabili devono essere sempre
%% con la lettera maiuscola, non è
%% linguaggio tipizzato

%% atomi sono con lettera minuscola,
%% non sono associati ad alcun valore

convert(Length) ->
    case Length of
        
	{centimeter, X} ->
            {inch, X / 2.54};
        {inch, Y} ->
            {centimeter, Y * 2.54}
    
    end.

%% tuple raccolgono dati insieme (struct)
%% liste elencano dati (collection)

list_length([]) ->
    0;
list_length([ _ | Rest ]) ->
    1 + list_length(Rest).

%% creazione lista in modo dinamico,
%% valore ritornato è creato con il pipe.

format_temps(List_of_cities) ->
    Converted_list = convert_list_to_c(List_of_cities),
    Sorted_list = lists:sort( fun( {_, {c, Temp1}},
		     {_, {c, Temp2}}) -> Temp1 < Temp2
		end,
		Converted_list),
    print_temp(Sorted_list),
    {Max_city, Min_city} = find_max_and_min(Converted_list),
    print_max_and_min(Max_city, Min_city).
convert_list_to_c([{Name, {f, F}} | Rest]) ->
    Converted_City = {Name, {c, (F -32)* 5 / 9}},
    [Converted_City | convert_list_to_c(Rest)];
convert_list_to_c([City | Rest]) ->
    [City | convert_list_to_c(Rest)];
convert_list_to_c([]) ->
    [].


print_temp([{Name, {c, Temp}} | Rest]) ->
    io:format("~-15w ~w c~n", [Name, Temp]),
    print_temp(Rest);
print_temp([]) ->
    ok.

find_max_and_min([ First | Rest ]) ->
    find_max_and_min(Rest, First, First).

find_max_and_min([], Max, Min) ->
    { Max, Min };

find_max_and_min([ {City, {c, Temp} } | Rest ],
		 {Max_name, {c, Temp_max} } ,
		 {Min_name, {c, Temp_min} }) ->
    
    if
	Temp > Temp_max ->
	    Max_new = {City, {c, Temp}};
	%% alla fine esegui questo codice
	%% ultima condizione non vuole ;
	true ->
	    Max_new = {Max_name, {c, Temp_max}}
    end,

    if
	Temp < Temp_min ->
	    Min_new = {City, {c, Temp}};
	%% alla fine esegui questo codice
	%% ultima condizione non vuole ;
	true ->
	    Min_new = {Min_name, {c, Temp_min}}
    end,
    
    find_max_and_min(Rest, Max_new, Min_new).

print_max_and_min({Max_name, {c, Max_temp}}, {Min_name, {c, Min_temp}}) ->
    io:format("MAX is ~w with ~w deegres.~n", [Max_name, Max_temp]),
    io:format("MIN is ~w with ~w deegres.~n", [Min_name, Min_temp]).

%% sort function


%% end format_temps

list_max([ Head | Rest ]) ->
    list_max(Rest, Head).
list_max([], Res) ->
    Res;
list_max([ First | Rest ], Res) when First > Res ->
    list_max(Rest, First);
list_max([ _ | Rest ], Res) ->
    list_max(Rest, Res).




%% per usare l'operatore di creazione lista pipe
%% devo creare oggetto lista vuota, da passare 
%% alla funzione reverse con due parametri
reverse(List) ->
    reverse(List, []).
reverse([], Res) ->
    Res;
reverse([Head | Rest], Res) ->
    reverse(Rest, [ Head | Res ]).


%% test IF
test_if(A, B) ->
    if
	A == 5 ->
	    io:format("A == 5~n", []);
	B == 6 ->
	    io:format("B == 6~n", []);
	A == 2, B == 3 ->
	    io:format("A == 2, B == 3~n", []);
	true ->
	    io:format("no case matched.~n")
    end.
