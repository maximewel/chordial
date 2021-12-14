-module(storing_interface).

-export([main/0, loop/1]).
%% This process is simply a storing interface to show that the DHT is for indexing, and does not implement a store per se. 
%% This is a trivial way of storing in whatever comes in the process memory 
%% Made to be accessed by a proxy DHT node

main() -> 
    io:format("Storing process is getting ready !~n"),
    %Start with empty list
    loop([]).

loop(StorageList) ->
    io:format("Looping with store: ~p~n", [StorageList]),
    receive
        {store, Source, {Key, Value}} -> 
            io:format("Storing ~p~n", [{Key, Value}]),
            AppendedList = StorageList ++ [{Key, Value}],
            Source ! {store, success},
            loop(AppendedList);
        {lookup, Source, Key} ->
            io:format("Looking up ~p~n", [Key]),
            Lookups = find_lookups(StorageList, Key),
            Source ! {lookup, Lookups},
            loop(StorageList);
        {data_dump, Source} ->
            Source ! {data_dump_result, StorageList},
            loop(StorageList);
        {delete, Source, Key} ->
            NewStorage = delete(Source, Key, StorageList),
            loop(NewStorage)
    end.

find_lookups(StorageList, LoookupKey) ->
    find_lookups(StorageList, LoookupKey, []).

find_lookups([], _, Matches) -> Matches;
find_lookups([{Key, Value} | StorageList], LoookupKey, Matches) when Key == LoookupKey-> 
    find_lookups(StorageList, LoookupKey, Matches ++ [{Key, Value}]);
find_lookups([{Key, Value} | StorageList], LoookupKey, Matches) -> 
    find_lookups(StorageList, LoookupKey, Matches).

delete(Source, Key, StorageList) ->
    case lists:keytake(Key, 1, StorageList) of
        {value, DeletedTuple, NewStorage} ->
            delete(Source, Key, NewStorage); %Keep deleting to ensure every entry with this key is deleted
        false -> 
            Source ! {delete, success}
    end,
    StorageList.