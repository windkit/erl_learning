-module(file_sender).

-behavior(leo_ordning_reda_behaviour).

-include_lib("leo_ordning_reda/include/leo_ordning_reda.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(BUFSIZE, 65536).
-define(TIMEOUT, 1000).
-define(RECVPATH, "recv/").

-export([start/0, handle_fail/2, handle_send/3, store/2, add_one_file/2, add_one_folder/2, store_one/1, set_folder/1]).

start() ->
    leo_ordning_reda_api:start(),
    ok.

seperate_file_folder(Folder, List) ->
    seperate_file_folder(Folder, List, [], []).
seperate_file_folder(Folder, [],FileList, FolderList) ->
    {FileList, FolderList};
seperate_file_folder(Folder, [H | T], FileList, FolderList) ->
    Target = filename:join(Folder, H),
    ?debugVal(Target),
    ?debugVal(filelib:is_dir(Target)),
    case filelib:is_dir(Target) of
        false ->
            seperate_file_folder(Folder, T, FileList ++ [Target], FolderList);
        true ->
            seperate_file_folder(Folder, T, FileList, FolderList ++ [Target])
    end.

add_one_folder(Target, Folder) ->
    ok = rpc:call(Target, ?MODULE, set_folder, [Folder]),
    {ok, List} = file:list_dir(Folder),
%    ?debugVal(List),
    {FileList, FolderList} = seperate_file_folder(Folder, List),
    ?debugVal(FileList),
%    ?debugVal({FileList, FolderList}).
    FunF = fun(File) -> add_one_file(Target, File) end,
    FunD = fun(File) -> add_one_folder(Target, File) end,
    lists:map(FunF, FileList),
    lists:map(FunD, FolderList).
%    FunF = fun(File, Acc) -> [add_one_file(Target, File) | Acc] end,
%    FunD = fun(File, Acc) -> [add_one_folder(Target, File) | Acc] end,
%    lists:foldl(FunF, [], FileList).

add_one_file(Target, FileName) ->
    ?debugVal(FileName),
    {ok, FileContent} = file:read_file(FileName),
    FileSize = byte_size(FileContent),
    {ok, Bin} = leo_ordning_reda_api:pack({FileName, FileSize, FileContent}),
    stack_one_file(Target, FileName, Bin).

stack_one_file(Target, FileName, Bin) ->
    io:format("~p~n", [byte_size(Bin)]),
    case leo_ordning_reda_api:stack(Target, FileName, Bin) of
        ok ->
            ok;
        {error, undefined} ->
            leo_ordning_reda_api:add_container(Target, [{module, ?MODULE},
                                                        {buffer_size, ?BUFSIZE},
                                                        {timeout, ?TIMEOUT}]),
            stack_one_file(Target, FileName, Bin)
    end.

handle_fail(Node, StackInfo) ->
%    ?debugVal({Node, length(StackInfo)}),
    ok.

handle_send(Node, StackInfo, CompressedBin) ->
    ?debugVal({Node, length(StackInfo), byte_size(CompressedBin)}),
    Ret = rpc:call(Node, ?MODULE, store, [StackInfo, CompressedBin]),
    ?debugVal(Ret),
    Ret.

store_one({FileName, FileSize, FileContent}) ->
    ?debugVal({FileName, FileSize}),
    filelib:ensure_dir(filename:join(?RECVPATH, FileName)),
    file:write_file(filename:join(?RECVPATH, FileName), FileContent).

%slice_and_store(Node, StackedObjs, Acc) ->
%    case slice(StackedObjs) of 
%        {ok, FileName, FileLength, FileContent, StackedObjsRest} ->
%            Ret = store(FileName, FileLength, FileContent),
%            slice_and_store(Node, StackedObjsRest, [Ret | Acc]);
%        {error, Cause} ->
%            {error, Cause}
%    end.

set_folder(Folder) ->
    filelib:ensure_dir(filename:join(?RECVPATH, Folder)),
    ok.

store(StackInfo, CompressedObjs) ->
    ?debugVal(StackInfo),
    ?debugVal(byte_size(CompressedObjs)),
    ok = leo_ordning_reda_api:unpack(CompressedObjs, fun ?MODULE:store_one/1).
%    case catch lz4:unpack(CompressedObjs) of
%        {ok, OriginalObjects} ->
        %    {ok, byte_size(OriginalObjects)};
        %            case slice_and_store(Node, OriginalObjects, []) of
        %                {ok, RetL} ->
        %                    {ok, RetL};
        %                {error, _Cause} ->
        %                    {error, fail_storing_files}
        %            end;
%        {_, Cause} ->
%            {error, Cause} 
%    end.
