-module(sssp_cpu).

-compile(export_all).

update_sets_worker({none, none}, _Ds, _MinDist, Sets) ->
    Sets;
update_sets_worker({none, It2}, DistsFromInd, MinDist, {NewWS, NewRS}) ->
    {Ind, NewIt2} = It2,
    D = lists:nth(Ind, DistsFromInd),
    {NewestWS, NewestRS} = case D of 
				 -1 -> {NewWS, NewRS};
				 _ -> {gb_sets:add({D+MinDist, Ind}, NewWS),
				       gb_sets:delete(Ind, NewRS)}
			     end,
    update_sets_worker({none, gb_sets:next(NewIt2)}, DistsFromInd, MinDist, {NewestWS, NewestRS});
update_sets_worker({It1, It2}, DistsFromInd, MinDist, {NewWS, NewRS}) ->
    {{CurrMinDist, Ind}, NewIt1} = It1,
    D = lists:nth(Ind, DistsFromInd),
    NewestWS = gb_sets:add({min(D+MinDist,CurrMinDist),Ind}, NewWS),
    update_sets_worker({gb_sets:next(NewIt1), It2}, DistsFromInd, MinDist, {NewestWS, NewRS}).

update_sets(OldWS, OldRS, DistsFromInd, MinDist) ->
    It1 = gb_sets:iterator(OldWS),
    It2 = gb_sets:iterator(OldRS),
    NewWS = gb_sets:empty(),
    NewRS = OldRS,
    update_sets_worker({gb_sets:next(It1), gb_sets:next(It2)}, DistsFromInd, MinDist, {NewWS, NewRS}).

collect_results(FinalDs) ->
    gb_sets:to_list(FinalDs).

dijkstra_worker(NrNodes, NrFinal, FinalDs, _WS, _RS, _Ds) when NrNodes == NrFinal ->
    collect_results(FinalDs);
dijkstra_worker(NrNodes, NrFinal, FinalDs, WS, RS, Ds) ->
    {{MinDist, Ind}, UpdatedWS} = gb_sets:take_smallest(WS),
    NewFinalDs = gb_sets:add({Ind, MinDist}, FinalDs),
    DistsFromInd = lists:nth(Ind, Ds),
    {NewWS, NewRS} = update_sets(UpdatedWS, RS, DistsFromInd, MinDist),
    dijkstra_worker(NrNodes, NrFinal+1, NewFinalDs, NewWS, NewRS, Ds).

dijkstra(NrNodes, StartNode, DistMatrix) ->
    WorkingSet = gb_sets:from_list([{0, StartNode}]),
    FinalDists = gb_sets:empty(),
    RemainingSet = gb_sets:from_list(lists:filter(fun(X) -> X =/= StartNode end, lists:seq(1,NrNodes))),
    dijkstra_worker(NrNodes, 0, FinalDists, WorkingSet, RemainingSet, DistMatrix).
	
