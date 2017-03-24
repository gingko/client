module TreeSort exposing (sortTrees)

import List.Extra as ListExtra
import Types exposing (..)

sortTrees : List Tree -> List Tree
sortTrees trees =
  trees
    |> toDag
    |> linearizeDag
    |> List.filterMap (\cid -> ListExtra.find (\t -> t.uid == cid) trees)


type alias DagEntry =
  { uid: String
  , prev: List String
  , next: List String
  }


toDag : List Tree -> List DagEntry
toDag posets =
  let
    getPrev : Tree -> List String
    getPrev pos =
      let
        prevId =
          case pos.prev of
            Nothing -> []
            Just puid -> [puid]
      in
        posets
          |> List.filter (\p -> p.next == Just pos.uid) -- List Tree
          |> List.map .uid -- List String
          |> List.append prevId
          |> ListExtra.unique -- List String

    getNext : Tree -> List String
    getNext pos =
      let
        nextId =
          case pos.next of
            Nothing -> []
            Just puid -> [puid]
      in
        posets
          |> List.filter (\p -> p.prev == Just pos.uid) -- List Tree
          |> List.map .uid -- List String
          |> List.append nextId
          |> ListExtra.unique -- List String

    f : Tree -> DagEntry
    f p = DagEntry p.uid (getPrev p) (getNext p)
  in
    posets -- List { uid: String, prev: Maybe String, next: Maybe String }
      |> List.map f


sortFunction : List DagEntry -> DagEntry -> DagEntry -> Order
sortFunction dag a b =
  let
    distA = (maxDist dag "end" a.uid)
    distB = (maxDist dag "end" b.uid)
  in
    if distA == distB then
      compare a.uid b.uid
    else
      compare distB distA


maxDist : List DagEntry -> String -> String -> Int
maxDist dag toId fromId =
  if toId == fromId then
    0
  else
    let
      maxNexts =
        dag -- List DagEntry
          |> ListExtra.find (\de -> de.uid == fromId) -- Maybe DagEntry
          |> Maybe.withDefault (DagEntry "end" ["start"] []) -- DagEntry
          |> .next
          |> List.map (maxDist dag toId)
          |> List.maximum
          |> Maybe.withDefault 0
    in
      1 + maxNexts


linearizeDag : List DagEntry -> List String
linearizeDag dag =
  dag -- List DagEntry
    |> List.sortWith (sortFunction dag) -- List DagEntry
    |> List.map .uid




