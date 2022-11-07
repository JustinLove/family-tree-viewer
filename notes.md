- raw log update
  - x import layer
  - x fetch logs
  - x display results
  - x apply search?
  - x lineage query
  - x select a result
  - x query from search link
  - x server list
    - x hardcoded bigserver
    - x server name param
      - x query from ohol link
- x graphviz render
- ui update
  - time
  - server
  - update search on typing
- elm render
  https://package.elm-lang.org/packages/shamansir/elm-graph-render/
  https://package.elm-lang.org/packages/goyalarchit/elm-dagre/
- extended graph
  - killer edges
  - killer nodes
  - ruby etc
- life mapping for render?
- possible cloudfront to get life data
  - need servers.json

## test cases
 - 813 #server_name=bigserver2.onehouronelife.com&start_time=1581642490&end_time=1581649690&playerid=2692836
 - 2xx #server_name=bigserver2.onehouronelife.com&start_time=1667316147&end_time=1667323347&playerid=5590635

- ParseLive concerns
  - Life, Parent in OHOLData
  - deadEndsToString in ParseMap
- LifeDataLayer concerns
  - population, clusters
  - OHOLData Life
  - event range was coming from population

- nicer loading indicator
- styling
- dedicated endpoints
- well known players?
