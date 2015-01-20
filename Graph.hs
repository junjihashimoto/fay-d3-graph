{-# LANGUAGE EmptyDataDecls #-}

module Graph where

import FFI

data D3
data SVG
data Scale
data Axis
data DataObject

type Padding = Int
type Width = Int
type Height = Int

data Dataset = Dataset {
  xKey :: String
, yKey :: String
, dObj :: DataObject
}

data Position = PLeft | PTop | PBottom | PRight

pStr :: Position -> String
pStr p =
  case p of
    PLeft -> "left"
    PTop -> "top"
    PBottom -> "bottom"
    PRight -> "right"

data Graph = Graph {
  gObj :: SVG
, gWidth :: Width
, gHeight :: Height
, gDataset :: Dataset
, gPadding :: Padding
, gXScale :: Scale
, gYScale :: Scale
, gXAxis :: Axis
, gYAxis :: Axis
}
  
d3 :: Fay D3
d3 = ffi "d3"

svg :: Int -> Int -> Fay SVG
svg = ffi "d3.select('body').append('svg').attr('width', %1).attr('height', %2)"

svg' :: Fay SVG
svg' = ffi "d3.select(\"body\").append(\"svg\")"

genXScale :: Int -> Padding -> DataObject -> String -> Fay Scale
genXScale = ffi "d3.scale.linear()\
                \.domain([d3.min(%3, function(d) { return d[%4]; }),\
                          \d3.max(%3, function(d) { return d[%4]; })])\
                \.range([%2, %1 - (%2 * 2)])"

genYScale :: Int -> Padding -> DataObject -> String -> Fay Scale
genYScale = ffi "d3.scale.linear()\
                \.domain([d3.min(%3, function(d) { return d[%4]; }),\
                          \d3.max(%3, function(d) { return d[%4]; })])\
                \.range([%1-%2, %2])"


withCSV :: String -> String -> String -> (Dataset -> Fay ()) -> Fay ()
withCSV url x y action = do
  withCSV' url $ \obj -> do
    action $ Dataset x y obj
  where
    withCSV' :: String -> (DataObject -> Fay ()) -> Fay ()
    withCSV' = ffi "d3.csv(%1, function(error, data) { %2(data)})"

genAxis :: Scale -> Position -> Fay Axis
genAxis s p = f s (pStr p)
  where
    f :: Scale -> String -> Fay Axis
    f = ffi "d3.svg.axis().scale(%1).orient(%2).ticks(5)"

graph :: Int -> Int -> Dataset -> Padding -> Fay Graph
graph w h d p = do
  s <- svg w h
  xs <- genXScale w p (dObj d) (xKey d)
  ys <- genYScale h p (dObj d) (yKey d)
  xa <- genAxis xs PBottom
  ya <- genAxis ys PLeft
  return $ Graph s w h d p xs ys xa ya

appendPoint :: Graph -> Fay ()
appendPoint g = f (gObj g) (gXScale g) (xKey (gDataset g)) (gYScale g) (yKey (gDataset g)) (dObj (gDataset g))
  where
    f :: SVG -> Scale -> String -> Scale -> String -> DataObject -> Fay ()
    f = ffi "%1.selectAll(\"circle\")\
        \.data(%6)\
        \.enter()\
        \.append(\"circle\")\
        \.attr(\"cx\", function(d) {\
        \   return (%2(d[%3]));\
        \})\
        \.attr(\"cy\", function(d) {\
        \   return (%4(d[%5]));\
        \})\
        \.attr(\"r\", function(d) {\
        \   return 3;\
        \})"

appendLine :: Graph -> Fay ()
appendLine g = f (gObj g) (gXScale g) (xKey (gDataset g)) (gYScale g) (yKey (gDataset g)) (dObj (gDataset g))
  where
    f :: SVG -> Scale -> String -> Scale -> String -> DataObject -> Fay ()
    f = ffi "%1.append(\"path\")\
        \.attr('stroke', 'red')\
        \.attr('stroke-width', '1')\
        \.attr('fill', 'transparent')\
        \.attr(\"d\",d3.svg.line().x(function(d) { return (%2(d[%3]))}).y(function(d) { return (%4(d[%5]))})(%6))"

appendAxis :: Graph -> Fay ()
appendAxis g = do
  fx (gObj g) (gHeight g) (gPadding g) (gXAxis g)
  fy (gObj g) (gHeight g) (gPadding g) (gYAxis g)
  where
    fx :: SVG -> Height -> Padding -> Axis -> Fay ()
    fx = ffi "\
        \%1.append(\"g\")\
        \      .attr(\"class\", \"axis\")\
        \      .attr(\"transform\", \"translate(0,\" + (%2 - %3) + \")\")\
        \      .call(%4)"
    fy :: SVG -> Height -> Padding -> Axis -> Fay ()
    fy = ffi "\
        \%1.append(\"g\")\
        \      .attr(\"class\", \"axis\")\
        \      .attr(\"transform\", \"translate(\" + %3 + \",0)\")\
        \      .call(%4)"

appendLabel :: Graph -> String -> String -> Fay ()
appendLabel g xlabel ylabel = do
  lx (gObj g) xlabel ((gWidth g)`div`2) ((gHeight g) - 3)
  ly (gObj g) ylabel ((gHeight g)`div`2) (-3)
  where
    font = ".attr(\"font-family\",\"sans-serif\").attr(\"font-size\",\"11px\")"
    lx :: SVG -> String -> Int -> Int -> Fay ()
    lx = ffi "%1.append(\"text\").text(%2).attr(\"x\",%3).attr(\"y\",%4)"
    ly :: SVG -> String -> Int -> Int -> Fay ()
    ly = ffi "%1.append(\"text\").text(%2).attr(\"transform\",\"rotate (90)\").attr(\"x\",%3).attr(\"y\",%4)"
               
ready :: Fay () -> Fay ()
ready = ffi "window.addEventListener(\"load\", %1)"

alert :: a -> Fay ()
alert = ffi "alert(%1)" 

