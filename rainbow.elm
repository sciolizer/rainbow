import Keyboard
import Window
import Mouse
import Graphics.Input as Input
import Debug
import Dict
import List
import Set

main = lift3 scene Window.dimensions Mouse.position state

type State = { selectedColor: String, grid: Dict.Dict (Int,Int) Cell }

type Cell = { colorName : String, color : Color, inBound : Int, outBound : Int }

state : Signal State
state = foldp accum {selectedColor="white",grid=Dict.empty} (lift4 (,,,) Mouse.isDown Mouse.position (fps 10) (lift Set.fromList Keyboard.keysDown))

hotKeys = Dict.fromList [
  (48, "white"),
  (49, "red"),
  (50, "orange"),
  (51, "yellow"),
  (52, "green"),
  (53, "blue"),
  (54, "purple"),
  (82, "red"),
  (79, "orange"),
  (89, "yellow"),
  (71, "green"),
  (66, "blue"),
  (86, "purple"), -- v
  (80, "purple"), -- p
  (87, "white")
  ]

hotKeyNums = Set.fromList (Dict.keys hotKeys)
  
accum (d,(x,y),step,kys) pre = 
  let prev = { pre - grid | grid = fluid step pre.grid }
      headerRowClick = 
        y < blockHeight &&
      x < blockWidth * (length rainbow)
  in { selectedColor =
    if d && headerRowClick 
      then last (take (1 + (x `div` blockWidth)) rainbow) else 
      let relevantKeys = Set.toList (Set.intersect hotKeyNums kys) in
      if List.isEmpty relevantKeys then prev.selectedColor else  Dict.getOrFail (List.head relevantKeys) hotKeys,
    grid =
     if not d || headerRowClick then prev.grid else
     let key = (x `div` blockWidth, (y - blockHeight) `div` blockHeight)
         out = (if prev.selectedColor == "white" then Dict.remove key else Dict.insert key
           {colorName = prev.selectedColor, color = untext prev.selectedColor, inBound = 0, outBound = 0} ) prev.grid in
      out}

fluid : Float -> Dict.Dict (Int,Int) Cell -> Dict.Dict (Int,Int) Cell
fluid ste original =
  let step = round ste
      adjust (x,y) ddddc =
        case Dict.get (x,y) ddddc of
          Nothing -> Debug.log "impossible" ddddc -- this should never happen
          Just cel ->
            let cell = if cel.colorName == "red"
                         then {cel - outBound | outBound = cel.outBound + step }
                         else if cel.inBound >= 10 then 
                         let nearlyDelta = min (round (toFloat step*2.5)) cel.inBound
                             delta = nearlyDelta - (nearlyDelta `mod` 10)
                         in {cel | inBound <- cel.inBound - delta, outBound <- cel.outBound + (delta `div` 10) } else cel
                dc = Dict.insert (x,y) cell ddddc
            in
            case subsequentColor cell.colorName of
              Nothing -> dc
              Just target ->
                let mv (dx,dy) ddc =
                      let newCurrent = Dict.getOrFail (x,y) ddc in
                      if newCurrent.outBound <= 0 then ddc else
                      case Dict.get (x+dx,y+dy) ddc of
                        Nothing -> ddc
                        Just c2 ->
                          if not (c2.colorName == target) then ddc else
                          let delta = min step newCurrent.outBound in
                          Dict.insert (x,y) {newCurrent |
                            outBound <- newCurrent.outBound - delta } (Dict.insert (x+dx,y+dy)
                            {c2 | inBound <- c2.inBound + delta} ddc)
                in List.foldl mv dc [(-1,0),(1,0),(0,-1),(0,1)]
  in List.foldl adjust original (Dict.keys original)

subsequentColor x =
  case x of
    "red" -> Just "orange"
    "orange" -> Just "yellow"
    "yellow" -> Just "green"
    "green" -> Just "blue"
    "blue" -> Just "purple"
    otherwise -> Nothing

scene (w,h) (x,y) sel = flow down
  [  header sel.selectedColor,
  flow outward ((renderGrid (w,h) sel) ++ [
  
 container w h
  (topLeftAt 
    (absolute (x - (x `mod` blockWidth)))
    (absolute (y - headerHeight - (y `mod` blockHeight)))) <| collage blockWidth blockHeight
  [outlined (dotted clearGrey) (rect blockWidth blockHeight)]])]

renderGrid : (Int,Int) -> State -> [Element]
renderGrid (w,h) sel = map (draw (w,h)) (Dict.toList sel.grid)

draw : (Int,Int) -> ((Int,Int),Cell) -> Element
draw (w,h) ((x,y),cell) = container w h
  (topLeftAt
    (absolute (x * blockWidth))
    (absolute (y * blockHeight))) <| collage blockWidth blockHeight
  (formOf cell)
  
formOf : Cell -> [Form]
formOf c = [ 
  filled (untext c.colorName) (rect blockWidth blockHeight),
  toForm (plainText (if c.colorName == "red" then show c.outBound else show c.inBound ++ " -> " ++ show c.outBound))]

untext clr =
  case clr of
    "red" -> red
    "orange" -> orange
    "yellow" -> yellow
    "green" -> green
    "blue" -> blue
    "purple" -> purple
    "white" -> white

header sel = flow right [
  headerButton sel red "red",
  headerButton sel orange "orange",
  headerButton sel yellow "yellow",
  headerButton sel green "green",
  headerButton sel blue "blue",
  headerButton sel purple "purple"]
  
rainbow = ["red", "orange", "yellow", "green", "blue", "purple", 
  "white", 
  "white", 
  "white", 
  "white", 
  "white", 
  "white", 
  "white", 
  "white", 
  "white", 
  "white", 
  "white", 
  "white", 
  "white", 
  "white", 
  "white", 
  "white", 
  "white", 
  "white", 
  "white"]

clearGrey : Color
clearGrey = rgba 111 111 111 0.6

headerHeight = blockHeight

headerButton sel color label = container blockWidth blockHeight middle 
  (collage blockWidth blockHeight [filled color (rect blockWidth blockHeight),
  toForm (plainText (if sel == label then String.toUpper label else label))])

blockWidth = 100
blockHeight = 50
