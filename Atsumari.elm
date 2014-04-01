module Atsumari where

import Window
import Mouse
import Maybe
import Set
import Dict

import HexGrid


data Cell = Empty | Filled | Anchor | Numbered Int

hammerhead : HexGrid.HexGrid Cell
hammerhead = HexGrid.rectangularHexGrid (4, 7) Empty |>
    HexGrid.insertIfPossible (-2, -3) (Numbered 1)   |>
    HexGrid.insertIfPossible ( 0, -1) (Numbered 3)   |>
    HexGrid.insertIfPossible (-2,  0) (Numbered 3)   |>
    HexGrid.insertIfPossible (-4,  3) (Numbered 4)   |>
    HexGrid.insertIfPossible (-2,  3) (Numbered 3)   |>
    HexGrid.insertIfPossible (-1, -3) Anchor         |>
    HexGrid.remove (1, -2) |>
    HexGrid.remove (0, 0)  |>
    HexGrid.remove (-1, 2)

hammerheadWin = HexGrid.Rectangular (4,7) <|
    Dict.fromList [ ((-5,3),Empty), ((-4,1),Empty), ((-4,2),Empty), ((-4,3),Numbered 4)
                  , ((-3,-1),Empty), ((-3,0),Filled), ((-3,1),Filled), ((-3,2),Filled)
                  , ((-3,3),Filled), ((-2,-3),Numbered 1), ((-2,-2),Filled), ((-2,-1),Empty)
                  , ((-2,0),Numbered 3), ((-2,1),Filled), ((-2,2),Empty), ((-2,3),Numbered 3)
                  , ((-1,-3),Anchor), ((-1,-2),Filled), ((-1,-1),Filled), ((-1,0),Filled)
                  , ((-1,1),Empty), ((0,-3),Filled), ((0,-2),Empty), ((0,-1),Numbered 3)
                  , ((1,-3),Empty)
                  ]

styler cell s =
    case cell of
        Empty      -> scale 0.8 . rotate 30 . filled green  . ngon 6 <| s
        Filled     -> scale 0.8 . rotate 30 . filled purple . ngon 6 <| s
        Anchor     -> scale 0.6 . rotate 30 . filled purple . circle <| s
        Numbered n -> group [scale 0.8 . rotate 30 . filled green  . ngon 6 <| s, toForm . text <| show n]

text = centered . bold . Text.height 25 . toText

overlayStyler cell s = if cell then rotate 30 . filled black . ngon 6 <| s else toForm empty

frames = fps 30

hexSize = (\h -> (2 * (toFloat h))/(3 * (toFloat 7) + 1)) <~ Window.height
mousePos = (\(w, h) (x, y) -> (x - (w `div` 2), y - (h `div` 2)))
    <~ Window.dimensions ~ Mouse.position
hovered = (\hexSize pos ->
    HexGrid.pixelToHexCoord hexSize pos)
    <~ hexSize ~ mousePos
lastClicked = sampleOn Mouse.clicks hovered

--contiguousBlock : HexGrid.HexCoord -> HexGrid.HexGrid Cell -> [HexGrid.HexCoord]
contiguousBlock coord grid =
    case HexGrid.valueAt coord grid of
        Just Empty -> []
        Just (Numbered _) -> []
        Nothing -> []
        _ -> let inner coord' grid' coords =
                let neighbors = filter (\c -> HexGrid.valueAt c grid' == Just Filled
                                           || HexGrid.valueAt c grid' == Just Anchor)
                                       <| HexGrid.neighbors coord'
                    coords' = foldl Set.insert coords neighbors
                in  if | coords == coords' -> coords
                       | otherwise         -> foldl (\c cs -> inner c grid' cs) coords' neighbors
                 coords = Set.toList . inner coord grid . Set.fromList <| [coord]
                 values = Maybe.justs . map (\c -> id HexGrid.valueAt c grid) <| coords
             in zip coords values

anchored : HexGrid.HexCoord -> HexGrid.HexGrid Cell -> Bool
anchored coord grid =
    let block = map snd <| contiguousBlock coord grid
    in  any (\x -> x == Anchor) block

orphaned : HexGrid.HexCoord -> HexGrid.HexGrid Cell -> Bool
orphaned coord grid =
    let neighbors = map (\c -> (c, HexGrid.valueAt c grid)) <| HexGrid.neighbors coord
    in  if | HexGrid.valueAt coord grid /= Just Filled -> False
           | any (\v -> v == Just Filled || v == Just Anchor) (map snd neighbors) -> False
           | otherwise -> True

surrounded : HexGrid.HexCoord -> HexGrid.HexGrid Cell -> Bool
surrounded coord grid =
    let neighbors = map (flip HexGrid.valueAt <| grid) <| HexGrid.neighbors coord
    in  if all (\v -> v == Just Filled || v == Just Anchor) neighbors
        then True
        else False

valid : HexGrid.HexCoord -> HexGrid.HexGrid Cell -> Bool
valid coord grid =
    let grid' = case HexGrid.valueAt coord grid of
                    Just Empty  -> HexGrid.insertIfPossible coord Filled grid
                    Just Filled -> HexGrid.insertIfPossible coord Empty  grid
                    _           -> grid
        neighbors = filter (\c -> HexGrid.valueAt c grid == Just Filled) <| HexGrid.neighbors coord
    in if | orphaned coord grid'   -> False
          | surrounded coord grid -> False
          | any (\c -> orphaned   c grid') neighbors -> False
          | any (\c -> surrounded c grid') neighbors -> False
          | any (\c -> not . anchored c <| grid') neighbors -> False
          | otherwise             -> True

step : HexGrid.HexCoord -> HexGrid.HexGrid Cell -> HexGrid.HexGrid Cell
step coord grid = if not <| valid coord grid then grid else
    case HexGrid.valueAt coord grid of
        Just Empty  -> HexGrid.insertIfPossible coord Filled grid
        Just Filled -> HexGrid.insertIfPossible coord Empty  grid
        _ -> grid

grid = foldp step hammerhead lastClicked

scene (w, h) hexSize grid =
    let won = HexGrid.gridEqual grid hammerheadWin
        wonText = if won then centered . bold . Text.height 50 . toText <| "you win!" else empty
    in layers [ container w h middle <| HexGrid.showHexGrid hexSize styler grid
              , container w h middle wonText
              ]

main = scene <~ Window.dimensions ~ hexSize ~ grid
