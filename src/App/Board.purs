module App.Board where

import Prelude
import Camera (Camera, Vec2, defaultCamera, screenPan, toViewBox, zoomOn)
import Data.Array as Arra
import Data.Array as Array
import Data.FunctorWithIndex (mapWithIndex)
import Data.GameMap (GameMap(..), generateMap, maximumMapSize, neighbours)
import Data.Int (floor, odd, toNumber)
import Data.Lens (over)
import Data.Lens.Index (ix)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.MediaType (MediaType(..))
import Data.MouseButton (MouseButton(..), isPressed)
import Data.Set (Set)
import Data.Set as Set
import Data.String (joinWith)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..), uncurry)
import Data.Typelevel.Num (D6, d0, d1, d5)
import Data.Vec (Vec, vec2, (!!))
import Data.Vec as Vec
import Effect.Class (class MonadEffect)
import Halogen (AttrName(..), get, gets, modify_)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Svg.Attributes as SA
import Halogen.Svg.Elements as SE
import Halogen.Svg.Indexed as SI
import Math (cos, pi, sin, sqrt)
import Web.UIEvent.MouseEvent as MouseEvent
import Web.UIEvent.WheelEvent (WheelEvent)
import Web.UIEvent.WheelEvent as WheelEent

type RenderSettings
  = { cellSize :: Number
    , mapPadding :: Vec2 Number
    , stackedPieceOffset :: Number
    , windowSize :: Vec2 Number
    , backgroundUrl :: String
    , backgroundSize :: Vec2 Number
    }

type PiecePosition
  = { x :: Int, y :: Int, index :: Int }

type SelectionState
  = { selectedCell ::
      Maybe
        { position :: Tuple Int Int
        , neighbours :: Set (Tuple Int Int)
        }
    , selectedPiece :: Maybe PiecePosition
    }

type State
  = { lastMousePosition :: Vec2 Number
    , camera :: Camera
    , gameMap :: GameMap
    , settings :: RenderSettings
    , selection :: SelectionState
    }

data Action
  = Initialize
  | HandleMouseDown
  | HandleMouseUp
  | HandleMouseMove MouseEvent.MouseEvent
  | HandleScroll WheelEvent
  | AddPiece (Tuple Int Int)
  | SelectCell (Tuple Int Int)
  | SelectPiece PiecePosition

_gameMap :: SProxy "gameMap"
_gameMap = SProxy

component :: forall q o m. MonadEffect m => H.Component HH.HTML q RenderSettings o m
component =
  H.mkComponent
    { initialState:
      \settings ->
        { camera: defaultCamera (Tuple 0.3 4.0)
        , lastMousePosition: zero
        , gameMap:
          generateMap
            $ ( maximumMapSize settings.cellSize $ (settings.mapPadding <#> (_ * 2.0))
                  + settings.backgroundSize
              )
        , selection:
          { selectedPiece: Nothing
          , selectedCell: Nothing
          }
        , settings
        }
    , render
    , eval:
      H.mkEval
        $ H.defaultEval
            { handleAction = handleAction
            , initialize = Just Initialize
            }
    }

render :: forall m cs. MonadEffect m => State -> H.ComponentHTML Action cs m
render state =
  SE.svg
    [ SA.height $ state.settings.windowSize !! d1
    , SA.width $ state.settings.windowSize !! d0
    , SA.class_ "board"
    , toViewBox state.settings.windowSize state.camera
    , HE.onWheel $ HandleScroll >>> Just
    , HE.onMouseMove $ HandleMouseMove >>> Just
    ]
    [ renderBackgroundMap state.settings
    , renderGameMap state.settings state.selection state.gameMap
    ]

renderGameMap ::
  forall m cs. RenderSettings -> SelectionState -> GameMap -> H.ComponentHTML Action cs m
renderGameMap settings selection (GameMap gameMap) =
  go $ join
    $ flip Array.mapWithIndex gameMap \x inner ->
        flip Array.mapWithIndex inner \y cellData -> renderCell settings selection cellData x y
  where
  go svg = SE.g [] $ (svg <#> _.hexagon) <> pieces
    where
    pieces = join $ _.pieces <$> svg

renderCell ::
  forall m cs.
  RenderSettings ->
  SelectionState ->
  Array { color :: SA.Color } ->
  Int ->
  Int ->
  { pieces :: Array (H.ComponentHTML Action cs m)
  , hexagon :: H.ComponentHTML Action cs m
  }
renderCell settings selection cellData x y = { pieces, hexagon }
  where
  hexagon =
    renderFlatHexagon settings.cellSize (vec2 screenX screenY)
      [ SA.class_ classes
      , HE.onClick
          $ \event ->
              Just
                if MouseEvent.ctrlKey event then
                  AddPiece (Tuple x y)
                else
                  SelectCell (Tuple x y)
      , HE.onMouseMove $ HandleMouseMove >>> Just
      ]

  pieces =
    flip Array.mapWithIndex cellData \index piece ->
      let
        offset = settings.stackedPieceOffset * toNumber index
      in
        SE.rect
          [ SA.x $ (screenX - settings.cellSize / 2.0) + offset
          , SA.y $ (screenY - settings.cellSize / 2.0) - offset
          , SA.height settings.cellSize
          , SA.width settings.cellSize
          , SA.class_ $ "board__piece"
              <> if selection.selectedPiece == Just { x, y, index } then
                  " board__piece--selected"
                else
                  ""
          , SA.fill $ Just piece.color
          , HE.onClick $ const $ Just $ SelectPiece { x, y, index }
          ]

  classes =
    joinWith " " $ append [ "board__hexagon" ] $ fromMaybe [] $ selection.selectedCell
      <#> \selectedCell ->
          if selectedCell.position == Tuple x y then
            [ "board__hexagon--selected" ]
          else
            if Set.member (Tuple x y) selectedCell.neighbours then
              [ "board__hexagon--selection-neighbour" ]
            else
              []

  screenX = settings.cellSize * (1.5 * toNumber x + 1.0)

  screenY = settings.cellSize * sqrt 3.0 * (toNumber y + if odd x then 1.0 else 0.5)

renderBackgroundMap :: forall m cs. RenderSettings -> H.ComponentHTML Action cs m
renderBackgroundMap settings =
  SE.foreignObject
    [ SA.width width
    , SA.height height
    , SA.x $ settings.mapPadding !! d0
    , SA.y $ settings.mapPadding !! d1
    ]
    [ HH.object
        [ HP.type_ (MediaType "image/svg+xml")
        , HP.attr (AttrName "data") ("assets/" <> settings.backgroundUrl)
        , HP.width $ floor width
        , HP.height $ floor height
        ]
        []
    ]
  where
  width = settings.backgroundSize !! d0

  height = settings.backgroundSize !! d1

hexagonCorner :: Number -> Int -> Vec2 Number -> Vec2 Number
hexagonCorner size nth center =
  center
    + vec2
        (size * cos angle)
        (size * sin angle)
  where
  angle = toNumber nth * pi / 3.0

renderFlatHexagon ::
  forall cs m.
  Number ->
  Vec2 Number ->
  Array
    ( HP.IProp
        SI.SVGpath
        Action
    ) ->
  H.ComponentHTML Action cs m
renderFlatHexagon size center props =
  SE.path
    $ Array.snoc props
    $ SA.d
    $ SA.Abs
    <$> Array.snoc commands SA.Z
  where
  commands =
    Vec.toArray
      $ flip mapWithIndex vec \index point ->
          (if index == 0 then SA.M else SA.L) (point !! d0)
            (point !! d1)

  vec :: Vec D6 (Vec2 Number)
  vec = Vec.range d0 d5 <#> \nth -> hexagonCorner size nth center

handleAction :: forall cs o m. MonadEffect m => Action → H.HalogenM State Action cs o m Unit
handleAction = case _ of
  Initialize -> pure unit
  HandleMouseDown -> pure unit
  HandleMouseUp -> pure unit
  HandleMouseMove event -> do
    { lastMousePosition } <- get
    let
      mouseButtonState = MouseEvent.buttons event

      mousePosition = toNumber <$> vec2 (MouseEvent.clientX event) (MouseEvent.clientY event)

      updateCamera camera
        | isPressed LeftMouseButton mouseButtonState = screenPan (mousePosition - lastMousePosition) camera
        | otherwise = camera
    modify_ \state ->
      state
        { lastMousePosition = mousePosition
        , camera = updateCamera state.camera
        }
  HandleScroll event -> do
    mousePosition <- gets _.lastMousePosition
    let
      delta = WheelEent.deltaY event
    when (delta /= 0.0) do
      modify_ \state ->
        state
          { camera =
            zoomOn mousePosition
              ( if delta < 0.0 then 1.1 else 1.0 / 1.1
              )
              state.camera
          }
  AddPiece (Tuple x y) -> do
    modify_
      $ over (prop _gameMap <<< _Newtype <<< ix x <<< ix y)
      $ flip Arra.snoc { color: SA.RGB 124 255 115 }
  SelectCell position -> do
    modify_
      $ over (prop _selection <<< prop _selectedCell) \selectedCell ->
          if (selectedCell <#> _.position) == Just position then
            Nothing
          else
            Just
              { position
              , neighbours:
                Set.fromFoldable $ map (\v -> Tuple (v !! d0) (v !! d1))
                  $ neighbours
                  $ uncurry vec2 position
              }
  SelectPiece position -> do
    modify_
      $ over (prop _selection <<< prop _selectedPiece) \oldSelection ->
          if oldSelection == Just position then Nothing else Just position

_selectedCell :: SProxy "selectedCell"
_selectedCell = SProxy

_selectedPiece :: SProxy "selectedPiece"
_selectedPiece = SProxy

_selection :: SProxy "selection"
_selection = SProxy
