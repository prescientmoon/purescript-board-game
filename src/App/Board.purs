module App.Board where

import Prelude
import Camera (Camera, Vec2, defaultCamera, screenPan, toViewBox, toWorldCoordinates, zoomOn)
import Component.Utils (intervalEventSource, maybeElement)
import Data.Array as Arra
import Data.Array as Array
import Data.FunctorWithIndex (mapWithIndex)
import Data.GameMap (GameMap(..), PiecePosition, Piece, generateMap, maximumMapSize, neighbours)
import Data.Int (floor, odd, toNumber)
import Data.Lens (over, set)
import Data.Lens.Index (ix)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), fromMaybe, isNothing, maybe)
import Data.MediaType (MediaType(..))
import Data.MouseButton (MouseButton(..), isPressed)
import Data.Set (Set)
import Data.Set as Set
import Data.String (joinWith)
import Data.Symbol (SProxy(..))
import Data.Time.Duration (Milliseconds)
import Data.Tuple (Tuple(..), uncurry)
import Data.Typelevel.Num (D6, d0, d1, d5)
import Data.Vec (Vec, vec2, (!!))
import Data.Vec as Vec
import Effect.Aff.Class (class MonadAff)
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
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.MouseEvent as MouseEvent
import Web.UIEvent.WheelEvent (WheelEvent)
import Web.UIEvent.WheelEvent as WheelEent

type Input
  = { cellSize :: Number
    , mapPadding :: Vec2 Number
    , stackedPieceOffset :: Number
    , windowSize :: Vec2 Number
    , backgroundUrl :: String
    , backgroundSize :: Vec2 Number
    , borderScrollAreaSize :: Number
    , borderScrollSpeed :: Number
    , borderScrollInterval :: Milliseconds
    }

type SelectionState
  = { selectedCell ::
      Maybe
        { position :: Tuple Int Int
        , neighbours :: Set (Tuple Int Int)
        }
    , selectedPiece :: Maybe PiecePosition
    }

type DragState
  = { position :: PiecePosition
    , cornerOffset :: Vec2 Number
    , piece :: Piece
    }

type State
  = { lastMousePosition :: Vec2 Number
    , camera :: Camera
    , gameMap :: GameMap
    , settings :: Input
    , selection :: SelectionState
    , dragState :: Maybe DragState
    }

data Action
  = Initialize
  | BorderScroll
  | HandleMouseDown
  | HandleMouseUp
  | HandleMouseMove MouseEvent.MouseEvent
  | HandleScroll WheelEvent
  | AddPiece (Tuple Int Int)
  | SelectCell (Tuple Int Int)
  | SelectPiece PiecePosition
  | DragPiece (Vec2 Number) PiecePosition Piece MouseEvent
  | DropPiece { x :: Int, y :: Int, index :: Maybe Int }

component :: forall q o m. MonadAff m => H.Component HH.HTML q Input o m
component =
  H.mkComponent
    { initialState
    , render
    , eval:
      H.mkEval
        $ H.defaultEval
            { handleAction = handleAction
            , initialize = Just Initialize
            }
    }

initialState :: Input -> State
initialState settings =
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
  , dragState: Nothing
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
    , HE.onMouseUp $ const $ Just HandleMouseUp
    ]
    [ renderBackgroundMap state.settings
    , renderGameMap state.settings state.selection state.gameMap
    , maybeElement state.dragState
        ( \a ->
            renderDraggedPiece state.settings.cellSize
              (toWorldCoordinates state.camera state.lastMousePosition)
              a
        )
    ]

renderGameMap ::
  forall m cs. Input -> SelectionState -> GameMap -> H.ComponentHTML Action cs m
renderGameMap settings selection (GameMap gameMap) =
  go $ join
    $ Array.reverse
    $ flip Array.mapWithIndex gameMap \x inner ->
        flip Array.mapWithIndex inner \y cellData -> renderCell settings selection cellData x y
  where
  go svg = SE.g [] $ (svg <#> _.hexagon) <> pieces
    where
    pieces = join $ _.pieces <$> svg

renderCell ::
  forall m cs.
  Input ->
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
    renderFlatHexagon settings.cellSize (vec2 centerX centerY)
      [ SA.class_ classes
      , HE.onClick
          $ \event ->
              Just
                if MouseEvent.ctrlKey event then
                  AddPiece (Tuple x y)
                else
                  SelectCell (Tuple x y)
      , HE.onMouseMove $ HandleMouseMove >>> Just
      , HE.onMouseUp $ const $ Just $ DropPiece { x, y, index: Nothing }
      ]

  pieces =
    flip Array.mapWithIndex cellData \index piece ->
      let
        position = { x, y, index }

        offset = settings.stackedPieceOffset * toNumber index

        cornerX = (centerX - settings.cellSize / 2.0) + offset

        cornerY = (centerY - settings.cellSize / 2.0) - offset
      in
        SE.rect
          [ SA.x cornerX
          , SA.y cornerY
          , SA.height settings.cellSize
          , SA.width settings.cellSize
          , SA.class_ $ "board__piece"
              <> if selection.selectedPiece == Just position then
                  " board__piece--selected"
                else
                  ""
          , SA.fill $ Just piece.color
          , HE.onClick $ const $ Just $ SelectPiece position
          , HE.onMouseDown $ Just <<< DragPiece (vec2 cornerX cornerY) position piece
          , HE.onMouseUp $ const $ Just $ DropPiece { x, y, index: Just index }
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

  centerX = settings.cellSize * (1.5 * toNumber x + 1.0)

  centerY = settings.cellSize * sqrt 3.0 * (toNumber y + if odd x then 1.0 else 0.5)

-- | Renders the piece we are dragging right now.
renderDraggedPiece :: forall cs m. Number -> Vec2 Number -> DragState -> H.ComponentHTML Action cs m
renderDraggedPiece cellSize mousePosition { cornerOffset, piece } =
  SE.rect
    [ SA.x $ position !! d0
    , SA.y $ position !! d1
    , SA.height cellSize
    , SA.width cellSize
    , SA.class_ $ "board__piece board__piece--dragged"
    , SA.fill $ Just piece.color
    ]
  where
  position = mousePosition - cornerOffset

renderBackgroundMap :: forall m cs. Input -> H.ComponentHTML Action cs m
renderBackgroundMap settings =
  SE.foreignObject
    [ SA.width width
    , SA.height height
    , SA.x $ settings.mapPadding !! d0
    , SA.y $ settings.mapPadding !! d1
    , SA.class_ "board__background"
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

handleAction :: forall cs o m. MonadAff m => Action → H.HalogenM State Action cs o m Unit
handleAction = case _ of
  Initialize -> do
    interval <- gets _.settings.borderScrollInterval
    void $ H.subscribe $ BorderScroll <$ intervalEventSource interval
  BorderScroll -> do
    { lastMousePosition, settings, dragState } <- get
    unless (isNothing dragState) do
      let
        panVector =
          ((*) settings.borderScrollSpeed)
            <$> Vec.zipWith (getPanDirection settings.borderScrollAreaSize)
                lastMousePosition
                settings.windowSize
      modify_ $ over (prop _camera) $ screenPan panVector
  HandleMouseUp -> do
    modify_ $ set (prop _dragState) Nothing
  HandleMouseDown -> pure unit
  HandleMouseMove event -> do
    { lastMousePosition, dragState } <- get
    let
      mouseButtonState = MouseEvent.buttons event

      mousePosition = toNumber <$> vec2 (MouseEvent.clientX event) (MouseEvent.clientY event)

      updateCamera camera
        | isPressed LeftMouseButton mouseButtonState = screenPan (mousePosition - lastMousePosition) camera
        | otherwise = camera

      update state
        | isNothing dragState = over (prop _camera) updateCamera state
        | not (isPressed LeftMouseButton mouseButtonState) = set (prop _dragState) Nothing state
        | otherwise = state
    modify_ $ set (prop _lastMousePosition) mousePosition <<< update
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
  DragPiece cornerPosition coordinates piece event -> do
    camera <- gets _.camera
    let
      mousePosition = toWorldCoordinates camera $ toNumber <$> vec2 (MouseEvent.clientX event) (MouseEvent.clientY event)
    modify_ $ set (prop _dragState)
      $ Just
          { position: coordinates
          , cornerOffset: mousePosition - cornerPosition
          , piece
          }
  DropPiece dropAt -> do
    dragState <- gets _.dragState
    case dragState of
      Just { position, piece }
        | position.x /= dropAt.x || position.y /= dropAt.y ->
          modify_ $ resetDragState
            <<< over (prop _gameMap) (addToNewStack <<< removeFromOldStack)
          where
          resetDragState = set (prop _dragState) Nothing

          removeFromOldStack :: GameMap -> GameMap
          removeFromOldStack =
            over (_Newtype <<< ix position.x <<< ix position.y) \pieces ->
              fromMaybe pieces $ Array.deleteAt position.index pieces

          addToNewStack :: GameMap -> GameMap
          addToNewStack =
            over (_Newtype <<< ix dropAt.x <<< ix dropAt.y) \pieces ->
              fromMaybe pieces $ Array.insertAt (maybe 0 ((+) 1) dropAt.index) piece pieces
      _ -> do
        handleAction HandleMouseUp

getPanDirection :: Number -> Number -> Number -> Number
getPanDirection limit position maximum
  | position < limit = 1.0
  | position > maximum - limit = -1.0
  | otherwise = 0.0

-- SProxies
_selectedCell :: SProxy "selectedCell"
_selectedCell = SProxy

_selectedPiece :: SProxy "selectedPiece"
_selectedPiece = SProxy

_selection :: SProxy "selection"
_selection = SProxy

_dragState :: SProxy "dragState"
_dragState = SProxy

_gameMap :: SProxy "gameMap"
_gameMap = SProxy

_camera :: SProxy "camera"
_camera = SProxy

_lastMousePosition :: SProxy "lastMousePosition"
_lastMousePosition = SProxy
