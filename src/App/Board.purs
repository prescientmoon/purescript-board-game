module App.Board where

import Prelude

import Camera (Camera, defaultCamera, pan, screenPan, toViewBox, toWorldCoordinates, zoomOn)
import Component.Utils (getMousePosition, intervalEventSource, joinClasses, maybeElement)
import Control.MonadZero (guard)
import Data.Array as Array
import Data.FunctorWithIndex (mapWithIndex)
import Data.GameMap (GameMap(..), Piece, PieceCoordinates, Cell, _cell, generateMap, hexagonCorner, maximumMapSize, neighbours)
import Data.Int (floor, odd, toNumber)
import Data.Lens (over, set)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), fromMaybe, isNothing)
import Data.MediaType (MediaType(..))
import Data.MouseButton (MouseButton(..), isPressed)
import Data.Symbol (SProxy(..))
import Data.Time.Duration (Milliseconds)
import Data.Traversable (for_)
import Data.Tuple (Tuple(..), uncurry)
import Data.Typelevel.Num (d0, d1)
import Data.Vec (vec2, (!!))
import Data.Vec as Vec
import Data.Vec2 (Vec2)
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
import Math (sqrt)
import Web.UIEvent.MouseEvent as MouseEvent
import Web.UIEvent.WheelEvent as WheelEvent

-- | The 2 supported ways of loading background maps.
data BackgroundMapUrl 
  = Svg String
  | Bitmap String 

-- Settings passed by the parent
type Input
  = { cellSize :: Number -- How big a hexagon is
    , mapPadding :: Vec2 Number -- How much bigger the hexaon map is than the background map.
    , stackedPieceOffset :: Number -- How much to offset each piece in a piece stack (relative to the previous piece)
    , zoomFactor :: Number -- How much to zoom per scroll event
    , size :: Vec2 Number -- How big the svg element should be 
    , backgroundUrl :: BackgroundMapUrl -- The path to the background svg (usually assets/[something])
    , backgroundSize :: Vec2 Number -- The size (in world coordinates) of the background map
     -- How far from the border the mouse needs to be 
     -- when dragging a piece for us to automatically pan in that direction
    , borderScrollAreaSize :: Number
    -- The number of pixels to pan every tick when border scrolling
    , borderScrollSpeed :: Number
    -- The number of milliseconds before we rerun the border scroll logic
    , borderScrollInterval :: Milliseconds
    }

-- State which has to do with selected stuff
type SelectionState
      -- The cell to highlight
  = { selectedCell ::
      Maybe
        { position :: Tuple Int Int -- The coordinates of the cell (highlighted in yellow)
        , neighbours :: Array (Vec2 Int) -- The neighbours of the cell (highlighted in red)
        }
      -- The coordinates of the currently selected piece
    , selectedPiece :: Maybe PieceCoordinates
    }

-- State related to dragging pieces aruond.
type DragState
  = { coordinates :: PieceCoordinates -- The coordinates of the piece the user is curently dragging
      -- To start dragging a piece, the mouse button needs to be pressed while the mouse is anywhere over it.
      -- This field represents the distance from the mouse to the corner of the piece
      -- We need this for rendering
    , cornerOffset :: Vec2 Number 
    , piece :: Piece -- The actual data of the piece we are moving
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
  | MouseUp
  | MouseMove MouseEvent.MouseEvent
  | Zoom WheelEvent.WheelEvent -- Fires when the user uses the scroll wheel
  | AddPiece (Tuple Int Int) -- Fires when ctrl+click -ing on a hexaon. Here for debugging.
  | SelectCell (Tuple Int Int) 
  | SelectPiece PieceCoordinates
  | DragPiece -- Fires when the user starts dragging a piece 
      { corner :: Vec2 Number -- The position of the piece we started to drag
      , coordinates :: PieceCoordinates -- The coordinates on the hexagonal grid
      , piece :: Piece -- The data carried by the piece
      , event :: MouseEvent.MouseEvent 
      }
    -- The reason the index is a (Maybe Int) instead of an Int is because we can either 
    -- drop a piece on top of another piece (which has an index in the piece stack on it's hexagon)
    -- or on top of an empty hexagon (which only has the x and y coordinates)
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

-- Creates the initial state of the component from the input passed by the parent
initialState :: Input -> State
initialState settings =
  { camera: defaultCamera (Tuple 0.3 4.0) 
  , lastMousePosition: zero
  , gameMap: let
        -- The grid has the size of the background map + the size of the padding on both sides.
        gridSize = map (_ * 2.0) settings.mapPadding + settings.backgroundSize
      in generateMap $ maximumMapSize settings.cellSize gridSize
  , selection:
    { selectedPiece: Nothing
    , selectedCell: Nothing
    }
  , dragState: Nothing
  , settings
  }

render :: forall m cs. MonadEffect m => State -> H.ComponentHTML Action cs m
render state =
  SE.svg
    [ SA.height $ state.settings.size !! d1
    , SA.width $ state.settings.size !! d0
    , toViewBox state.settings.size state.camera
    , HE.onWheel $ Zoom >>> Just
    , HE.onMouseMove $ MouseMove >>> Just
    , HE.onMouseUp $ const $ Just MouseUp
    , SA.class_ "board" 
    ]
    [ renderBackgroundMap state.settings
    , renderGameMap state.settings state.selection state.gameMap
    -- We use maybeElement here to only render this while we are dragging something
    , maybeElement state.dragState
        ( \dragState ->
            renderDraggedPiece state.settings.cellSize
              -- The function expects world coordinates but we save the screen coordinates
              -- So we have to convert them
              (toWorldCoordinates state.camera state.lastMousePosition)
              dragState
        )
    ]

-- | Renders the hexagonal grid
renderGameMap :: forall m cs. Input -> SelectionState -> GameMap -> H.ComponentHTML Action cs m
renderGameMap settings selection (GameMap gameMap) =
  arrangeShapes
    $ join
    -- If we don't render from right to left we run into issues like the one in this screenshot:
    --  https://cdn.discordapp.com/attachments/672889285438865453/780428402343018516/unknown.png
    $ Array.reverse 
    $ flip Array.mapWithIndex gameMap \x ->
        Array.mapWithIndex \y cell -> renderCell settings selection cell (Tuple x y)
  where
  arrangeShapes shapes = SE.g [] $ hexagons <> pieces
    where
    hexagons = _.hexagon <$> shapes
    pieces = join $ _.pieces <$> shapes

-- | Renders a single cell of the hexagonal map
renderCell ::
  forall m cs.
  Input ->
  SelectionState ->
  Cell ->
  Tuple Int Int ->
  -- All the pieces need to be rendered after all the hexagons to prevent some weird overlapping issues
  -- So we just return both elements and let the parent handle the ordering
  { pieces :: Array (H.ComponentHTML Action cs m)
  , hexagon :: H.ComponentHTML Action cs m
  }
renderCell settings selection cell (Tuple x y) = { pieces, hexagon }
  where
  hexagon =
    renderHexagon settings.cellSize (vec2 centerX centerY)
      [ SA.class_ classes
      , HE.onClick $ Just <<< onClick      
      , HE.onMouseMove $ MouseMove >>> Just
      -- Handles the case of dropping a piece directly on a heaxgon
      , HE.onMouseUp $ const $ Just $ DropPiece { x, y, index: Nothing }
      ]

  -- If the ctrl key is pressed we add a new piece to the hexagon
  -- else we highlight the hexaon and it's neighbours
  onClick event | MouseEvent.ctrlKey event = AddPiece $ Tuple x y
                | otherwise = SelectCell $ Tuple x y
 
  pieces =
    flip Array.mapWithIndex cell \index piece -> 
      renderPiece settings selection (Tuple centerX centerY) { x, y, index } piece

  -- There is no SA.classes prop so we need to join the classes manually
  classes = joinClasses
      [ Just "board__hexagon"
      , extraClass ]
  
  -- A dynamic className used to style each state diferently from css
  extraClass = case selection.selectedCell of
      Just selectedCell 
        -- The case when this is the selected cell (highlighted with yellow)
        | selectedCell.position == Tuple x y -> Just "board__hexagon--selected" 
        -- The case when this is a neighbour of the selected cell (highlighted with red) 
        | Array.elem (vec2 x y) selectedCell.neighbours -> Just "board__hexagon--selection-neighbour"
      _ -> Nothing

  -- Based on https://www.redblobgames.com/grids/hexagons
  centerX = settings.cellSize * (1.5 * toNumber x + 1.0)
  centerY = settings.cellSize * sqrt 3.0 * (toNumber y + if odd x then 1.0 else 0.5)

-- | Renders an individual game piece
renderPiece :: 
  forall cs m.
  Input -> 
  SelectionState -> 
  Tuple Number Number -> 
  PieceCoordinates -> 
  Piece -> 
  H.ComponentHTML Action cs m
renderPiece settings selection (Tuple centerX centerY) coordinates piece =
  SE.rect
    [ SA.x cornerX
    , SA.y cornerY  
    , SA.height settings.cellSize
    , SA.width settings.cellSize
    , SA.class_ classes 
    -- We use the color the piece carries around as the fill
    , SA.fill $ Just piece.color
    , HE.onMouseDown onMouseDown
    , HE.onClick $ const $ Just $ SelectPiece coordinates
    -- This handles dropping another piece on top of this one
    , HE.onMouseUp $ const $ Just 
        $ DropPiece 
            { x: coordinates.x
            , y: coordinates.y
            , index: Just coordinates.index }
    ]

  where
  classes = joinClasses 
    [ Just "board__piece"
    -- This class is only added when this piece is selected
    , "board__piece--selected" <$ guard (selection.selectedPiece == Just coordinates)
    ]

  onMouseDown event 
    = Just $ DragPiece 
        { corner: vec2 cornerX cornerY
        , coordinates
        , piece
        , event }

  -- The offset from the previous piece in the stack
  offset = settings.stackedPieceOffset * toNumber coordinates.index

  cornerX = (centerX - settings.cellSize / 2.0) + offset
  -- we want the stack to go towards top-right, so we invert the y axis
  cornerY = (centerY - settings.cellSize / 2.0) - offset 

-- | Renders the piece the user is currently dragging.
renderDraggedPiece :: forall cs m. Number -> Vec2 Number -> DragState -> H.ComponentHTML Action cs m
renderDraggedPiece cellSize mousePosition { cornerOffset, piece } =
  SE.rect
    [ SA.x $ position !! d0
    , SA.y $ position !! d1
    , SA.height cellSize
    , SA.width cellSize
    , SA.class_ "board__piece board__piece--dragged"
    , SA.fill $ Just piece.color
    ]
  where
  -- This is the reason we kept track of how far the corner of the piece was from the mouse
  -- (When the piece was piecked up)
  position = mousePosition - cornerOffset

renderBackgroundMap :: forall m cs. Input -> H.ComponentHTML Action cs m
renderBackgroundMap settings =
  -- We use foreign objects to render a non-svg element inside svg.
  -- The reason I prefer this over just rendering the background outside the svg
  -- is so I don't have to replicate the zoom & pan logic in 2 different places
  SE.foreignObject
    [ SA.width width
    , SA.height height
    , SA.x $ settings.mapPadding !! d0
    , SA.y $ settings.mapPadding !! d1
    , SA.class_ "board__background"
    ] [ image ]
  where
  image = case settings.backgroundUrl of
    Svg svgPath -> 
      HH.object -- the <object> tag can be used to load an external svg into the dom
        [ HP.type_ (MediaType "image/svg+xml")
        -- Halogen doesn't have this attribute so we create it manually
        , HP.attr (AttrName "data") svgPath
        -- Halogen expect those to be ints so we floor them
        , HP.width $ floor width
        , HP.height $ floor height
        ] []
    Bitmap bitmapPath -> 
      HH.img 
        [ HP.src bitmapPath
        , HP.width $ floor width
        , HP.height $ floor height
        ]

  width = settings.backgroundSize !! d0
  height = settings.backgroundSize !! d1

-- | Creaes a hexagon using svg.
-- | Accepts an external array of extra attributes 
renderHexagon ::
  forall cs m.
  Number ->
  Vec2 Number ->
  Array (HP.IProp SI.SVGpath Action) -> 
  H.ComponentHTML Action cs m
renderHexagon size center additionalAttributes =
  SE.path
    $ Array.snoc additionalAttributes
    $ SA.d -- this attribute holds the commands for path
    $ SA.Abs -- make all the commands absolute
    -- we add a Z command to the end, which draws a line to where we started, completing the hexagon
    <$> Array.snoc commands SA.Z 
  where
  -- The M command moves us to a point
  -- The L command draws a line to a point
  -- We move to the first corner and then keep drawing lines to the next
  commands = flip mapWithIndex points \index point ->
    (if index == 0 then SA.M else SA.L)  
      (point !! d0)
      (point !! d1)

  -- Here we calculate the positions of all 6 corners in the hexaon
  points :: Array (Vec2 Number)
  points = Array.range 0 5 <#> \nth -> hexagonCorner size nth center

handleAction :: forall cs o m. MonadAff m => Action → H.HalogenM State Action cs o m Unit
handleAction = case _ of
  Initialize -> do
    interval <- gets _.settings.borderScrollInterval
    -- This will fire the BorderScroll action every [interval] milliseconds
    void $ H.subscribe $ BorderScroll <$ intervalEventSource interval
  BorderScroll -> do
    { lastMousePosition, settings, dragState } <- get
    -- We only borderScroll while dragging a piece
    unless (isNothing dragState) do
      let 
        -- The directions (on both axis) to border scroll towards
        direction = 
          Vec.zipWith 
            -- This can be eta-reduced but I'm thinking it might hinder readability
            (\mousePosition size -> getPanDirection settings.borderScrollAreaSize mousePosition size)
            lastMousePosition
            settings.size

        -- Once we have the direction we can just scale it up by the speed and get the delta to pan
        vector = map (_ * settings.borderScrollSpeed) direction
      
      modify_ $ over (prop _camera) $ pan vector
  -- If this fires it means the user dropped a piece outside the hexagon grid,
  -- so we just clear the dragState
  MouseUp -> modify_ $ set (prop _dragState) Nothing
  MouseMove event -> do
    { lastMousePosition, dragState } <- get
    let
      mouseButtonState = MouseEvent.buttons event
      mousePosition = getMousePosition event

      lmbIsPressed = isPressed LeftMouseButton mouseButtonState

      panCamera :: Camera -> Camera
      panCamera camera
        -- when the mouse button is pressed we ban
        -- The amount we pan by is simply the difference between the mouse positions
        | lmbIsPressed = screenPan (mousePosition - lastMousePosition) camera
        | otherwise = camera

      update :: State -> State
      update state
        -- We only pan if we are not dragging a game piece
        | isNothing dragState = over (prop _camera) panCamera state

        {- So it's possible to:
          - start dragging a piece
          - move the mouse outside the browser window
          - release the mouse
          - move the mouse back inside the browser window

          This can cause us to get in a state where the mouse is no longer pressed
          but the piece the user was dragging is still following the mouse.

          To fix this we simply make sure to clear 
          the dragState if the user doesn't hold the left button 
            (when the user moves the mouse back into the browser window we start getting mouse-move events again)
        -}
        | not lmbIsPressed = set (prop _dragState) Nothing state
        | otherwise = state

      updateMousePosition :: State -> State
      updateMousePosition = set (prop _lastMousePosition) mousePosition

    -- Each call to modify_ causes a rerender, so to avoid waiting
    -- for unnecessary rerenders we compose the updates into 1 call
    modify_ $ updateMousePosition <<< update
  Zoom event -> do
    { lastMousePosition, settings } <- get

    let 
      delta = WheelEvent.deltaY event

      factor | delta < 0.0 = settings.zoomFactor
             | otherwise = 1.0 / settings.zoomFactor
    
    -- Delta is 0 when the user scrolls horizontally (on something like a touchpad)
    unless (delta == 0.0) do
      modify_ $ over (prop _camera) $ zoomOn lastMousePosition factor -- we zoom relative to the mouse position
  -- For debugging purpouses, ctrl + clicking on a hexagon adds a new piece there
  AddPiece (Tuple x y) -> do
    modify_
      $ over (prop _gameMap <<< _cell x y)
      $ flip Array.snoc { color: SA.RGB 124 255 115 }
  SelectCell position -> do
    modify_
      $ over (prop _selection <<< prop _selectedCell) \selectedCell ->
          -- If we clicked on the hexagon selected before we just unselect it
          if (selectedCell <#> _.position) == Just position then
            Nothing
          else
            Just
              { position
              , neighbours: neighbours $ uncurry vec2 position
              }
  SelectPiece position -> do
    modify_
      $ over (prop _selection <<< prop _selectedPiece) \oldSelection ->
          -- If we clicked on the piece selected before we just unselect it
          if oldSelection == Just position then 
            Nothing 
          else 
            Just position
  DragPiece { corner, coordinates, piece, event } -> do
    camera <- gets _.camera

    let mousePosition = toWorldCoordinates camera $ getMousePosition event

    modify_ $ set (prop _dragState)
      $ Just
          { coordinates
          , cornerOffset: mousePosition - corner
          , piece
          }
  DropPiece dropAt -> do
    -- We only run this if we are dragging something
    dragState <- gets _.dragState   
    for_ dragState
      \{ coordinates, piece } -> let
        resetDragState = set (prop _dragState) Nothing

        -- This removes the piece from the stack it came from
        removeFromOldStack :: GameMap -> GameMap
        removeFromOldStack =
          over (_cell coordinates.x coordinates.y) \pieces ->
            fromMaybe pieces $ Array.deleteAt coordinates.index pieces

        -- This adds the piece to the stack it was dropped on
        addToNewStack :: GameMap -> GameMap
        addToNewStack =
          over (_cell dropAt.x dropAt.y) \pieces ->
            fromMaybe pieces $ Array.insertAt index piece pieces
          where
          index = case dropAt.index of
            -- If it was dropped on an empty hexagon it's going to be the first in the stack
            Nothing -> 0
            -- else it's going to sit next to the piece it was dropped onto
            Just previousIndex 
            -- When we remove the piece from the stack it came from, 
            -- all the pieces higher in the same stack have their indices decreased by one.
            -- So we check if this is one of those, and if it is, we decrease it too
            --  (previousIndex + 1 - 1 instead of previousIndex + 1)
              | dropAt.x == coordinates.x && 
                dropAt.y == coordinates.y && 
                previousIndex >= coordinates.index -> previousIndex
            -- In the default case, we want to place the piece on top of the one it was dropped onto, so we increase the indec by one
              | otherwise -> previousIndex + 1

        in modify_ $ resetDragState <<< over (prop _gameMap) (addToNewStack <<< removeFromOldStack)

-- | Calculates the direction we should border scroll towards (positive / negative / zero)
getPanDirection :: Number -> Number -> Number -> Number
getPanDirection limit position maximum
  | position < limit = 1.0
  | position > maximum - limit = -1.0
  | otherwise = 0.0

-- SProxies for use with lenses
-- I use a vscode snippet to quickly generate those
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
