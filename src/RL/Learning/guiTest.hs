import Graphics.Rendering.Cairo as C
import Graphics.UI.Gtk
import Control.Concurrent
import Control.Concurrent.MVar as MV
import Control.Monad (when, liftM, forM, forM_)
import qualified TDControl as AI
import qualified Data.Map as M
import qualified Data.Maybe as Maybe
import GridWorldGUI
import System.Exit

handleKeyboard window state = do
    tryEvent $ do
        val <- eventKeyVal
        "Left" <- liftIO $ keyvalName val
        checkInput 3
    tryEvent $ do
        val <- eventKeyVal
        "Right" <- liftIO $ keyvalName val
        checkInput 4
    tryEvent $ do
        val <- eventKeyVal
        "Up" <- liftIO $ keyvalName val
        checkInput 1
    tryEvent $ do
        val <- eventKeyVal
        "Down" <- liftIO $ keyvalName val
        checkInput 2

    where checkInput i = liftIO $ do
            --MV.modifyMVar_ state (\s -> return s{sInput = return i})
            MV.modifyMVar_ state (\s -> return s{sOffset = sOffset s + 10})
            updateWorld state
            print i
            widgetQueueDraw window


updateWorld state = do
    s <- readMVar state
    let world = sWorld s
    MV.modifyMVar_ state (\s -> return s{sWorld = world})


drawWindow window state tiles = liftIO $ do
    cr <- widgetGetDrawWindow window
    world <- liftM sWorld $ MV.readMVar state
    --offset <- liftM sOffset $ readMVar state
    let (multx, multy) = (200, 200) -- tiles are 200 x 200
    renderWithDrawable cr $ do
        C.scale 0.3 0.3

        let (maxX, maxY) = (c world, r world)
        let coords = [(x,y) | x <- [0..maxX-1], y <- [0..maxY-1]]
        let lookup_ = Maybe.fromJust . (`M.lookup` tiles)
        let showAt what (x,y) = do
                C.setSourceSurface (lookup_ what)
                                   (fromIntegral x * multx)
                                   (fromIntegral y * multy)
                C.paint
        let goal c  = showAt "rsz_flag"          c
        let agent c = showAt "rsz_yoda"          c
        let grass c = showAt "rsz_grass_texture" c

        forM coords $ \c ->
            case () of () | isGoal' (grid world) c -> grass c >> goal  c
                          | isAgent (grid world) c -> grass c >> agent c
                          | otherwise              -> grass c

        --image <- liftIO $ C.imageSurfaceCreateFromPNG "images/rsz_grass_texture.png"
        --C.setSourceSurface image offset offset
        --C.paint
    return True


loadTiles strings = do 
    let longStrings = map (\s -> "images/" ++ s ++ ".png") strings
    surfaces <- mapM C.imageSurfaceCreateFromPNG longStrings
    return $ M.fromList $ zip strings surfaces

main :: IO ()
main = do
    initGUI

    qT <- AI.qLearn
    state <- MV.newMVar =<< (emptyState qT)
    tiles <- loadTiles ["rsz_flag"
                       ,"rsz_grass_texture"
                       ,"rsz_yoda"]

    window <- windowNew
    window `on` sizeRequest   $ return (Requisition 300 300)
    window `on` keyPressEvent $ handleKeyboard window state
    window `on` exposeEvent   $ drawWindow window state tiles


    forkIO $ do
        let trainHere = do
            threadDelay 1000000
            postGUIAsync $ train 1 100 state window
        trainHere

    onDestroy window mainQuit
    widgetShowAll window
    mainGUI

