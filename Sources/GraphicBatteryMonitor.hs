-----------------------------------------------------------------------------
-- Copyright : (c) mhd aka 414d
-- License : GPL3
-- 
-- Maintainer : mhd <414dbox at Gmail>
-- Stability : unstable
-- Portability : unportable
-- 
-- A graphic battery monitor
-- 
-----------------------------------------------------------------------------

module GraphicBatteryMonitor (showMonitor) where

import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO
import System.FilePath ((</>))
import System.Directory
import System.Posix
import Text.Printf
import Data.Char
import Data.Maybe  
import Control.Monad


data BattOptions = BattOptions
                   {
                     lowColor :: String,
                     lowColorFlicker :: String,
                     normalColor :: String,
                     highColor :: String,
                     highColorFlicker :: String,
                     chargingColor :: String,
                     disableTimeColoring :: Bool,
                     
                     criticalLowThreshold :: Int,
                     lowThreshold :: Int,
                     highThreshold :: Int,
                     chunkCount :: Int,

                     batDir :: String,
                     timeFormat :: String,

                     timePos :: String
                   }

                 
defaultOptions = BattOptions
                 {
                   lowColor = "#ff0000",
                   lowColorFlicker = "#cc0000",
                   normalColor = "#ffff00",
                   highColor = "#00ff00",
                   highColorFlicker = "#00cc00",
                   chargingColor = "#c6eff7",
                   disableTimeColoring = False,
                   
                   criticalLowThreshold = 10,
                   lowThreshold = 20,
                   highThreshold = 60,
                   chunkCount = 5,

                   batDir = "BAT0",
                   timeFormat = "%2d:%d",

                   timePos = "r"
                 }
 

options :: [OptDescr (BattOptions -> IO BattOptions) ]
options = [
  	     Option "h?" ["help"]	       (NoArg(\_ -> do
    	     	    	 		       		     name <- getProgName
							     let header = "Usage: " ++ name ++ " [OPTION...]\n\
                                                             \To show the output in xmobar add to xmobar config \
                                                             \Run Com \"grabtor\" [] \"\" 10 for commands field\n\
                                                             \%grabtor% for template field.\nOptions and defaults:"
                					     putStr (usageInfo header options)
                					     exitWith ExitSuccess) ) "Show this help and exit"

							   
	   , Option "l" ["low"]	       	       (ReqArg (\arg opt -> return opt {lowColor = arg} ) (lowColor defaultOptions) )
	     	    			       	       "The color of the icon when the battery has its charge below \
						       \the low threshold\n\n"
						       
	   , Option "f" ["low-color-flicker"]  (ReqArg (\arg opt -> return opt {lowColorFlicker = arg} )
	     	    			       	       (lowColorFlicker defaultOptions) )
						       "The color for the icon's flicker when the battery has its charge below\nthe \
						        \critical low threshold. In this case the color will switch\nfrom the low \
						        \color to the low color flicker\n\n"
									       
	   , Option "n" ["normal"] 	       (ReqArg (\arg opt -> return opt {normalColor = arg} ) (normalColor defaultOptions) )
	     	    			       	       "The color of the icon when the battery has its charge between \
						       \the low threshold\nand the high threshold\n\n"
						       
	   , Option "g" ["high"]   	       (ReqArg (\arg opt -> return opt {highColor = arg} ) (highColor defaultOptions) )
	     	    			       	       "The color of the icon when the battery has its charge above \
						       \the high threshold\n\n"
	     	    			       	       
	     	    			       	       
	   , Option "F" ["high-color-flicker"] (ReqArg (\arg opt -> return opt {highColorFlicker = arg} )
	     	    			       	       (highColorFlicker defaultOptions) )
						       "The color for the icon's flicker when the battery has full charge and\
						       \\none is connected to ac current. \
						       \In this case the color will switch\nfrom the high color to \
						       \the high color flicker\n\n"
									       
	   , Option "a" ["charging"]	       (ReqArg (\arg opt -> return opt {chargingColor = arg} )
	     	    			       	       (chargingColor defaultOptions) )
						       "The color of the progress bar during the charging"
						       
	   , Option "d" ["disable-time-coloring"] (NoArg (\opt -> return opt {disableTimeColoring = True} ) )
	     	    			       	      	 "Don't colorize time remaining up to charging/discharging"
	   
	   , Option "r" ["critical-low"]       (ReqArg (\arg opt -> return opt {criticalLowThreshold = read arg} )
	     	    			       	       (show $ criticalLowThreshold $ defaultOptions) )
						       "The critical low threshold of the charge. The valid value is between\n\
						       \0 and the low treshold\n\n"
									       
	   , Option "L" ["Low"]		       (ReqArg (\arg opt -> return opt {lowThreshold = read arg} )
	     	    			       	       (show $ lowThreshold defaultOptions) )
						       "The low threshold of the charge. The valid value is between\n\
						       \the critical threshold and the high threshold\n\n"
									       
	   , Option "H" ["High"]   	       (ReqArg (\arg opt -> return opt {highThreshold = read arg} )
	     	    			       	       (show $ highThreshold defaultOptions) )
						       "The high threshold of the charge. The valid value is between \
						       \the low threshold\nand 100\n\n"
									       
	   , Option "c" ["chunks"] 	       (ReqArg (\arg opt -> return opt {chunkCount = read arg} )
	     	    			       	       (show $ chunkCount defaultOptions) )
						       "The chunks' amount to show the charge level. Valid values are from 1 to 5\n\n"

	   , Option "D" ["bat-dir"]	       (ReqArg (\arg opt -> return opt {batDir = arg} )
	     	    			       	       (show $ batDir defaultOptions) )
						       "The directory name in /sys/class/power_supply/ where to look for ACPI files\n\
						       \for the battery\n\n"
									       
	   , Option "m" ["time-format"]	       (ReqArg (\arg opt -> return opt {timeFormat = arg} )
	     	    			       	       (show $ timeFormat defaultOptions) )
						       "The time format for printf. The time means time up to charging/discharging.\n\
                                                        \The empty string will hide the time\n\n"
									       
	   , Option "p" ["time-pos"]	       (ReqArg (\arg opt -> return opt {timePos = arg} )
	     	    			       	       (show $ timePos $ defaultOptions) )
	        				       "Show the time to the left/right of the battery icon. Valid values are \"r\" or\
	        				       \ \"l\""
	  ]
 

sysDir = "/sys/class/power_supply"


getOptions :: [String] -> IO BattOptions
getOptions argv = do
  when (not $ null $ errors) (printErrorAndExit $ dropTrailingBlanks $ foldl1 (++) errors)
	   
  opts <- foldl (>>=) (return defaultOptions) actions
  checkAndExitOnWrongOption opts
	   
    where (actions, _, errors) = getOpt RequireOrder options argv
          dropTrailingBlanks s = reverse $ dropWhile isSpace $ reverse s
                                        
	  printErrorAndExit s = do putStrLn s
                                   exitWith $ ExitFailure $ 1

	  checkAndExitOnWrongOption opts
	      | not (chunkCount opts >= 1 && chunkCount opts <= 5) =
                  printErrorAndExit "chunks opt should be in [1..5]"
					   
	      | not (highThreshold opts >= 0 &&  highThreshold opts <= 100) =
                  printErrorAndExit "High opt should be in [0..100]"

	      | not (lowThreshold opts >= 0 && lowThreshold opts < highThreshold opts) =
                  printErrorAndExit "Low opt should be in [0..High opt)"

              | not (criticalLowThreshold opts >= 0 && criticalLowThreshold opts < lowThreshold opts) =
                  printErrorAndExit "criticalLow opt should be in [0.. lowThreshold opt)"

              | not (timePos opts == "r" || timePos opts == "l") =
                  printErrorAndExit "timePos opt should be in [\"r\", \"l\"]"
					   
	      | otherwise = return opts					 	  	      


data BattInfo = BattInfo
                {
                  isCharging :: Bool,
                  percents :: Int,
                  timeLeft :: (Int, Int)
     	      	}
     	      
		
getBattInfo :: BattOptions -> IO BattInfo
getBattInfo opts = do
  s <- getFirstLineFromFile (path </> "charge_full")
  let chFull = read $ fromJust s

  s <- getFirstLineFromFile (path </> "charge_now")
  let chNow = read $ fromJust s

  s <- getFirstLineFromFile (path </> "current_now")
  let current = read $ fromJust s

  status <- getFirstLineFromFile (path </> "status")

  let hours = truncate $ (chFull - chNow) / current
  let mins = round $ 60 * ( (chFull - chNow) / current - fromIntegral hours)
	    
  return BattInfo
         {
	   isCharging = if map toLower (fromJust status) == "charging" then True else False,
	   percents = round $ (chNow / chFull) * 100,
	   timeLeft = (hours, mins)
	 }
			    
    where path = sysDir </> batDir opts


data Frame = Frame
             {
               chunks :: Int,
               partOfChunk :: Int
             }

             
defaultFrame = Frame
               {
                 chunks = 0,
                 partOfChunk = 0
               }
               
  
data MonitorStateInfo = MonitorStateInfo
                        {
                          lowColorFlickerValue :: Maybe String,
                          highColorFlickerValue :: Maybe String,
                          frame :: Maybe Frame
                        }


defaultMonitorStateInfo = MonitorStateInfo
                          {
                            lowColorFlickerValue = Nothing,
                            highColorFlickerValue = Nothing,
                            frame = Nothing
                          }
                          

getMonitorStateInfo :: IO MonitorStateInfo
getMonitorStateInfo = do
  l <- getFirstLineFromFile "/tmp/lowColorFlicker"
  h <- getFirstLineFromFile "/tmp/highColorFlicker"

  line <- getFirstLineFromFile "/tmp/chargingFrame"
             
  let frameStr = fromMaybe "0 0" line

  return MonitorStateInfo
           {
             lowColorFlickerValue = l,
             highColorFlickerValue = h,
             frame = Just Frame
             {
               chunks = read $ head $ words $ frameStr,
               partOfChunk = read $ last $ words $ frameStr
             }
           }


saveMonitorStateInfo :: MonitorStateInfo -> IO ()
saveMonitorStateInfo info = do                        
  writeStringToFile (lowColorFlickerValue info) "/tmp/lowColorFlicker"
  writeStringToFile (highColorFlickerValue info) "/tmp/highColorFlicker"
  writeStringToFile frameStr "/tmp/chargingFrame"
                    
  where frameStr = if isJust $ frame info
                   then let f = fromJust $ frame info
                            c = chunks f
                            p = partOfChunk f
                        in Just ( (show c) ++ " " ++ (show p) ) 
                   else Nothing
                 
  
chunkPartsToString :: Int -> String
chunkPartsToString part | part == 0 = ""
		        | part == 1 = "▍"
			| part == 2 = "▌"
			| part == 3 = "▋"
			| part == 4 = "▊"

			  
getOneChunk = '▉'


getEmptyChunk = '_'


getChunkPartsCount = 5


chargeToColor :: Int -> BattOptions -> String
chargeToColor percent opts | percent < lowThreshold opts = lowColor opts
		 	   | percent < highThreshold opts = normalColor opts
			   | percent >= highThreshold opts = highColor opts

			      
chargeToString :: Int -> Int -> (String, Int)
chargeToString percent battChunksCount = (stringView, emptyChunksAmount)
    where percentsInAChunk = 100 `div` battChunksCount
	  percentsInAChunkPart = percentsInAChunk `div` getChunkPartsCount
			    
	  chunksToShow = percent `div` percentsInAChunk + partsOfChunk `div` getChunkPartsCount
	  remOfPercent = percent `mod` percentsInAChunk
			    
	  chunkPartsToShow = partsOfChunk `mod` getChunkPartsCount

	  partsOfChunk = round ( (fromIntegral remOfPercent) / (fromIntegral percentsInAChunkPart) )

          stringView = take chunksToShow (repeat getOneChunk) ++ chunkPartsToString chunkPartsToShow
			    								       
	  emptyChunksAmount = (battChunksCount - length stringView)
			    		      	 

emptyChunksToString :: Int -> String
emptyChunksToString emptyChunksAmount = take emptyChunksAmount (repeat getEmptyChunk)


chargingFrameToString :: Frame -> Int -> (String, Frame)
chargingFrameToString _ 0 = ("", defaultFrame)
chargingFrameToString curFrame emptyChunksAmount = (stringView, nextFrame)
    where curChunks = chunks curFrame
          curPartOfChunk = partOfChunk curFrame
                                             
          stringView = take curChunks (repeat getOneChunk) ++ chunkPartsToString curPartOfChunk

	  normalChunks = curChunks `mod` emptyChunksAmount
	  normalPartOfChunk = curPartOfChunk `mod` getChunkPartsCount
				
	  nextChunks = (normalChunks + (normalPartOfChunk + 1) `div` getChunkPartsCount) 
	  nextPartOfChunk = ( (normalPartOfChunk + 1) `mod` getChunkPartsCount ) * setToZeroOnChunksOverflow

          nextFrame = Frame {chunks = nextChunks, partOfChunk = nextPartOfChunk}

	  setToZeroOnChunksOverflow = fromEnum $ (curChunks `div` emptyChunksAmount) == 0


colorizeString :: String -> String -> String
colorizeString colorNum string = "<fc=" ++ colorNum ++ ">" ++ string ++ "</fc>"


battInfoToColors :: BattInfo -> MonitorStateInfo -> BattOptions -> (String, Maybe String)
battInfoToColors battInfo monStateInfo opts = (chargeColor, colorFlicker)
  where colorFlicker = if showFlicker
                       then if prevColorF == color 
                            then Just colorF
                            else Just color
                       else Nothing

        chargeColor = if isNothing colorFlicker
                      then chargeToColor (percents battInfo) opts
                      else fromJust colorFlicker

        (showFlicker, color, colorF, prevColorF) =
            if isCharging battInfo
            then let (hours, mins) = timeLeft battInfo
                 in (percents battInfo == 100 && hours == 0 && mins == 0,
                     highColor opts, highColorFlicker opts,
                     fromMaybe "" (highColorFlickerValue monStateInfo) )
                 
            else (percents battInfo <= criticalLowThreshold opts,
                  lowColor opts, lowColorFlicker opts,
                  fromMaybe "" (lowColorFlickerValue monStateInfo) )
		      	      

timeLeftToString :: BattInfo -> BattOptions -> String
timeLeftToString _ opts    | timeFormat opts == "" = ""
timeLeftToString battInfo opts = printf (timeFormat opts) hours mins
		 where (hours, mins) = timeLeft battInfo


makeTimeIcon :: BattInfo -> MonitorStateInfo -> BattOptions -> String
makeTimeIcon battInfo monStateInfo opts = timeIcon
    where timeStr = timeLeftToString battInfo opts
                     
          timeIcon = if disableTimeColoring opts
                     then timeStr
                     else let (chargeColor, colorFlicker) = battInfoToColors battInfo monStateInfo opts
                              color = fromMaybe chargeColor colorFlicker
                          in colorizeString color timeStr
		       

makeBattIcon :: BattInfo -> MonitorStateInfo -> BattOptions -> (String, Maybe String)
makeBattIcon battInfo monStateInfo opts = (battIcon, colorFlicker)
    where (chargeString, emptyChunksAmount) = chargeToString (percents battInfo) (chunkCount opts)
			     
          (chargeColor, colorFlicker) = battInfoToColors battInfo monStateInfo opts
                                        
          battIcon = if isNothing colorFlicker
		     then "[" ++ colorizeString chargeColor chargeString ++ emptyChunksString ++ "}"
                                   
		     else colorizeString (fromJust colorFlicker) ("[" ++ chargeString ++ emptyChunksString ++ "}")
					   
	  emptyChunksString = emptyChunksToString emptyChunksAmount			     
			     
			     
makeChargingBattIcon :: BattInfo -> MonitorStateInfo -> BattOptions -> (String, Frame, Maybe String)
makeChargingBattIcon battInfo monStateInfo opts = (battIcon, nextFrame, colorFlicker)
    where (chargeString, emptyChunksAmount) = chargeToString (percents battInfo) (chunkCount opts)

          (chargeColor, colorFlicker) = battInfoToColors battInfo monStateInfo opts

 	  battIcon = if isNothing colorFlicker
		     then "[" ++ colorizeString chargeColor chargeString ++
			  colorizeString (chargingColor opts) frameStr ++ emptyChunksString ++ "}"
                                           
		     else colorizeString (fromJust colorFlicker) ("[" ++ chargeString ++ "}")

          (frameStr, nextFrame) = chargingFrameToString (fromJust $ frame monStateInfo) emptyChunksAmount
                           
          emptyChunksString = emptyChunksToString (emptyChunksAmount - length frameStr)
		       

getFirstLineFromFile :: String -> IO (Maybe String)
getFirstLineFromFile path = do
  fileExists <- doesFileExist path
  if fileExists
  then withFile path ReadMode $
           \h -> do
             eof <- hIsEOF h
             if eof
             then return Nothing
             else do s <- hGetLine h; return (Just s)
  else return Nothing


writeStringToFile :: Maybe String -> String -> IO ()
writeStringToFile str path = do
  when (isJust str) (writeFile path $ fromJust str)
  emptyFile <- isEmptyFile path
  when (isNothing str &&  (not $ fromMaybe True emptyFile) ) (writeFile path "")
    where isEmptyFile path = do
            fileExists <- doesFileExist path
            if fileExists
            then do stat <- getFileStatus path
                    return $ Just $ (fileSize stat == 0)
            else return Nothing


battInfoToString :: BattInfo -> MonitorStateInfo -> BattOptions -> (String, MonitorStateInfo)
battInfoToString battInfo monStateInfo opts = (stringView, newMonStateInfo)
    where (battIcon, newMonStateInfo) = 
              if isCharging battInfo
              then
                  let (icon, nextFrame, colorFlicker) = makeChargingBattIcon battInfo monStateInfo opts
                      stateInfo = defaultMonitorStateInfo
                                  {
                                    highColorFlickerValue = colorFlicker
                                  , frame = Just nextFrame
                                  }
                  in (icon, stateInfo)               
              else
                  let (icon, colorFlicker) = makeBattIcon battInfo monStateInfo opts
                      stateInfo = defaultMonitorStateInfo {lowColorFlickerValue = colorFlicker}
                                       
                  in (icon, stateInfo)
                  
          timeIcon = makeTimeIcon battInfo monStateInfo opts

          stringView = case timePos opts of
                         "r" -> battIcon ++ timeIcon
                         "l" -> timeIcon ++ battIcon
       
        
showMonitor :: [String] -> IO String
showMonitor args =
    do opts <- getOptions args
       battInfo <- getBattInfo opts
       monStateInfo <- getMonitorStateInfo

       let (monitorOutput, newMonStateInfo) = battInfoToString battInfo monStateInfo opts
                                              
       saveMonitorStateInfo newMonStateInfo
       return monitorOutput
