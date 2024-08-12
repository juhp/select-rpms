module SelectRPMs (
  installArgs,
  checkSelection,
  decideRPMs,
  installRPMs,
  notDebugPkg,
  printInstalled,
  selectDefault,
  showRpm,
  Existence(..),
  ExistingStrategy(..),
  ExistPathNVRA,
  PkgMgr(..),
  Select(..),
  Yes(..)
  )
where

import Control.Monad.Extra (forM_, mapMaybeM, unless, when)
import Data.Either (partitionEithers)
import Data.List.Extra (foldl', isInfixOf, nubOrd, nubSort, (\\))
import Data.Maybe (fromMaybe)
import Data.RPM.NVRA (NVRA(..), showNVRA)
import Data.Tuple.Extra (fst3, thd3)
import Safe (headMay)
import SimpleCmd (cmd_, cmdMaybe, error', sudo_, (+-+))
import SimplePrompt (yesNoDefault)
import System.Directory
import System.FilePath ((</>), (<.>))
import System.FilePath.Glob (compile, isLiteral, match)

data Select = All
            | Ask
            | PkgsReq
              [String] -- include matches
              [String] -- except matches
              [String] -- exclude
              [String] -- adde
  deriving Eq

selectDefault :: Select
selectDefault = PkgsReq [] [] [] []

installArgs :: String -> Select
installArgs cs =
  case words cs of
    ["-a"] -> All
    ["--all"] -> All
    ["-A"] -> Ask
    ["--ask"] -> Ask
    ws -> installPairs [] [] [] [] ws
  where
    installPairs :: [String] -> [String] -> [String] -> [String]
                 -> [String] -> Select
    installPairs incl except excl add [] = PkgsReq incl except excl add
    installPairs incl except excl add (w:ws)
      | w `elem` ["-p","--package"] =
          case ws of
            [] -> error' "--install opts: --package missing value"
            (w':ws') -> checkPat w' $
                        installPairs (w':incl) except excl add ws'
      | w `elem` ["-e","--except"] =
          case ws of
            [] -> error' "--install opts: --except missing value"
            (w':ws') -> checkPat w' $
                        installPairs incl (w':except) excl add ws'
      | w `elem` ["-x","--exclude"] =
          case ws of
            [] -> error' "--install opts: --exclude missing value"
            (w':ws') -> checkPat w' $
                        installPairs incl except (w':excl) add ws'
      | w `elem` ["-i","--include"] =
          case ws of
            [] -> error' "--install opts: --include missing value"
            (w':ws') -> checkPat w' $
                        installPairs incl except excl (w':add) ws'
      | otherwise = error' "invalid --install opts"

    checkPat w' f =
      if null w'
      then error' "empty pattern!"
      else f

checkSelection :: Monad m => Select -> m ()
checkSelection (PkgsReq ps es xs is) =
  forM_ (ps ++ es ++ xs ++ is) $ \s ->
  when (null s) $ error' "empty package pattern not allowed"
checkSelection _ = return ()

data ExistingStrategy = ExistingNoReinstall | ExistingSkip

data Yes = No | Yes
  deriving Eq

data Existence = ExistingNVR | ChangedNVR | NotInstalled
  deriving (Eq, Ord, Show)

type ExistPathNVRA = (Existence,Maybe FilePath,NVRA)

-- FIXME determine and add missing internal deps
decideRPMs :: Yes -> Bool -> Maybe ExistingStrategy -> Select -> String
           -> [(Maybe FilePath,NVRA)] -> IO [ExistPathNVRA]
decideRPMs yes listmode mstrategy select prefix mfpnvras = do
  classified <- mapMaybeM installExists (filter (isBinaryRpm . snd) mfpnvras)
  if listmode
    then do
    case select of
      PkgsReq subpkgs exceptpkgs exclpkgs addpkgs ->
        mapM_ printInstalled $
        selectRPMs prefix (subpkgs,exceptpkgs,exclpkgs,addpkgs) classified
      _ -> mapM_ printInstalled classified
    return []
    else
    case select of
      All -> promptPkgs yes classified
      Ask -> mapMaybeM (rpmPrompt yes) classified
      PkgsReq subpkgs exceptpkgs exclpkgs addpkgs ->
        promptPkgs yes $
        selectRPMs prefix (subpkgs,exceptpkgs,exclpkgs,addpkgs) classified
  where
    installExists :: (Maybe FilePath,NVRA) -> IO (Maybe ExistPathNVRA)
    installExists (mfp,nvra) = do
      -- FIXME this will fail for noarch changes
      -- FIXME check kernel
      minstalled <- cmdMaybe "rpm" ["-q", rpmName nvra <.> rpmArch nvra]
      let existence =
            case minstalled of
              Nothing -> NotInstalled
              Just installed ->
                if showNVRA nvra `elem` lines installed
                then ExistingNVR
                else ChangedNVR
      return $
        case mstrategy of
          Just ExistingSkip | existence /= NotInstalled -> Nothing
          Just ExistingNoReinstall | existence == ExistingNVR -> Nothing
          _ -> Just (existence, mfp, nvra)

selectRPMs :: String
           -> ([String],[String],[String],[String]) -- (subpkgs,except,exclpkgs,addpkgs)
           -> [ExistPathNVRA] -> [ExistPathNVRA]
selectRPMs prefix (subpkgs,exceptpkgs,exclpkgs,addpkgs) rpms =
  let excluded = matchingRPMs prefix exclpkgs rpms
      included = matchingRPMs prefix addpkgs rpms
      matching =
        if null subpkgs && null exceptpkgs
        then defaultRPMs rpms
        else matchingRPMs prefix subpkgs rpms
      nonmatching = nonMatchingRPMs prefix exceptpkgs rpms
  in nubSort $ ((matching ++ nonmatching) \\ excluded) ++ included

isBinaryRpm :: NVRA -> Bool
isBinaryRpm = (/= "src") . rpmArch

renderInstalled :: ExistPathNVRA -> String
renderInstalled (exist, _path, nvra) =
  case exist of
    ExistingNVR -> '='
    ChangedNVR -> '^'
    NotInstalled -> '+'
  : showNVRA nvra

printInstalled :: ExistPathNVRA -> IO ()
printInstalled = putStrLn . renderInstalled

promptPkgs :: Yes -> [ExistPathNVRA]
           -> IO [ExistPathNVRA]
promptPkgs _ [] = error' "no rpms found"
promptPkgs yes classified = do
  mapM_ printInstalled classified
  ok <- prompt yes "install above"
  return $ if ok then classified else []

prompt :: Yes -> String -> IO Bool
prompt yes str = do
  if yes == Yes
    then return True
    else yesNoDefault True str

rpmPrompt :: Yes -> ExistPathNVRA -> IO (Maybe ExistPathNVRA)
rpmPrompt yes epn = do
  ok <- prompt yes $ renderInstalled epn
  return $
    if ok
    then Just epn
    else Nothing

defaultRPMs :: [ExistPathNVRA] -> [ExistPathNVRA]
defaultRPMs rpms =
  let installed = filter ((/= NotInstalled) . fst3) rpms
  in if null installed
     then rpms
     else installed

matchingRPMs :: String -> [String] -> [ExistPathNVRA] -> [ExistPathNVRA]
matchingRPMs prefix subpkgs rpms =
  nubSort . mconcat $
  flip map (nubOrd subpkgs) $ \ pkgpat ->
  case getMatches pkgpat of
    [] -> if headMay pkgpat /= Just '*'
          then
            case getMatches (prefix ++ '-' : pkgpat) of
              [] -> error' $ "no subpackage match for " ++ pkgpat
              result -> result
          else error' $ "no subpackage match for " ++ pkgpat
    result -> result
  where
    getMatches :: String -> [ExistPathNVRA]
    getMatches pkgpat =
      filter (match (compile pkgpat) . rpmName . thd3) rpms

nonMatchingRPMs :: String -> [String] -> [ExistPathNVRA] -> [ExistPathNVRA]
nonMatchingRPMs _ [] _ = []
nonMatchingRPMs prefix subpkgs rpms =
  -- FIXME somehow determine unused excludes
  nubSort $ foldl' (exclude (nubOrd subpkgs)) [] rpms
  where
    rpmnames = map (rpmName . thd3) rpms

    exclude :: [String] -> [ExistPathNVRA] -> ExistPathNVRA
            -> [ExistPathNVRA]
    exclude [] acc rpm = acc ++ [rpm]
    exclude (pat:pats) acc rpm =
        if checkMatch (rpmName (thd3 rpm))
        then acc
        else exclude pats acc rpm
      where
        checkMatch :: String -> Bool
        checkMatch rpmname =
          let comppat = compile pat
          in if isLiteral comppat
             then pat == rpmname ||
                  pat `notElem` rpmnames &&
                  (prefix ++ '-' : pat) == rpmname
             else match comppat rpmname

notDebugPkg :: String -> Bool
notDebugPkg p =
  not ("-debuginfo-" `isInfixOf` p || "-debugsource-" `isInfixOf` p)

data InstallType = ReInstall | Install

data PkgMgr = DNF3 | DNF5 | RPM | OSTREE
  deriving Eq

-- FIXME support options per build: install ibus imsettings -i plasma
-- (or don't error if multiple packages)
installRPMs :: Bool -> Bool -> Maybe PkgMgr -> Yes
            -> [ExistPathNVRA] -> IO ()
installRPMs _ _ _ _ [] = return ()
installRPMs dryrun debug mmgr yes classified = do
  case installTypes classified of
    ([],is) -> doInstall Install is
    (ris,is) -> do
      doInstall ReInstall (ris ++ is) -- include any new deps
      doInstall Install is            -- install any non-deps
  where
    doInstall :: InstallType -> [(Maybe FilePath,NVRA)] -> IO ()
    doInstall inst dirpkgs =
      unless (null dirpkgs) $ do
      mgr <-
        case mmgr of
          Just m -> return m
          Nothing -> do
            ostree <- doesDirectoryExist "/sysroot/ostree"
            if ostree
              then return OSTREE
              else do
              mdnf5 <- findExecutable "dnf5"
              return $ maybe DNF3 (const DNF5) mdnf5
      let pkgmgr =
            case mgr of
              DNF3 -> "dnf-3"
              DNF5 -> "dnf5"
              RPM -> "rpm"
              OSTREE -> "rpm-ostree"
          com =
            case inst of
              ReInstall -> reinstallCommand mgr
              Install -> installCommand mgr
        in
        if dryrun
        then mapM_ putStrLn $ ("would" +-+ unwords (pkgmgr : com) ++ ":") : map showRpmFile dirpkgs
        else do
          when debug $ mapM_ (putStrLn . showRpmFile) dirpkgs
          (case mgr of
            OSTREE -> cmd_
            _ -> sudo_) pkgmgr $
            com ++ map showRpmFile dirpkgs ++ ["--assumeyes" | yes == Yes && mgr `elem` [DNF3,DNF5]]

    installTypes :: [ExistPathNVRA]
                 -> ([(Maybe FilePath,NVRA)],[(Maybe FilePath,NVRA)])
    installTypes = partitionEithers . map partExist
      where
        partExist :: ExistPathNVRA
                  -> Either (Maybe FilePath,NVRA) (Maybe FilePath,NVRA)
        partExist (e,mdir,n) = combineExist e (mdir,n)

        combineExist e = if e == ExistingNVR then Left else Right

    reinstallCommand :: PkgMgr -> [String]
    reinstallCommand mgr =
      case mgr of
        DNF3 -> ["reinstall"]
        DNF5 -> ["reinstall"]
        RPM -> ["-Uvh","--replacepkgs"]
        OSTREE -> ["install"]

    installCommand :: PkgMgr -> [String]
    installCommand mgr =
      case mgr of
        DNF3 -> ["localinstall"]
        DNF5 -> ["install"]
        RPM -> ["-ivh"]
        OSTREE -> ["install"]

showRpm :: NVRA -> FilePath
showRpm nvra = showNVRA nvra <.> "rpm"

showRpmFile :: (Maybe FilePath,NVRA) -> FilePath
showRpmFile (mdir,nvra) = fromMaybe "" mdir </> showRpm nvra
