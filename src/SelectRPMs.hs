{-# LANGUAGE CPP, TupleSections #-}

module SelectRPMs (
  Select(..),
  selectDefault,
  selectRpmsOption,
  installArgs,
  checkSelection,
  rpmsToNVRAs,
  Existence(..),
  ExistNVRA,
  Yes(..),
  ExistingStrategy(..),
  decideRPMs,
  nvraToRPM,
  groupOnArch,
  PkgMgr(..),
  installRPMs
  )
where

import Control.Monad.Extra (forM_, mapMaybeM, unless, when)
import Data.Either (partitionEithers)
import Data.List.Extra (foldl', groupOnKey, isInfixOf, nubOrd, nubSort, sort,
                        (\\))
import Data.RPM.NVRA (NVRA(..), readNVRA, showNVRA)
import Safe (headMay)
import SimpleCmd (cmd_, cmdMaybe, error', sudo_, (+-+),
#if MIN_VERSION_simple_cmd(0,2,7)
                  sudoLog
#endif
                 )
import SimpleCmdArgs (Parser, flagLongWith', many, strOptionWith, (<|>))
import SimplePrompt (yesNoDefault)
import System.Directory
import System.FilePath ((</>), (<.>))
import System.FilePath.Glob (compile, isLiteral, match)

-- | The Select type specifies the subpackage selection
data Select = All -- ^ all packages
            | Ask -- ^ interactive prompting
            | PkgsReq
              [String] -- ^ include matches
              [String] -- ^ except matches
              [String] -- ^ exclude
              [String] -- ^ added
  deriving Eq

-- | default package selection
selectDefault :: Select
selectDefault = PkgsReq [] [] [] []

-- | optparse-applicative Parser for Select
selectRpmsOption :: Parser Select
selectRpmsOption =
  flagLongWith' All "all" "all subpackages [default if not installed]" <|>
  flagLongWith' Ask "ask" "ask for each subpackage" <|>
  PkgsReq
  <$> many (strOptionWith 'p' "package" "SUBPKG" "select subpackage (glob) matches")
  <*> many (strOptionWith 'e' "except" "SUBPKG" "select subpackages not matching (glob)")
  <*> many (strOptionWith 'x' "exclude" "SUBPKG" "deselect subpackage (glob): overrides -p and -e")
  <*> many (strOptionWith 'i' "include" "SUBPKG" "additional subpackage (glob) to install: overrides -x")

-- | alternative CLI args option parsing to Select packages
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

-- FIXME explain if/why this is actually needed (used by koji-tool install)
-- | check package Select is not empty
checkSelection :: Monad m => Select -> m ()
checkSelection (PkgsReq ps es xs is) =
  forM_ (ps ++ es ++ xs ++ is) $ \s ->
  when (null s) $ error' "empty package pattern not allowed"
checkSelection _ = return ()

-- | converts a list of RPM files to NVRA's, filtering out debug subpackages
rpmsToNVRAs :: [String] -> [NVRA]
rpmsToNVRAs = sort . map readNVRA . filter notDebugPkg

-- | how to handle already installed packages: re-install, skip, or
-- default update
--
-- The default strategy is to select existing subpackages, otherwise all.
data ExistingStrategy = ExistingNoReinstall | ExistingSkip | ExistingOnly
  deriving Eq

-- | sets prompt default behaviour for yes/no questions
data Yes = No | Yes
  deriving Eq

-- | current state of a package NVR
data Existence = ExistingNVR -- ^ NVR is already installed
               | ChangedNVR -- ^ NVR is different to installed package
               | NotInstalled -- ^ package is not currently installed
  deriving (Eq, Ord, Show)

-- | combines Existence state with an NVRA
type ExistNVRA = (Existence, NVRA)

-- FIXME determine and add missing internal deps
-- | decide list of NVRs based on a Select selection (using a package prefix)
decideRPMs :: Yes -- ^ prompt default choice
           -> Bool -- ^ enable list mode which just display the package list
           -> Maybe ExistingStrategy -- ^ optional existing install strategy
           -> Select -- ^ specifies package Select choices
           -> String -- ^ package set prefix: allows abbreviated Select
           -> [NVRA] -- ^ list of packages to select from
           -> IO [ExistNVRA] -- ^ returns list of selected packages
decideRPMs yes listmode mstrategy select prefix nvras = do
  classified <- mapMaybeM installExists (filter isBinaryRpm nvras)
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
      All -> promptPkgs mstrategy yes classified
      Ask -> mapMaybeM (rpmPrompt yes) classified
      PkgsReq subpkgs exceptpkgs exclpkgs addpkgs ->
        promptPkgs mstrategy yes $
        selectRPMs prefix (subpkgs,exceptpkgs,exclpkgs,addpkgs) classified
  where
    installExists :: NVRA -> IO (Maybe ExistNVRA)
    installExists nvra = do
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
          Just ExistingOnly | existence == NotInstalled -> Nothing
          _ -> Just (existence, nvra)

-- FIXME move to submodule?
selectRPMs :: String
           -- (subpkgs,except,exclpkgs,addpkgs)
           -> ([String],[String],[String],[String])
           -> [ExistNVRA] -> [ExistNVRA]
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

renderInstalled :: ExistNVRA -> String
renderInstalled (exist, nvra) =
  case exist of
    ExistingNVR -> '='
    ChangedNVR -> '^'
    NotInstalled -> '+'
  : showNVRA nvra

printInstalled :: ExistNVRA -> IO ()
printInstalled = putStrLn . renderInstalled

promptPkgs :: Maybe ExistingStrategy -> Yes -> [ExistNVRA] -> IO [ExistNVRA]
promptPkgs (Just ExistingOnly) _ [] = do
  putStrLn "skipped"
  return []
promptPkgs _ _ [] = error' "no rpms found"
promptPkgs _ yes classified = do
  mapM_ printInstalled classified
  ok <- prompt yes "install above"
  return $ if ok then classified else []

prompt :: Yes -> String -> IO Bool
prompt yes str = do
  if yes == Yes
    then return True
    else yesNoDefault True str

rpmPrompt :: Yes -> ExistNVRA -> IO (Maybe ExistNVRA)
rpmPrompt yes epn = do
  ok <- prompt yes $ renderInstalled epn
  return $
    if ok
    then Just epn
    else Nothing

defaultRPMs :: [ExistNVRA] -> [ExistNVRA]
defaultRPMs rpms =
  let installed = filter ((/= NotInstalled) . fst) rpms
  in if null installed
     then rpms
     else installed

matchingRPMs :: String -> [String] -> [ExistNVRA] -> [ExistNVRA]
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
    getMatches :: String -> [ExistNVRA]
    getMatches pkgpat =
      filter (match (compile pkgpat) . rpmName . snd) rpms

nonMatchingRPMs :: String -> [String] -> [ExistNVRA] -> [ExistNVRA]
nonMatchingRPMs _ [] _ = []
nonMatchingRPMs prefix subpkgs rpms =
  -- FIXME somehow determine unused excludes
  nubSort $ foldl' (exclude (nubOrd subpkgs)) [] rpms
  where
    rpmnames = map (rpmName . snd) rpms

    exclude :: [String] -> [ExistNVRA] -> ExistNVRA
            -> [ExistNVRA]
    exclude [] acc rpm = acc ++ [rpm]
    exclude (pat:pats) acc rpm =
        if checkMatch (rpmName (snd rpm))
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

-- | whether a package needs to be reinstalled or installed
data InstallType = ReInstall
                 | Install

-- | package manager
data PkgMgr = DNF3 | DNF5 | RPM | OSTREE
  deriving Eq

-- FIXME support options per build: install ibus imsettings -i plasma
-- (or don't error if multiple packages)
-- | do installation of packages
installRPMs :: Bool -- ^ dry-run
            -> Bool -- ^ debug output
            -> Maybe PkgMgr -- ^ optional specify package manager
            -> Yes -- ^ prompt default choice
            -> [(FilePath,[ExistNVRA])] -- ^ list of rpms to install with path
            -> IO ()
installRPMs _ _ _ _ [] = return ()
installRPMs dryrun debug mmgr yes classifieds = do
  case installTypes (concatMap zipDir classifieds) of
    ([],is) -> doInstall Install is
    (ris,is) -> do
      doInstall ReInstall (ris ++ is) -- include any new deps
      doInstall Install is            -- install any non-deps
  where
    zipDir :: (FilePath,[ExistNVRA]) -> [(FilePath,ExistNVRA)]
    zipDir (dir, rpms) = map (dir,) rpms

    installTypes :: [(FilePath,ExistNVRA)]
                 -> ([(FilePath,NVRA)],[(FilePath,NVRA)])
    installTypes = partitionEithers . map partExist
      where
        partExist :: (FilePath,ExistNVRA)
                  -> Either (FilePath,NVRA) (FilePath,NVRA)
        partExist (d,(e,n)) = (if e == ExistingNVR then Left else Right) (d,n)

    doInstall :: InstallType -> [(FilePath,NVRA)] -> IO ()
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
            _ -> if debug then sudoLog else sudo_) pkgmgr $
            com ++ map showRpmFile dirpkgs ++ ["--assumeyes" | yes == Yes && mgr `elem` [DNF3,DNF5]]

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

-- FIXME replace with export from rpm-nvr (once released)
-- | render a NVRA as rpm file
nvraToRPM :: NVRA -> FilePath
nvraToRPM nvra = showNVRA nvra <.> "rpm"

-- | render path and NVRA are rpm filepath
showRpmFile :: (FilePath,NVRA) -> FilePath
showRpmFile (dir,nvra) = dir </> nvraToRPM nvra

-- | group rpms by arch (subdirs)
groupOnArch :: FilePath -- ^ prefix directory (eg "RPMS")
            -> [ExistNVRA]
            -> [(FilePath,[ExistNVRA])]
groupOnArch dir = groupOnKey (\(_,p) -> dir </> rpmArch p)

#if !MIN_VERSION_simple_cmd(0,2,7)
sudoLog :: String -- ^ command
     -> [String] -- ^ arguments
     -> IO ()
sudoLog = sudo_
#endif
