-- Assemble command for connecting to windows slave
-- Some credentials documented HERE: https://mojo.redhat.com/docs/DOC-951980
-- Intended usage: runghc cnwin.hs dev110
import Data.List (isPrefixOf)
import System.Environment (getArgs)
import Text.Printf (printf)

main = do
  args <- getArgs
  putStrLn $ case args of
    [label, username] -> assembleCommand label username
    _                 -> "Usage: cnwin <hostname> <username>"

assembleCommand :: String -> String -> String
assembleCommand label user = unwords [command label, resolution, credentials user label, getDomain label, getHost label]

command label
  | label `elem` win8 = "xfreerdp --plugin cliprdr"
  | otherwise         = "rdesktop -r clipboard:PRIMARYCLIPBOARD"
  where win8 = ["soa7", "vmg15"]

resolution = "-g 1920x1024"

credentials :: String -> String -> String
credentials user label = printf "-u %s -p %s" user passwd
  where passwd = case user of
          "hudson" | inMsDomain label -> "jboss42"
                   | otherwise        -> "JBossCITool"
          "jbossqa" -> "jboss42"
          _         -> error $ "Unknown user " ++ user

-- Machines that are in windows domain need additional -d <domain> (see https://mojo.redhat.com/docs/DOC-951980)
getDomain label = case label of
  "dev101" -> "-d jboss1"
  "dev156" -> "-d jboss3"
  "vmg13"  -> "-d dc2008"
  "vmg48"  -> "-d dc2012"
  x | inMsDomain x -> "-d msdomain"
    | otherwise    -> "" --the rest of machines is not in domain

inMsDomain :: String -> Bool
inMsDomain label = label `elem` ["vmg01", "vmg04", "vmg05", "vmg43", "vmg44", "vmg45", "vmg49"]

getHost label
  | hasLongerHostname = label ++ "-w2012-x86-64.mw.lab.eng.bos.redhat.com"
  | otherwise         = label ++              ".mw.lab.eng.bos.redhat.com"
  where hasLongerHostname = "dev15" `isPrefixOf` label || "vmg0" `isPrefixOf` label

knownWinMachines = ["dev157","dev158","dev156","vmg01","vmg02","vmg04","vmg05","vmg06","brms01","dev101","dev102","dev103","dev104","dev110","dev16","dev29","dev96","dev97","dev98","dev99","dspvmg04","dspvmg05","soa5","soa7","vmg13","vmg14","vmg15","vmg26","vmg42","vmg43","vmg44","vmg45","vmg48","vmg49", "brms06", "brms07"]

test user = mapM_ (\host -> putStrLn $ assembleCommand host user) knownWinMachines

{-
label    : os.name      :IE version : hostname

brms01   :Windows Server 2012   : 11:              brms01
dev101   :Windows Server 2008 R2: 11:              dev101
dev102   :Windows Server 2008 R2: 11:              dev102
dev103   :Windows Server 2008   :  9:              dev103
dev104   :Windows Server 2008   :  9:              dev104
dev110   :Windows 7             : 11:              dev110
dev156   :Windows Server 2012   : 11: dev156-w2012-x86-64
dev157   :Windows Server 2012   : 11: Dev157-w2012-x86-64
dev158   :Windows Server 2012   : 11: Dev158-w2012-x86-64
dev16    :Windows 2003          :  6:               dev16
dev29    :Windows 2003          :  7:               dev29
dev96    :Windows Server 2008   :  8:               dev96
dev97    :Windows Server 2008   :  8:               dev97
dev98    :Windows Server 2008 R2: 10:               dev98
dev99    :Windows Server 2008 R2: 10:               dev99
dspvmg04 :Windows 2003          :  6:            dspvmg04
dspvmg05 :Windows 2000          :N/A:            dspvmg05
soa5     :Windows 2003          :  7:                soa5
soa7     :Windows 8             : 11:                soa7
vmg01    :Windows Server 2012   : 11:  vmg01-w2012-x86-64
vmg02    :Windows Server 2012   : 11:  vmg02-w2012-x86-64
vmg04    :Windows Server 2012   : 10:  vmg04-w2012-x86-64
vmg05    :Windows Server 2012   : 10:  vmg05-w2012-x86-64
vmg06    :Windows Server 2012   : 10:  vmg06-w2012-x86-64
vmg13    :Windows Server 2008   :  9:               vmg13
vmg14    :Windows Server 2008   :  7:               vmg14
vmg15    :Windows 8             : 11:               vmg15
vmg26    :Windows 7             :  8:               vmg26
vmg42    :Windows Server 2008 R2: 11:               vmg42
vmg43    :Windows Server 2008   :N/A:               vmg43
vmg44    :Windows Server 2008 R2:N/A:               vmg44
vmg45    :Windows Server 2008   :  8:               vmg45
vmg48    :Windows Server 2012   : 10:               vmg48
vmg49    :Windows Server 2012   : 10:               vmg49
Failed getting infor about dspvmg06-win2k8
Failed getting infor about vmg03-w2012-x86-64
-}
