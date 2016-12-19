import Data.Time.Clock (DiffTime, UTCTime, picosecondsToDiffTime,
                        secondsToDiffTime)
import Data.Time.Clock.POSIX (POSIXTime, posixSecondsToUTCTime)
import Data.Time.Format (formatTime)
import System.Locale (defaultTimeLocale)

-- DiffTime represents duration
t1, t2, t3 :: DiffTime
t1 = 10 :: DiffTime
t2 = fromRational 1.23 :: DiffTime
t3 = secondsToDiffTime 20
t4 = picosecondsToDiffTime 20

-- POSIXTime is type synonym for NominalDiffTime, which is like DiffTime, but with implicit stating point
t5 :: POSIXTime
t5 = 10 :: POSIXTime --10 sec after EPOCH (1.1.1970)

-- UTCTime (Coordinated Universal Time) is main type for dealing with time in Haskell
t6 :: UTCTime
t6 = posixSecondsToUTCTime t5

fmt = formatTime defaultTimeLocale "%c" t6
