module Types where

data Anniversary = Birthday Name Date | Weding Name Name Date
data Date = Date Year Month Day

type Name = String
type Year = Int
type Month = Int
type Day = Int


showAnniversary :: Anniversary -> String
showAnniversary (Birthday name date) = name ++ " was born on " ++ showDate date
showAnniversary (Weding name1 name2 date) = name1 ++ " and " ++ name2 ++ " got married on " ++ showDate date

showDate :: Date -> String
showDate (Date y m d) = show y ++ "-" ++ show m ++ "-" ++ show d

