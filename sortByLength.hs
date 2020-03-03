
freqSort' :: String -> String
freqSort' a = concat . sortBy (comparing length) . group . sort $ a
