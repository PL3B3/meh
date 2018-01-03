data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday deriving (Eq, Ord, Show, Read, Bounded, Enum)

next :: (Enum a, Bounded a) => a -> a
--next e = eList !! rem (fromEnum e + 1 + eLength) eLength
  --where eList = enumFrom (asTypeOf minBound e)
    --    eLength = length eList
next e
  | fromEnum e == (length (enumFrom (asTypeOf minBound e))) - 1 = toEnum 0
  | otherwise = succ e

prev :: (Enum a, Bounded a) => a -> a
prev e
  | fromEnum e == 0 = toEnum $ (length (enumFrom (asTypeOf minBound e))) - 1
  | otherwise = pred e
