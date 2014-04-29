-- Via email from DRC...

data Flag = FlagX | FlagY | FlagZ

mutual
  data Flags : Type where
    Nil : Flags
    (::) : (f : Flag) -> (fs : Flags) -> {default () stupid : flagOK f fs} -> Flags

  flagOK : Flag -> Flags -> Type

  -- No flags set is ok.
  flagOK _ [] = ()

  flagOK FlagX (FlagX :: _) = _|_
  flagOK FlagX (FlagY :: _) = _|_
  flagOK FlagX (FlagZ :: _) = _|_
  flagOK FlagX (f :: fs) = flagOK FlagX fs

  flagOK FlagY (FlagX :: _) = _|_
  flagOK FlagY (FlagY :: _) = _|_
  flagOK FlagY (FlagZ :: _) = _|_
  flagOK FlagY (f :: fs) = flagOK FlagY fs

  flagOK FlagZ (FlagX :: _) = _|_
  flagOK FlagZ (FlagY :: _) = _|_
  flagOK FlagZ (FlagZ :: _) = _|_
  flagOK FlagZ (f :: fs) = flagOK FlagZ fs

  -- flagOK Z _ = ()



flags01 : Flags
flags01 = FlagX :: Nil

flags02 : Flags
flags02 = FlagY :: Nil

flags03 : Flags
flags03 = FlagZ :: Nil

-- Doesn't type check:
-- flags04 : Flags
-- flags04 = X :: Y :: Nil

-- Doesn't type check:
-- flags05 : Flags
-- flags05 = X :: Z :: Nil

-- Doesn't type check:
-- flags06 : Flags
-- flags06 = Y :: X :: Nil

-- Doesn't type check:
-- flags07 : Flags
-- flags07 = Z :: Y :: Nil

mutual
  data Blah : Type where
    MkBlah : {default () stupid : computeThing} -> Blah

  computeThing : Type
  computeThing = ()


-- Q1: Can the user define FlagX, FlagY, FlagZ, or can we write
-- a function to produce this type? Is this appropriate for a type provider?

-- Q2: Need help on the 'default' stuff in MkBlah.
