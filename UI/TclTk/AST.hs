-- | 
-- Not really Tcl AST. It's some small subset of Tcl/Tk which is used
-- for emitting code.
--
-- Unusual feature is type parameter which many data types have. It's
-- used to define callbacks. They depend on some haskell value and
-- parameter is type of that value. Lambdas are defined as special
-- constructors. It's certainl possible to wrap AST into reader monad
-- but result deemed to unvieldy.
module UI.TclTk.AST (
    -- * AST
    Tcl(..)
  , Expr(..)
  , TkName(..)
  , castFrom_
    -- ** Extra types
  , Option(..)
  , Color(..)
  , Grid(..)
  , Pack(..)
  , PackAnchor(..)
  , PackFill(..)
  , PackSide(..)
    -- * Rendering
  , renderTcl
  , renderTclParam
    -- ** Convert to AST
  , renderPack
  , renderOption
  ) where

import Data.String
import Data.Functor.Contravariant
import Text.Printf



-- | Single Tcl statement/expression.
data Tcl a
  = Stmt [Expr a]               -- ^ Single statement
  | Lam  (a -> [Tcl a])         -- ^ Lambda expression

-- | Tcl expression
data Expr a
  -- FIXME: LamE (a -> [Expr a]) ???
  = Name    String              -- ^ Simple name
  | WName   TkName              -- ^ Name of Tk widget
  | SubVar  String              -- ^ Variable substitution

  | LitStr  String              -- ^ Literal string
  | LitInt  Int                 -- ^ Literal integer
  | LitReal Double              -- ^ Literal floating point value
  | LitBool Bool                -- ^ Literal boolen

  | Braces  [Expr a]            -- ^ Braces {...}
  | BracesS [Tcl  a]            -- ^ Braces for statements
  | Eval    [Expr a]            -- ^ Square brackets
  | LamE    (a -> Expr a)       -- ^ Lambda expression
  | SeqE    [Expr a]            -- ^ Sequence of expressions
    

-- | Name of Tk widgets
newtype TkName = TkName [String]

-- | Widget options. It's enumeration of all possible options. Some
--   could be invalid for some widgets.
data Option a
  = Text    String
  | Width   Int
  | Height  Int
  | Padding Int
  | TextVariable String
  | Foreground   Color
  | Background   Color
  | LamOpt (a -> Option a)

-- | Colors known by Tcl/Tk
data Color
  = Color String      -- ^ Symbolic name for color
  | RGB   Int Int Int -- ^ RGB values


-- | Information for grid geometry manager: @Grid column row@
data Grid
  = Grid Int Int

-- | Packing for widgets
data Pack 
  = Side   PackSide
  | Fill   PackFill
  | Anchor PackAnchor
  | Expand Bool

data PackAnchor
  = AnchorN
  | AnchorNE
  | AnchorE
  | AnchorSE
  | AnchorS
  | AnchorSW
  | AnchorW
  | AnchorNW
  | AnchorCenter

data PackSide
  = PackTop
  | PackLeft
  | PackRight
  | PackBottom

-- | Fill options
data PackFill 
  = FillNone
  | FillX
  | FillY
  | FillBoth

instance Contravariant Tcl where
  contramap f (Stmt es) = Stmt $ map (contramap f) es
  contramap f (Lam  l ) = Lam  $ map (contramap f) . l . f

instance Contravariant Expr where
  contramap _ (Name    s) = Name   s
  contramap _ (WName   s) = WName  s
  contramap _ (SubVar  s) = SubVar s
  contramap f (Eval    e) = Eval    $ map (contramap f) e
  contramap f (Braces  e) = Braces  $ map (contramap f) e
  contramap f (BracesS s) = BracesS $ map (contramap f) s
  contramap _ (LitStr  s) = LitStr  s
  contramap _ (LitInt  i) = LitInt  i
  contramap _ (LitReal x) = LitReal x
  contramap _ (LitBool x) = LitBool x
  contramap f (LamE lam)  = LamE $ contramap f . lam . f
  contramap f (SeqE es )  = SeqE $ map (contramap f) es
  
instance IsString (Expr a) where
  fromString = LitStr

-- | Change type
castFrom_ :: Contravariant f => f () -> f b
castFrom_ = contramap (const ())



----------------------------------------------------------------
-- Render Tcl to string
----------------------------------------------------------------

-- | Convert unparametrized Tcl to strings
renderTcl :: Tcl () -> [String]
renderTcl = workerTcl 0 ()

-- | Convert parametrized Tcl to strings
renderTclParam :: Tcl a -> a -> [String]
renderTclParam tcl x = workerTcl 0 x tcl

-- Convert Tcl code to strings
workerTcl :: Int -> a -> Tcl a -> [String]
workerTcl n x (Stmt es) = [pref n ++ unwords (map (renderExpr n x) es)]
workerTcl n x (Lam lam) = workerTcl n x =<< lam x

-- Convert Tcl expressions to strings
renderExpr :: Int -> a -> Expr a -> String
renderExpr _ _ (Name   s)           = s
renderExpr _ _ (WName  (TkName ss)) = ('.':) =<< ss
renderExpr _ _ (SubVar s)           = '$':s
renderExpr n x (Eval   e) = "[ " ++ unwords (map (renderExpr n x) e) ++ " ]"
renderExpr n x (Braces e) = "{ " ++ unwords (map (renderExpr n x) e) ++ " }"
renderExpr n x (BracesS s) 
  =  unlines
  $  (pref n ++ "{")
  :  concatMap (workerTcl (n+4) x) s
  ++ [pref n ++ "{"]
-- Literals
renderExpr _ _ (LitStr  s) = '"' : (escape =<< s) ++ "\""
  where
    escape '\\' = "\\\\"
    escape '"'  = "\\\""
    escape '$'  = "\\$"
    escape '['  = "\\["
    escape ']'  = "\\]"
    escape  c   = [c]
renderExpr _ _ (LitInt  i) = show i
renderExpr _ _ (LitReal x) = show x
renderExpr _ _ (LitBool b) = if b then "1" else "0"
-- Lambda
renderExpr n x (LamE lam)  = renderExpr n x (lam x)
renderExpr n x (SeqE es )  = unwords $ map (renderExpr n x) es

pref :: Int -> String
pref n = replicate n ' '



----------------------------------------------------------------
-- Convert to AST
----------------------------------------------------------------

-- | Convert packing options with 
renderPack :: Pack -> [Expr a]
renderPack (Anchor a) = [ Name "-anchor", Name anch ]
  where anch = case a of
                 AnchorN      -> "n"
                 AnchorNE     -> "ne"
                 AnchorE      -> "e"
                 AnchorSE     -> "se"
                 AnchorS      -> "s"
                 AnchorSW     -> "sw"
                 AnchorW      -> "w"
                 AnchorNW     -> "nw"
                 AnchorCenter -> "center"
renderPack (Side s) = [ Name "-side", Name side ]
  where side = case s of
          PackTop    -> "top"
          PackLeft   -> "left"
          PackRight  -> "right"
          PackBottom -> "bottom"
renderPack (Fill x) = [ Name "-fill", Name a ]
  where a = case x of
          FillNone -> "none"
          FillX    -> "x"
          FillY    -> "y"
          FillBoth -> "both"
renderPack (Expand f) = [ Name "-expand", LitInt $ if f then 1 else 0 ]

-- | Convert options to expressions
renderOption :: Option a -> [Expr a]
renderOption (Text    s) = [Name "-text"   , LitStr s]
renderOption (Width   n) = [Name "-width"  , LitInt n]
renderOption (Height  n) = [Name "-height" , LitInt n]
renderOption (Padding n) = [Name "-padding", LitInt n]
renderOption (TextVariable v) = [ Name "-textvariable", Name v]
renderOption (Foreground   c) = [ Name "-foreground"  , color c ]
renderOption (Background   c) = [ Name "-background"  , color c ]
renderOption (LamOpt  f) = [LamE $ SeqE . renderOption . f ]

color :: Color -> Expr a
color (Color s)   = Name s
color (RGB r g b) = Name $ "#" ++ toS r ++ toS g ++ toS b
  where
    toS i | i < 0     = "00"
          | i >= 255  = "ff"
          | otherwise = printf "%02x" i
