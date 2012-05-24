-- | Something that resemble Tcl AST.
module UI.TclTk.AST (
    Tcl(..)
  , Expr(..)
  , TkName(..)
  , castFrom_
    -- ** Extra types
  , Option(..)
  , Pack(..)
  , PackSide(..)
  , PackFill(..)
    -- * Rendering
  , renderTcl
  , renderTclParam
  , renderPack
  , renderOption
  ) where

import Control.Reactive.Cofunctor
import Data.String



-- | Single Tcl statement/expression
data Tcl a
  = Stmt [Expr a]               -- ^ Single statement
  | Lam  (a -> [Tcl a])

-- | Tcl expression
data Expr a
  = Name    String              -- ^ Simple name
  | WName   TkName              -- ^ Name of Tk widget
  | SubVar  String              -- ^ Variable substitution

  | LitStr  String              -- ^ Literal string
  | LitInt  Int                 -- ^ Literal integer
  | LitReal Double              -- ^ Literal floating point value

  | Braces  [Expr a]            -- ^ Braces {...}
  | BracesS [Tcl  a]            -- ^ Braces for statement
  | Eval    [Expr a]            -- ^ Square brackets
  | LamE    (a -> Expr a)
  | SeqE    [Expr a]

-- | Name of Tk widgte
newtype TkName = TkName [String]

-- | Widget options
data Option a
  = Text   String
  | Width  Int
  | Height Int
  | LamOpt (a -> Option a)

-- | Packing for widgets
data Pack 
  = Side   PackSide
  | Fill   PackFill
  | Expand Bool

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

instance Cofunctor Tcl where
  cofmap f (Stmt es) = Stmt $ map (cofmap f) es
  cofmap f (Lam  l ) = Lam  $ map (cofmap f) . l . f

instance Cofunctor Expr where
  cofmap _ (Name    s) = Name   s
  cofmap _ (WName   s) = WName  s
  cofmap _ (SubVar  s) = SubVar s
  cofmap f (Eval    e) = Eval    $ map (cofmap f) e
  cofmap f (Braces  e) = Braces  $ map (cofmap f) e
  cofmap f (BracesS s) = BracesS $ map (cofmap f) s
  cofmap _ (LitStr  s) = LitStr  s
  cofmap _ (LitInt  i) = LitInt  i
  cofmap _ (LitReal x) = LitReal x
  cofmap f (LamE lam)  = LamE $ cofmap f . lam . f
  cofmap f (SeqE es )  = SeqE $ map (cofmap f) es
  
instance IsString (Expr a) where
  fromString = LitStr

castFrom_ :: Cofunctor f => f () -> f b
castFrom_ = cofmap (const ())

----------------------------------------------------------------
-- 
----------------------------------------------------------------

-- | Convert packing options with 
renderPack :: Pack -> [Expr a]
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
renderOption (Text   s) = [Name "-text"  , LitStr s]
renderOption (Width  n) = [Name "-width" , LitInt n]
renderOption (Height n) = [Name "-height", LitInt n]
renderOption (LamOpt f) = [LamE $ SeqE . renderOption . f ]



----------------------------------------------------------------

renderTcl :: Tcl () -> [String]
renderTcl = workerTcl 0 ()

renderTclParam :: Tcl a -> a -> [String]
renderTclParam tcl x = workerTcl 0 x tcl

-- | Render Tcl code to strings
workerTcl :: Int -> a -> Tcl a -> [String]
workerTcl n x (Stmt es) = [pref n ++ unwords (map (renderExpr n x) es)]
workerTcl n x (Lam lam) = workerTcl n x =<< lam x


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
    escape '"' = "\\\""
    escape '$' = "\\$"
    escape '[' = "\\["
    escape ']' = "\\]"
    escape  c  = [c]
renderExpr _ _ (LitInt  i) = show i
renderExpr _ _ (LitReal x) = show x
-- Lambda
renderExpr n x (LamE lam)  = renderExpr n x (lam x)
renderExpr n x (SeqE es )  = unwords $ map (renderExpr n x) es

pref :: Int -> String
pref n = replicate n ' '
