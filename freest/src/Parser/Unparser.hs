module Parser.Unparser
  ( Unparse(..)
  )
  where

import Syntax.Base
import Syntax.Kind qualified as K
import Syntax.Type.Internal qualified as T
import Syntax.Refinement qualified as R

import Data.List qualified as List

data Precedence =
    PMin
  | PDot
  | PIff
  | PArrow
  | PSemi
  | PPipePipe
  | PAmpAmp
  | PCmp
  | PSumSub
  | PProdDiv
  | PNot
  | PMsg
  | PApp
  | PMax
  deriving (Eq, Ord, Bounded)

data Associativity = LeftAssoc | RightAssoc | NonAssoc deriving (Eq)

type Rator = (Precedence, Associativity)

type Fragment = (Rator, String)

minRator
  , dotRator
  , iffRator
  , arrowRator
  , semiRator
  , pipePipeRator
  , ampAmpRator
  , cmpRator
  , sumSubRator
  , prodDivRator
  , notRator
  , msgRator
  , appRator
  , maxRator
  :: Rator
minRator      = (minBound , NonAssoc)
dotRator      = (PDot     , RightAssoc)
iffRator      = (PIff     , LeftAssoc)
arrowRator    = (PArrow   , RightAssoc)
semiRator     = (PSemi    , RightAssoc)
pipePipeRator = (PPipePipe, LeftAssoc)
ampAmpRator   = (PAmpAmp  , LeftAssoc)
cmpRator      = (PCmp     , NonAssoc)
sumSubRator   = (PSumSub  , LeftAssoc)
prodDivRator  = (PProdDiv , LeftAssoc)
notRator      = (PNot     , LeftAssoc)
msgRator      = (PMsg     , RightAssoc)
appRator      = (PApp     , LeftAssoc)
maxRator      = (maxBound , NonAssoc)

noparens :: Rator -> Rator -> Associativity -> Bool
noparens (pi, ai) (po, ao) side = pi > po || pi == po && ai == ao && ao == side

bracket :: Fragment -> Associativity -> Rator -> String
bracket (inner, image) side outer
  | noparens inner outer side = image
  | otherwise = "(" ++ image ++ ")"

class Unparse t where
  fragment :: t -> Fragment

  unparse :: t -> String
  unparse = snd . fragment

instance Unparse K.Kind where
  fragment = \case
    K.Proper _ m pk -> (maxRator, show m ++ show pk)
    K.Arrow _ k1 k2 -> (arrowRator, l ++ " -> " ++ r)
      where
        l = bracket (fragment k1) LeftAssoc arrowRator
        r = bracket (fragment k2) RightAssoc arrowRator
    K.Var _ τ       -> (maxRator, show τ)

instance Unparse Variable where
  fragment a = (maxRator, external a)

instance Unparse (T.Type x) where
  fragment = \case
    T.Int _ _ r -> (maxRator, refinement "Int" r)
    T.Float _ _ -> (maxRator, "Float")
    T.Char _ _ -> (maxRator, "Char")
    T.Arrow _ _ m -> (maxRator, "(" ++ arrow m ++ ")")
    T.Quant _ _ p pk -> (maxRator, "(" ++ quant True p pk ++ ")")
    T.Skip _ _ -> (maxRator, "Skip")
    T.End _ _ p -> (maxRator, case p of T.Out -> "Close"
                                        T.In  -> "Wait")
    T.Message _ _ m p -> (maxRator, "(" ++ multiplicity m ++ polarity p ++ ")")
    T.Choice _ _ m p is ->
      (maxRator, multiplicity m ++ view p ++ "{" ++ fields ++ "}")
      where
        fields = List.intercalate ", " (map show is)
    T.Semi _ _ -> (maxRator, "(;)")
    T.Dual _ _ -> (maxRator, "Dual")
    T.TName _ _ i -> (maxRator, show i)
    T.DName _ _ i -> (maxRator, show i)
    T.Void _ _ k -> (appRator, "Void @" ++ r)
      where
        r = bracket (fragment k) RightAssoc appRator
    T.Var  _ _ a -> fragment a
    T.Abs _ _ aks t -> (dotRator, "\\" ++ bindings aks ++ " -> " ++ unparse t)
    T.AppArrow _ _ _ m t u   -> (arrowRator, l ++ " " ++ arrow m ++ " " ++ r)
      where
        l = bracket (fragment t) LeftAssoc arrowRator
        r = bracket (fragment u) RightAssoc arrowRator
    T.AppQuant _ _ _ _ p pk aks t ->
      (dotRator, quant False p pk ++ bindings aks ++ ". " ++ unparse t)
    T.Tuple _ _ _ ts ->
      (maxRator, "(" ++ List.intercalate ", " (map unparse ts) ++ ")")
    T.List _ _ _ t ->
      (maxRator, "[" ++ unparse t ++ "]")
    T.AppMessage _ _ _ m p t ->
      (msgRator, multiplicity m ++ polarity p ++ bracket (fragment t) RightAssoc msgRator)
    T.AppQuantS _ _ _ _ p a k t ->
      (dotRator, polarity p ++ polarity p ++ bindings [(a, k)] ++ ". " ++ unparse t)
    T.AppLinChoice _ _ _ p lts ->
      (maxRator, view p ++ "{" ++ fields lts ++ "}")
      where
        fields = List.intercalate ", "
               . map (\(l, t) -> show l ++ ": " ++ unparse t)
    T.AppSemi _ _ _ t u -> (semiRator, l ++ "; " ++ r)
      where
        l = bracket (fragment t) LeftAssoc semiRator
        r = bracket (fragment u) RightAssoc semiRator
    T.App s x t ts -> (appRator, l ++ " " ++ r)
      where
        l = bracket (fragment (if length ts == 1 then t
                                                 else T.App s x t (init ts)))
                    LeftAssoc appRator
        r = bracket (fragment (last ts)) RightAssoc appRator
    where
      quant prefix = \cases
        T.In  K.Top     -> "forall" ++ if prefix then "" else " "
        T.Out K.Top     -> "exists" ++ if prefix then "" else " "
        T.In  K.Session -> "??"
        T.Out K.Session -> "!!"
      arrow = \case
        K.Lin    -> "1->"
        K.Un     -> "->"
        K.VarM φ -> external φ ++ "->"
      multiplicity = \case
        K.Lin    -> ""
        K.Un     -> "*"
        K.VarM φ -> external φ
      polarity = \case
        T.In  -> "?"
        T.Out -> "!"
      bindings =
        unwords . map \(a, k) -> "(" ++ external a ++ " : " ++ unparse k ++ ")"
      view = \case
        T.In  -> "&"
        T.Out -> "+"
      refinement typ = \case
        R.Unrefined -> typ
        R.Refined v t p -> unparse $ R.Refined v t p

instance Unparse R.Refinement where
  fragment r = (maxRator, unparsed r)
    where
      unparsed = \case
        R.Refined v t p -> "{" ++ unparse v ++ ": " ++ unparse t ++ " | " ++ unparse p ++ "}"
        R.Unrefined -> ""

instance Unparse R.RefinementType where
  fragment t = (maxRator, unparsed t)
    where
      unparsed = \case
        R.RefinedInt -> "Int"

instance Unparse R.Predicate where
  fragment = \case
    R.PredicateComparison p1 cmp p2 -> (cmpRator, l p1 cmpRator ++ " " ++ unparse cmp ++ " " ++ r p2 cmpRator)
    R.PredicateAnd p1 p2 -> (ampAmpRator, l p1 ampAmpRator ++ " && " ++ r p2 ampAmpRator)
    R.PredicateOr p1 p2 -> (pipePipeRator, l p1 pipePipeRator ++ " || " ++ r p2 pipePipeRator)
    R.PredicateImplies p1 p2 -> (arrowRator, l p1 arrowRator ++ " => " ++ r p2 arrowRator)
    R.PredicateIff p1 p2 -> (iffRator, l p1 iffRator ++ " <=> " ++ r p2 iffRator)
    R.PredicateNot p -> (notRator, "not " ++ r p notRator)
    R.PredicateTrue -> (maxRator, "True")
    R.PredicateFalse -> (maxRator, "False")
    R.PredicateParens p -> (maxRator, "(" ++ unparse p ++ ")")
    where
      l p = bracket (fragment p) LeftAssoc
      r p = bracket (fragment p) RightAssoc

instance Unparse R.PredicateExpression where
  fragment = \case
    R.ExpressionVariable x -> (maxRator, unparse x)
    R.ExpressionConstant c -> (maxRator, unparse c)
    R.ExpressionSum e1 e2 -> (sumSubRator, l e1 sumSubRator ++ " + " ++ r e2 sumSubRator)
    R.ExpressionSubtraction e1 e2 -> (sumSubRator, l e1 sumSubRator ++ " - " ++ r e2 sumSubRator)
    R.ExpressionProduct c e -> (prodDivRator, unparse (R.ExpressionConstant c) ++ " * " ++ r e prodDivRator)
    R.ExpressionConditional p e1 e2 -> (maxRator,
      "if "    ++ unparse p  ++
      " then " ++ unparse e1 ++
      " else " ++ unparse e2)
    R.ExpressionParens e -> (maxRator, "(" ++ unparse e ++ ")")
    where
      l e = bracket (fragment e) LeftAssoc
      r e = bracket (fragment e) RightAssoc

instance Unparse R.ExpressionConstant where
  fragment = \case
    R.ConstantInt c -> (maxRator, show c)

instance Unparse R.PredicateAppExp where
  fragment = \case
    R.Int i -> (maxRator, show i)
    R.Var x -> (maxRator, unparse x)
    R.DCons i -> (maxRator, show i)
    R.App t ts -> (appRator, l ++ " " ++ r)
      where
        l = bracket (fragment (if length ts == 1 then t else R.App t (init ts))) LeftAssoc appRator
        r = bracket (fragment (last ts)) RightAssoc appRator
    R.If e1 e2 e3  -> (maxRator, "if "++show e1++" then "++show e2++" else "++show e3)