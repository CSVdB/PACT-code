module PACT.Form.Field where

import Prelude
import PACT.Component.HTML.Utils (css, maybeElem)
import PACT.Form.Validation as V
import Data.Variant (Variant)
import Data.Newtype (class Newtype)
import Data.Symbol (class IsSymbol)
import Type.Proxy (Proxy)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Formless as F
import DOM.HTML.Indexed (HTMLinput)
import Type.Row as Row

-- | Explanation of the type declaration:
-- | - `IsSymbol` means `sym` must be a type-level string, e.g. a record label
-- | - The `Newtype` constaints mean that both forms must basically be of the
-- |   same form as the right hand side of these constraintsi, so we can unpack
-- |   our form type into a raw record (or variant thereof).
-- | - `Cons`: `sym` is an input field in the given types
input ::
  forall form act slots m sym fields inputs out t0 t1.
  IsSymbol sym =>
  Newtype (form Record F.FormField) { | fields } =>
  Newtype (form Variant F.InputFunction) (Variant inputs) =>
  Row.Cons sym (F.FormField V.FormError String out) t0 fields =>
  Row.Cons sym (F.InputFunction V.FormError String out) t1 inputs =>
  Proxy sym ->
  form Record F.FormField ->
  Array (HH.IProp HTMLinput (F.Action form act)) ->
  F.ComponentHTML form act slots m
input sym form props =
  HH.fieldset
    [ css "form-group" ]
    [ HH.input
        ( append
            [ css "form-control form-control-lg"
            , HP.value $ F.getInput sym form
            , HE.onValueInput $ F.setValidate sym
            ]
            props
        )
    , maybeElem (F.getError sym form) \err ->
        HH.div
          [ css "error-messages" ]
          [ HH.text $ V.errorToString err ]
    ]
