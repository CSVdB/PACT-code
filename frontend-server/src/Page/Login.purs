module PACT.Page.Login where

import Prelude
import PACT.Data.Router (Route(..))
import PACT.Data.User (LoginFields, Username)
import PACT.Capability.Log (class Log)
import PACT.Capability.User (class ManageUser, loginUser)
import PACT.Capability.Navigate (class Navigate, navigate)
import PACT.Component.HTML.Header (header)
import PACT.Component.HTML.Utils (css, safeHref, whenElem)
import PACT.Form.Validation as V
import PACT.Form.Field as Field
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Effect.Class (class MonadEffect)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE
import Formless as F
import Type.Proxy (Proxy(..))
import Web.Event.Event as Event

data Action
  = HandleLoginForm LoginFields

-- After being logged in, should you be redirected to home?
type State
  = { redirect :: Boolean }

type Input
  = { redirect :: Boolean }

-- The component for the full login page. Focus on rendering and navigation. The
-- functionality happens in the child component for the login form.
component ::
  forall q o m.
  MonadAff m =>
  ManageUser m =>
  Navigate m =>
  Log m => H.Component q Input o m
component =
  H.mkComponent
    { initialState: identity
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
  where
  container html =
    HH.div
      [ css "auth-page" ]
      [ header Nothing Login,
        HH.div
          [ css "container page" ]
          [ HH.div
              [ css "row" ]
              [ HH.div
                  [ css "col-md-6 offset-md-3 col-xs12" ]
                  html
              ]
          ]
      ]

  -- Ignores the state, because there is none.
  render _ =
    container
      [ HH.h1
          [ css "text-xs-center" ]
          [ HH.text "Sign In" ]
      , HH.p
          [ css "text-xs-center" ]
          [ HH.a
              [ safeHref Register ]
              [ HH.text "Need an account?" ]
          ]
      -- Link child component
      , HH.slot F._formless unit formComponent unit HandleLoginForm
      ]

  handleAction = case _ of
    HandleLoginForm form -> do
      loginUser form
        >>= case _ of
            Nothing -> void $ H.query F._formless unit $ F.injQuery $ SetLoginError unit
            Just _ -> do
              st <- H.get
              when st.redirect $ navigate Home

data FormQuery a
  = SetLoginError a

derive instance Functor FormQuery

newtype LoginForm (r :: Row Type -> Type) f
  = LoginForm
  ( r
      ( username :: f V.FormError String Username
      , password :: f V.FormError String String
      )
  )

derive instance Newtype (LoginForm r f) _

input :: forall m. Monad m => F.Input LoginForm ( loginError :: Boolean ) m
input =
  { initialInputs: Nothing -- Initial values for the form
  , loginError: false
  , validators:
      LoginForm
        -- Parse the input from the forms to the right type
        { username: V.required >>> V.usernameFormat
        , password: V.required >>> V.minLength 8
        }
  }

data FormAction
  = Submit Event.Event

spec ::
  forall i m.
  Monad m =>
  MonadEffect m =>
  MonadAff m =>
  F.Spec LoginForm ( loginError :: Boolean ) FormQuery FormAction () i LoginFields m
spec =
  F.defaultSpec
    { render = render
    , handleEvent = handleEvent
    , handleQuery = handleQuery
    , handleAction = handleAction
    }
  where
  handleEvent = F.raiseResult

  handleQuery :: forall a. FormQuery a -> H.HalogenM _ _ _ _ _ (Maybe a)
  handleQuery = case _ of
    SetLoginError a -> do
      H.modify_ $ _ { loginError = true }
      pure $ Just a

  handleAction = case _ of
    Submit event -> do
      H.liftEffect $ Event.preventDefault event
      F.handleAction handleAction handleEvent F.submit

  proxies = F.mkSProxies (Proxy :: _ LoginForm)

  render { form, loginError } =
    HH.form
      -- Use `onSubmit` to catch all different ways to submit the form,
      -- including clicking the button and pressing enter.
      [ HE.onSubmit $ F.injAction <<< Submit ]
      [ whenElem loginError $
          HH.div
          [ css "error-messages" ]
          [ HH.text "Email or password is invalid" ]
      , HH.fieldset_
          [ username
          , password
          ]
      , HH.input
          [ css "btn btn-lg btn-primary pull-xs-right"
          , HP.type_ HP.InputSubmit
          , HP.value "Sign in"
          ]
      ]
    where
    username =
      Field.input proxies.username form
        [ HP.placeholder "Username", HP.type_ HP.InputText ]

    password =
      Field.input proxies.password form
        [ HP.placeholder "Password", HP.type_ HP.InputPassword ]

formComponent ::
  forall input m.
  MonadAff m =>
  MonadEffect m =>
  F.Component LoginForm FormQuery () input LoginFields m
formComponent = F.component (const input) spec
