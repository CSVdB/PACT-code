module PACT.Page.Register where

import Prelude (class Monad, Unit, Void, const, unit, ($), (<<<), discard, (>>=), pure)
import PACT.Data.Router (Route(..))
import PACT.Data.Email (EmailAddress)
import PACT.Data.User (RegisterFields, Username)
import PACT.Capability.Log (class Log)
import PACT.Capability.User (class ManageUser, registerUser)
import PACT.Capability.Navigate (class Navigate, navigate)
import PACT.Component.HTML.Header (header)
import PACT.Component.HTML.Utils (css, safeHref)
import PACT.Form.Validation as V
import PACT.Form.Field as Field
import Data.Maybe (Maybe(..))
import Data.Const (Const)
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
  = HandleRegisterForm RegisterFields

-- The component for the full registration page. Focus on rendering and
-- navigation. The functionality happens in the child component for the
-- registration form.
component :: forall q o m. MonadAff m => ManageUser m => Navigate m
  => Log m => H.Component q Unit o m
component =
  H.mkComponent
    { initialState: const unit
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
  where
  container html =
    HH.div_
      [ header Nothing Register,
        HH.div
          [ css "auth-page" ]
          [ HH.div
              [ css "container page" ]
              [ HH.div
                  [ css "row" ]
                  [ HH.div
                      [ css "col-md-6 offset-md-3 col-xs12" ]
                      html
                  ]
              ]
          ]
      ]

  -- Ignores the state, because there is none.
  render _ =
    container
      [ HH.h1
          [ css "text-xs-center" ]
          [ HH.text "Sign Up" ]
      , HH.p
          [ css "text-xs-center" ]
          [ HH.a
              [ safeHref Login ]
              [ HH.text "Already have an account?" ]
          ]
      -- Link child component
      , HH.slot F._formless unit formComponent unit HandleRegisterForm
      ]

  handleAction = case _ of
    HandleRegisterForm form -> do
      registerUser form >>= case _ of
        Nothing -> pure unit -- TODO: This should send some error around saying
        -- something went wrong on the backend, and whom to contact for help.
        Just _ -> navigate Home

newtype RegisterForm (r :: Row Type -> Type) f = RegisterForm (r
  ( username :: f V.FormError String Username
  , email :: f V.FormError String EmailAddress
  , password :: f V.FormError String String
  ))

derive instance Newtype (RegisterForm r f) _

data FormAction
  = Submit Event.Event

spec :: forall i m. Monad m => MonadEffect m => MonadAff m
  => F.Spec RegisterForm () (Const Void) FormAction () i RegisterFields m
spec = F.defaultSpec
  { render = render,
    handleEvent = handleEvent,
    handleAction = handleAction
  }
  where
    proxies = F.mkSProxies (Proxy :: _ RegisterForm)

    handleEvent = F.raiseResult

    handleAction = case _ of
      Submit event -> do
         H.liftEffect $ Event.preventDefault event
         F.handleAction handleAction handleEvent F.submit

    render { form } =
      HH.form
      [ HE.onSubmit $ F.injAction <<< Submit ]
        [ HH.fieldset_
            [ username
            , email
            , password
            ]
        , HH.input
          [ css "btn btn-lg btn-primary pull-xs-right"
          , HP.type_ HP.InputSubmit
          , HP.value "Sign up"
          ]
        ]
      where
        username = 
          Field.input proxies.username form
            [ HP.placeholder "Username", HP.type_ HP.InputText ]

        email =
          Field.input proxies.email form
            [ HP.placeholder "Email", HP.type_ HP.InputEmail ]

        password =
          Field.input proxies.password form
            [ HP.placeholder "Password" , HP.type_ HP.InputPassword ]


input :: forall m. Monad m => F.Input' RegisterForm m
input =
  { initialInputs: Nothing -- Initial values for the form
  , validators: RegisterForm
      -- Parse the input from the forms to the right type
      { username: V.usernameValidator
      , email: V.emailValidator
      , password: V.passwordValidator
      }
  }

formComponent
  :: forall input m. MonadAff m
  => F.Component RegisterForm (Const Void) () input RegisterFields m
formComponent = F.component (const input) spec
