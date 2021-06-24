module PACT.Page.Register where

import Prelude
import PACT.Data.Router (Route(..))
import PACT.Data.Email (EmailAddress)
import PACT.Data.User
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
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE
import Formless as F
import Type.Proxy (Proxy(..))

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
      _ <- registerUser form
      navigate Home

newtype RegisterForm (r :: Row Type -> Type) f = RegisterForm (r
  ( username :: f V.FormError String Username
  , email :: f V.FormError String EmailAddress
  , password :: f V.FormError String String
  ))

derive instance Newtype (RegisterForm r f) _

spec :: forall input m. Monad m => F.Spec' RegisterForm RegisterFields input m
spec = F.defaultSpec { render = render, handleEvent = F.raiseResult }
  where
    proxies = F.mkSProxies (Proxy :: _ RegisterForm)

    render { form } =
      HH.form
        [ HE.onClick $ const F.submit ]
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
      { username: V.required >>> V.usernameFormat
      , email: V.required >>> V.minLength 3 >>> V.emailFormat
      , password: V.required >>> V.minLength 8
      }
  }

formComponent
  :: forall input m. MonadAff m
  => F.Component RegisterForm (Const Void) () input RegisterFields m
formComponent = F.component (const input) spec
