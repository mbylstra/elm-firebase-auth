
module Demo exposing (Model, Msg, init, update, view)



import Task

import Html exposing
  -- delete what you don't need
  ( Html, div, span, img, p, a, h1, h2, h3, h4, h5, h6, h6, text
  , ol, ul, li, dl, dt, dd
  , form, input, textarea, button, select, option
  , table, caption, tbody, thead, tr, td, th
  , em, strong, blockquote, hr, node
  )
import Html.Attributes exposing
  ( style, class, id, title, hidden, type', checked, placeholder, selected
  , name, href, target, src, height, width, alt, attribute
  )
import Html.Events exposing
  ( on, targetValue, targetChecked, keyCode, onBlur, onFocus, onSubmit
  , onClick, onDoubleClick
  , onMouseDown, onMouseUp, onMouseEnter, onMouseLeave, onMouseOver, onMouseOut
  )

import FirebaseAuth exposing (User, AuthProvider(..), SignInWithPopupError(..))

firebaseConfig =
  { apiKey = "AIzaSyDQFpnGKlC_77oj4tdgArakdcxYZ42M1H0"
  , authDomain = "elm-firebase-simple.firebaseapp.com"
  , databaseURL = "https://elm-firebase-simple.firebaseio.com"
  }

facebookLoginTask = FirebaseAuth.facebookSignInWithPopup firebaseConfig
githubLoginTask = FirebaseAuth.githubSignInWithPopup firebaseConfig

performSignUpCommand =

cmd = Task.perform Error Success githubLoginTask

-- MODEL

type alias Model =
  { user : Maybe User
  , lastError : Maybe String
  }

init : (Model, Cmd Msg)
init =
  { user = Nothing
  , lastError = Nothing
  }
  !
  []


-- UPDATE

type Msg
  = SignInClicked
  | Success User
  | Error SignInWithPopupError

update : Msg -> Model -> (Model, Cmd Msg)
update action model =
  case Debug.log "action" action of
    SignInClicked ->
      model ! [ cmd ]

    Success user ->
      { model | user = Just user } ! []

    Error error ->
      model ! []


-- VIEW

script src =
  node "script" [ attribute "src" src ] []

-- dlItem key value =
--   dl []

dlItems : List (String, String) -> List (Html msg)
dlItems items =
  List.concatMap
    (\(key, value) ->
      [ dt [] [ text key]
      , dd [] [ text value]
      ]
    )
    items


userView maybeUser =
  case maybeUser of
    Just user ->
      dl []
        ( dlItems
            [ ("uid", user.uid)
            , ("token", user.token)
            , ("display name", toString user.displayName)
            , ("photo URL", toString user.photoURL)
            ]
        )
    Nothing ->
      p [] [ text "Not signed in"]


view : Model -> Html Msg
view model =
  div []
    [ script "https://www.gstatic.com/firebasejs/3.1.0/firebase.js"
    , script "https://www.gstatic.com/firebasejs/3.1.0/firebase-app.js"
    , script "https://www.gstatic.com/firebasejs/3.1.0/firebase-auth.js"
    , script "https://www.gstatic.com/firebasejs/3.1.0/firebase-database.js"
    , button [ onClick SignInClicked ] [ text "M1" ]
    , div []
      [ userView model.user ]
    ]
