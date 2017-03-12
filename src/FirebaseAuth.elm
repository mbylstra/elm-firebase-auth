module FirebaseAuth
    exposing
        ( authProviderSignInWithPopup
        , githubSignInWithPopup
        , googleSignInWithPopup
        , twitterSignInWithPopup
        , facebookSignInWithPopup
        , getCurrentUser
        , updateUserProfile
        , signOut
        , User
        , AuthProvider(..)
        , SignInWithPopupError(..)
        , Config
        )

import Task exposing (Task, andThen, succeed, fail)
import Native.FirebaseAuth
import Json.Encode
import Json.Decode
import Http


type AuthProvider
    = Facebook
    | Github
    | Google
    | Twitter


type alias Config =
    { apiKey : String
    , authDomain : String
    , databaseURL : String
    }


type alias User =
    { uid : String
    , token : String
    , displayName : String
    , photoURL : Maybe String
    , githubUid : String
    , githubUsername : String
    }


type alias UserProfile =
    { displayName : String
    , photoURL : Maybe String
    }


type SignInWithPopupError
    = AccountExistsWithDifferentCredential String
    | AuthDomainConfigRequired String
    | CancelledPopupRequest String
    | OperationNotAllowed String
    | OperationNotSupportedInThisEnvironment String
    | PopupBlocked
    | PopupClosedByUser
    | UnauthorizedDomain
    | FirebaseSdkNotAvailable
    | GithubApiError


authProviderSignInWithPopup : Config -> AuthProvider -> Task SignInWithPopupError User
authProviderSignInWithPopup =
    Native.FirebaseAuth.authProviderSignIn


-- githubSignInWithPopup : Config -> Task SignInWithPopupError User
-- githubSignInWithPopup config =
--     Native.FirebaseAuth.authProviderSignIn config Github


-- the thing with tasks is that the error type alwasy needs to be the same
-- scoping is wierd

githubSignInWithPopup : Config -> Task SignInWithPopupError User
githubSignInWithPopup config =
    Native.FirebaseAuth.authProviderSignIn config Github
      |> Task.andThen
        (\user ->
            getGithubUsername user.githubUid
            |> Task.andThen
                ( \githubUsername ->
                  Task.succeed { user | githubUsername = githubUsername }
                )
        )

      -- succeed 4

facebookSignInWithPopup : Config -> Task SignInWithPopupError User
facebookSignInWithPopup config =
    Native.FirebaseAuth.authProviderSignIn config Facebook


googleSignInWithPopup : Config -> Task SignInWithPopupError User
googleSignInWithPopup config =
    Native.FirebaseAuth.authProviderSignIn config Google


twitterSignInWithPopup : Config -> Task SignInWithPopupError User
twitterSignInWithPopup config =
    Native.FirebaseAuth.authProviderSignIn config Twitter


getCurrentUser : Config -> Task SignInWithPopupError (Maybe User)
getCurrentUser config =
    Native.FirebaseAuth.getCurrentUser config
      |> Task.andThen
            (\maybeUser ->
                case maybeUser of
                    Just user ->
                        getGithubUsername user.githubUid
                        |> Task.andThen
                            ( \githubUsername ->
                              Task.succeed <| Just { user | githubUsername = githubUsername }
                            )
                    Nothing ->
                        Task.succeed Nothing
            )



updateUserProfile : Config -> UserProfile -> Task () ()
updateUserProfile config userProfile =
    let
        photoUrlEncoded =
            case userProfile.photoURL of
                Just photoURL ->
                    Json.Encode.string photoURL

                Nothing ->
                    Json.Encode.null

        userProfileEncoded =
            Json.Encode.encode 0 <|
                Json.Encode.object
                    [ ( "displayName", Json.Encode.string userProfile.displayName )
                    , ( "photoURL", photoUrlEncoded )
                    ]
    in
        Native.FirebaseAuth.updateUserProfile config userProfileEncoded


signOut : Config -> Task SignInWithPopupError Bool
signOut =
    Native.FirebaseAuth.signOut


getGithubUsername : String -> Task SignInWithPopupError String
getGithubUsername githubUid =
    let
        url = "https://api.github.com/user/" ++ githubUid
    in
        Http.get url (Json.Decode.field "login" Json.Decode.string)
        |> Http.toTask
        |> Task.mapError (\_ -> GithubApiError)
