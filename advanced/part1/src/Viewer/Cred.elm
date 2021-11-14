module Viewer.Cred exposing (Cred, addHeader, addHeaderIfAvailable, credUsername, decoder, encodeToken)

import HttpBuilder exposing (RequestBuilder, withHeader)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode exposing (Value)
import Username exposing (Username)



-- TYPES


type Cred
    = {- 👉 TODO: Make Cred an opaque type, then fix the resulting compiler errors.
         Afterwards, it should no longer be possible for any other module to access
         this `token` value directly!

         💡 HINT: Other modules still depend on being able to access the
         `username` value. Expand this module's API to expose a new way for them
         to access the `username` without also giving them access to `token`.
      -}
      Cred { username : Username, token : String }



-- SERIALIZATION


decoder : Decoder Cred
decoder =
    Decode.map2 (\username token -> Cred { username = username, token = token })
        (Decode.field "username" Username.decoder)
        (Decode.field "token" Decode.string)



-- TRANSFORM


encodeToken : Cred -> Value
encodeToken (Cred cred) =
    Encode.string cred.token


credUsername : Cred -> Username
credUsername (Cred cred) =
    cred.username


addHeader : Cred -> RequestBuilder a -> RequestBuilder a
addHeader (Cred cred) builder =
    builder
        |> withHeader "authorization" ("Token " ++ cred.token)


addHeaderIfAvailable : Maybe Cred -> RequestBuilder a -> RequestBuilder a
addHeaderIfAvailable maybeCred builder =
    case maybeCred of
        Just cred ->
            addHeader cred builder

        Nothing ->
            builder
