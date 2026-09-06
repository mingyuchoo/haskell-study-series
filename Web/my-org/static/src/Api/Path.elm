module Api.Path exposing (orgPath)

import Url


orgPath : String -> String -> String
orgPath org tail =
    "/api/organizations/"
        ++ Url.percentEncode org
        ++ (if tail == "" then
                ""

            else
                "/" ++ tail
           )
