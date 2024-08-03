module Feature exposing (enabled)

import Features exposing (Feature)
import Session exposing (LoggedIn, Session(..))


enabled : Feature -> LoggedIn -> Bool
enabled feature session =
    List.member feature (Session.features session)
