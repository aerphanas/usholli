module Date exposing (..)

import Date
import Html

-- Get the current date and time
currentDate : Date
currentDate = Date.now

-- Format the date and time as a string
dateString : String
dateString = Date.toString currentDate "%Y-%m-%d %H:%M:%S"

-- Create a view that displays the date and time
view : Html msg
view =
    Html.text dateString
