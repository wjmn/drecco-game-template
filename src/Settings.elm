module Settings exposing (..)

{-| This module handles everything on the Settings screen.

TODO: You will need to modify this file to add / remove settings for your game.

Adding/removing a setting is a 5-step process.
(I know it seems like a lot, but it is necessary so Elm can make static
guarantees at compile time about your Settings).

I've outlined the five steps below under SETTING DEFINITIONS.

-}

import Common exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import SettingsComponents exposing (..)



--------------------------------------------------------------------------------
-- SETTING DEFINITIONS
--
-- You can add / delete settings by modifying the following 5 steps:
-- 1. Define the data model for your settings and their types.
-- 2. Define the default values for your settings.
-- 3. Add a message type to update your settings.
-- 4. Define explicitly what happens to your settings when a message is received.
-- 5. Define a list of pickers for each setting you want to be able to change.
--
-- This should cover most of the basic use cases. If you need extra
-- customisation, you're welcome to edit the code below or delete everything
-- here and start from scratch.
--------------------------------------------------------------------------------


{-| STEP 1: Define the data model for your settings and their types.

Keep it simple: you probably don't as many settings as there are here
(you might only need 1 or 2). You'll have access to the data of this type
in your Game when the user clicks StartGame.

-}
type alias Settings =
    { initialCount : Int
    }


{-| STEP 2: Define the default values for your settings.

For simplicity's sake, every setting MUST have a default value.

-}
default : Settings
default =
    { initialCount = 0
    }


{-| STEP 3: Add a message type to update your settings.

Your message type should have a payload attached (the new value for the
setting). This is typically the same type as your setting.

-}
type Msg
    = SetInitialCount Int


{-| STEP 4: Define explicitly what happens to your settings when a message is received.

Handle each Msg case below. Most likely, you'll just update the settings record
with the new payload. You can see the implementations below for this.

-}
update : Msg -> Settings -> Settings
update msg settings =
    case msg of
        SetInitialCount count ->
            { settings | initialCount = count }


{-| STEP 5: Define a list of pickers for each setting you want to be able to change.

I've defined a bunch of helper functions for you to make things easier.

Helper functions include:

  - inputString (a small text input for the user to input a string)
  - inputFloat (a number input for floats)
  - inputInt (a number input for ints)
  - inputFloatRange (a range slider for floats)
  - inputIntRange (a range slider for ints)
  - pickChoiceButtons (a set of buttons for the user to pick from - good for small enums)
  - pickChoiceDropdown (a dropdown of options for the user to pick from)

Each function has it's own type defining what data it needs; see the HELPER
FUNCTIONS section.

You can customise this further if you so wish (see the HELPER FUNCTIONS section below).

-}
pickers : Settings -> List (SettingPickerItem Msg)
pickers settings =
    [ inputIntRange
        { label = "Initial Count"
        , value = settings.initialCount
        , min = 0
        , max = 100
        , onChange = SetInitialCount
        }
    ]


{-| The function that views all settings which gets called from the Main application.
-}
view : Settings -> Html Msg
view settings =
    div [ id "settings" ]
        [ viewPicker [] (pickers settings)
        ]
