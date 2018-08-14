module Event where

import           State

import           SDL

parseEvents :: [Event] -> [Action]
parseEvents = map parseEvent

parseEvent :: Event -> Action
parseEvent event =
  case eventPayload event of
    QuitEvent -> QuitProgram
    KeyboardEvent d ->
      case keysymKeycode . keyboardEventKeysym $ d of
        KeycodeEscape -> QuitProgram
        KeycodeD      -> Move 1
        KeycodeA      -> Move (-1)
        KeycodeLeft   -> Move 1
        KeycodeRight  -> Move (-1)
        KeycodeSpace  -> StartGame
        _             -> NoOp
    _ -> NoOp
