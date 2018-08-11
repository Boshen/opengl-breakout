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
        _             -> NoOp
    _ -> NoOp
