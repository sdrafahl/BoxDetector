package Models

enum Space {
  case Occupied extends Space
  case Empty extends Space
  case InvalidSpace extends Space
}
