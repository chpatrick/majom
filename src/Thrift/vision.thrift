struct Position {
  1: double x,
  2: double y,
  3: double z
}

service Vision { 
  Position observe()
}
