-- This is a library with a weak test suite, so we have to define the the whole
-- thing as a root until we address that.
{ roots = [ "^Freckle\\.App\\..*", "^Main\\.main\$", "^Paths_.*" ]
, type-class-roots = True
}
