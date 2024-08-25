module Shape where

data Point = Point { x::Double, y:: Double} deriving (Eq, Show)

data Circle    = Circle    Point Double deriving (Eq, Show)
data Rectangle = Rectangle Point Point deriving (Eq, Show)


-- A point from a tuple Pair
point::(Double, Double) -> Point
point (a, b) = Point { x=a, y=b }

-- The origin
origin::Point
origin = Point { x=0, y=0 }

-- Rectangle from a Tuple where (x0 y0) == origin
rectangle::(Double, Double) -> Rectangle
rectangle (x1, y1) = Rectangle origin (point (x1, y1))

base::Rectangle -> Double
base (Rectangle p1 p2) = x p2 - x p1

height::Rectangle -> Double
height (Rectangle p1 p2) = y p2 - y p1

-- Circle from radius
circle::Double -> Circle
circle r = Circle origin r

-- Clase Shift

class Shift a where
   shift::a -> (Double, Double) -> a
   
instance Shift Point where
   shift p (dx, dy) = point (x', y') where
     x' = x p + dx
     y' = y p + dy
   
instance Shift Rectangle where
   shift (Rectangle p1 p2) (dx, dy) = Rectangle p1' p2' where
     p1' = shift p1 (dx, dy)
     p2' = shift p2 (dx, dy)
   
instance Shift Circle where
   shift (Circle p r) (dx, dy) = Circle p' r where
     p' = shift p (dx, dy)
   
-- Define the Surface class
   
class Surface a where
  surface :: a -> Double

instance Surface Circle where
  surface (Circle _ r) = pi * r^2

instance Surface Rectangle where
  surface rtgl = b * h where
    b = base rtgl
    h = height rtgl
