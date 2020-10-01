package main.scala

object ShapesHomeWork {

  /* General:
   *
   * TODO: make abstraction from Dimension support ( Shape for any TPoint, like Triangle & Triangle3D)
   *
   * TODO: implement map & fold methods for NormalShape and
   *       implement common realization for move,center,max,min
   *
   * TODO: get knowledge how to make Shape an instance of Monad,
   *       i.e. allow extract (ps:TPoint) from Shape[TPoint] using for comprehension
   *            - use it for comfort measure and boundary generalization
   *                       - another option: always keep TPoints in List
   *
   */

  /* Basic Interfaces */

  // generalization of length,area,volume
  sealed trait Measurable {
    def measure: Double
  }

  sealed trait Located[TPoint] {
    def center: TPoint
  }

  sealed trait Bounded[TPoint] {
    def min: TPoint
    def max: TPoint
  }

  // it takes borders. e.g. for Triangle ABC we get lines: [AB,BC,CA]
  sealed trait Differential[TBound <: Differential[TBound]] {
    def boundary: Array[TBound]
  }

  sealed trait Movable[TSelf <: Movable[TSelf,TDirection], TDirection] {
    def move(dr: TDirection): TSelf
  }

  /* Advanced Interfaces */

  sealed trait Shape[TPoint] extends Located[TPoint] with Bounded[TPoint] {
  }

  sealed trait Tangible[TBound <: Tangible[TBound]] extends Measurable with Differential[TBound] {
    def perimeter: Double = this.boundary.map(_.measure).sum
    def volume: Double = this.measure
  }

  // only Origin is an instance of this trait. Perhaps, should be removed.
  sealed trait FictiveShape[TBound,TPoint] extends Shape[TPoint] with Tangible[TBound]



  // TODO: add standard implementations: center, max, min, move
  //       add map, fold; try to make instance of trait that allows to use for comprehension.
  sealed trait NormalShape[TSelf,
                           TBound,
                           TPoint <: NormalShape[TPoint,Nothing,TPoint] ] extends /*<: PrimitiveShape[TPoint]*/
    FictiveShape[TBound,TPoint] with Movable[TSelf,TPoint] {
    def centralize: TSelf = this move this.center

    //def map: (TPoint => TPoint) => TSelf
    //override def move(dr: TPoint): TSelf = this.map(_.move(dr))

    /*
    override def center: TPoint = this.fold(_ move _) / this.len
    def fold: ((TPoint,TPoint) => TPoint) => TPoint
    def len: Int
    */

  }

  /*
  sealed trait PrimitiveShape[TSelf] extends NormalShape[TSelf,Nothing,TSelf] {
  }
  */


  /* Points */

  final case class Point2D(x: Double, y: Double)
    extends NormalShape[Point2D,Nothing,Point2D]
  {
    override def center: Point2D = this
    override def min: Point2D = this
    override def max: Point2D = this
    override def measure: Double = 0
    override def boundary: Array[Nothing] = Array()
    override def move(dr: Point2D): Point2D = Point2D(x+dr.x,y+dr.y)

    //override def map: (Point2D => Point2D) => Point2D = f => f(this)
  }

  final case class Point3D(x: Double, y: Double, z: Double)
    extends NormalShape[Point3D,Nothing,Point3D]
  {
    override def center: Point3D = this
    override def min: Point3D = this
    override def max: Point3D = this
    override def measure: Double = 0
    override def boundary: Array[Nothing] = Array()
    override def move(dr: Point3D): Point3D = Point3D(x+dr.x,y+dr.y,z+dr.z)

    //override def map: (Point3D => Point3D) => Point3D = f => f(this)

  }

  /* Lines */

  final case class Line2D(a: Point2D, b: Point2D)
    extends NormalShape[Line2D,Point2D,Point2D]
  {
    override def center: Point2D =
      Point2D( (a.x+b.x)/2 , (a.y+b.y)/2 )

    override def min: Point2D = {
      val minX = Math.min( a.x, b.x )
      val minY = Math.min( a.y, b.y )
      Point2D(minX,minY)
    }

    override def max: Point2D = {
      val maxX = Math.max( a.x, b.x )
      val maxY = Math.max( a.y, b.y )
      Point2D(maxX,maxY)
    }

    override def measure: Double = {
      val dx2 = Math.pow( a.x - b.x , 2 )
      val dy2 = Math.pow( a.y - b.y , 2 )
      Math.sqrt( dx2 + dy2 )
    }

    override def boundary: Array[Point2D] = Array(a,b)

    override def move(dr: Point2D): Line2D =
      Line2D(a.move(dr),b.move(dr))
  }


  final case class Line3D(a: Point3D, b: Point3D)
    extends NormalShape[Line3D,Point3D,Point3D]
  {
    override def center: Point3D =
      Point3D( (a.x+b.x)/2 , (a.y+b.y)/2, (a.z+b.z)/2 )

    override def min: Point3D = {
      val minX = Math.min( a.x, b.x )
      val minY = Math.min( a.y, b.y )
      val minZ = Math.min( a.z, b.z )
      Point3D(minX,minY,minZ)
    }

    override def max: Point3D = {
      val maxX = Math.max( a.x, b.x )
      val maxY = Math.max( a.y, b.y )
      val maxZ = Math.max( a.z, b.z )
      Point3D(maxX,maxY,maxZ)
    }

    override def measure: Double = {
      val dx2 = Math.pow( a.x - b.x , 2 )
      val dy2 = Math.pow( a.y - b.y , 2 )
      val dz2 = Math.pow( a.z - b.z , 2 )
      Math.sqrt( dx2 + dy2 + dz2)
    }

    override def boundary: Array[Point3D] = Array(a,b)

    override def move(dr: Point3D): Line3D =
      Line3D(a.move(dr),b.move(dr))
  }


  /*  Origins  */

  final case class Origin2D(o: Point2D)
    extends FictiveShape[Nothing,Point2D]
  {
    override def measure: Double = 0
    override def center: Point2D = o
    override def min: Point2D = o
    override def max: Point2D = o
    override def boundary: Array[Nothing] = Array()
  }

  final case class Origin3D(o: Point3D)
    extends FictiveShape[Nothing,Point3D]
  {
    override def measure: Double = 0
    override def center: Point3D = o
    override def min: Point3D = o
    override def max: Point3D = o
    override def boundary: Array[Nothing] = Array()
  }

  /*  Circles & Spheres  */

  final case class Circumference(o: Point2D, r: Double)
    extends NormalShape[Circumference,Nothing,Point2D]
  {
    override def center: Point2D = o
    override def move(dr: Point2D): Circumference =
      Circumference(o.move(dr),r)

    override def boundary: Array[Nothing] = Array()

    override def min: Point2D = Point2D(o.x-r,o.y-r)
    override def max: Point2D = Point2D(o.x+r,o.y+r)

    override def measure: Double = 2*Math.PI*r
  }

  final case class Circle(o: Point2D, r: Double)
    extends NormalShape[Circle,Circumference,Point2D]
  {
    override def center: Point2D = o

    override def move(dr: Point2D): Circle = Circle(o.move(dr),r)

    override def boundary: Array[Circumference] = Array(Circumference(o,r))

    override def min: Point2D = Point2D(o.x-r,o.y-r)
    override def max: Point2D = Point2D(o.x+r,o.y+r)

    override def measure: Double = Math.PI*r*r
  }

  // Boundary of a Sphere
  final case class DSphere(o: Point3D, r: Double)
    extends NormalShape[DSphere,Nothing,Point3D]
  {
    override def center: Point3D = o
    override def move(dr: Point3D): DSphere = DSphere(o.move(dr),r)

    override def boundary: Array[Nothing] = Array()

    override def min: Point3D = Point3D(o.x-r,o.y-r,o.z-r)
    override def max: Point3D = Point3D(o.x+r,o.y+r,o.z-r)

    override def measure: Double = 4*Math.PI*r*r
  }

  final case class Sphere(o: Point3D, r: Double)
    extends NormalShape[Sphere,DSphere,Point3D]
  {
    override def center: Point3D = o
    override def move(dr: Point3D): Sphere = Sphere(o.move(dr),r)

    override def boundary: Array[DSphere] = Array(DSphere(o,r))

    override def min: Point3D = Point3D(o.x-r,o.y-r,o.z-r)
    override def max: Point3D = Point3D(o.x+r,o.y+r,o.z-r)

    override def measure: Double = 4*Math.PI*r*r*r/3
  }





  /*  Polygons and 2D Shapes  */



  //any closed piecewise linear curve
  //assumption: ps are ranged in the connection order.
  final case class Polygon(ps: Array[Point2D])
    extends NormalShape[Polygon,Line2D,Point2D]
  {
    private val xs = ps.map(_.x)
    private val ys = ps.map(_.y)
    private val len = ps.length

    override def center: Point2D = Point2D(xs.sum/len, ys.sum/len)
    override def min: Point2D = Point2D(xs.min, ys.min)
    override def max: Point2D = Point2D(xs.max, ys.max)

    override def move(dr: Point2D): Polygon = Polygon(ps.map(_.move(dr)))

    override def boundary: Array[Line2D] = {
      val pairs = ps.zip(ps.tail)
      val lastFirst = Array( ps.last , ps.head )
      val lines: Array[(Point2D,Point2D)] =
        Array.concat( pairs, lastFirst )

      lines.map( p => Line2D(p._1,p._2) )

    }

    override def measure: Double = ???  //TODO <- implement

  }

  /*  2D & 3D Rectangles & Squares  */

  final case class Rectangle2D(p1: Point2D, p2:Point2D)
    extends NormalShape[Rectangle2D,Line2D,Point2D]
  {
    def width: Double  = max.x - min.x
    def height: Double = max.y - min.y

    //same for Lines2D
    override def move(dr: Point2D): Rectangle2D = Rectangle2D(p1.move(dr),p2.move(dr))
    override def center: Point2D = Line2D(p1,p2).center
    override def min: Point2D = Line2D(p1,p2).min
    override def max: Point2D = Line2D(p1,p2).max

    //new ones:
    override def measure: Double = width * height

    override def boundary: Array[Line2D] = {

      val a = min
      val b = Point2D(max.x,min.y)
      val c = max
      val d = Point2D(min.x,max.y)

      Array( Line2D(a,b),
             Line2D(b,c),
             Line2D(c,d),
             Line2D(d,a))

    }

  }

  //type Cuboid = Rectangle3D
  //TODO: implement border realization. Now it is without Borders.
  final case class Cuboid(p1: Point3D, p2:Point3D) extends NormalShape[Cuboid,Nothing,Point3D] {

    val size: Point3D = Point3D(max.x - min.x , max.y - min.y, max.z - min.z)

    //same for Lines3D
    override def move(dr: Point3D): Cuboid = Cuboid(p1.move(dr),p2.move(dr))
    override def center: Point3D = Line3D(p1,p2).center
    override def min: Point3D = Line3D(p1,p2).min
    override def max: Point3D = Line3D(p1,p2).max

    //new ones:
    override def measure: Double = size.x * size.y * size.z

    //TODO
    override def boundary: Array[Nothing] = ???
  }


  //TODO implement Square3D. then: boundary
  final case class Cube(center: Point3D, a: Double) extends NormalShape[Cube,Nothing,Point3D] {

    override def move(dr: Point3D): Cube = Cube(center.move(dr),a)

    override def measure: Double = a*a*a
    override def perimeter: Double = 6*a

    override def min: Point3D = center.move(Point3D(-a/2,-a/2,-a/2))
    override def max: Point3D = Point3D(center.x+a/2,center.y+a/2,center.z+a/2)

    override def boundary: Array[Nothing] = ???
  }



  /* Simplexes */


  final case class Triangle(a: Point2D, b: Point2D, c: Point2D)
    extends NormalShape[Triangle,Line2D,Point2D] {
    override def move(dr: Point2D): Triangle = Triangle(a.move(dr), b.move(dr), c.move(dr))

    override def center: Point2D = {
      val p: Point2D = a.move(b).move(c)
      Point2D(p.x / 3, p.y / 3)
    }

    override def boundary: Array[Line2D] = Array(Line2D(a, b), Line2D(b, c), Line2D(c, a))

    override def measure: Double =
      Math.abs((b.x - a.x) * (c.y - a.y) - (b.y - a.y) * (c.x - a.x)) / 2

    override def min: Point2D = {
      val minX = Math.min(Math.min(a.x, b.x), c.x)
      val minY = Math.min(Math.min(a.y, b.y), c.y)
      Point2D(minX, minY)
    }

    override def max: Point2D = {
      val maxX = Math.max(Math.max(a.x, b.x), c.x)
      val maxY = Math.max(Math.max(a.y, b.y), c.y)
      Point2D(maxX, maxY)
    }

  }


  final case class Triangle3D(a: Point3D, b: Point3D, c: Point3D)
    extends NormalShape[Triangle3D,Line3D,Point3D] {

    override def move(dr: Point3D): Triangle3D = Triangle3D(a.move(dr), b.move(dr), c.move(dr))

    override def center: Point3D = {
      val p: Point3D = a.move(b).move(c)
      Point3D(p.x / 3, p.y / 3, p.z / 3)
    }

    override def boundary: Array[Line3D] = Array(Line3D(a, b), Line3D(b, c), Line3D(c, a))

    // TODO: implement using for comprehension
    override def measure: Double = ???

    override def min: Point3D = {
      val minX = Math.min(Math.min(a.x, b.x), c.x)
      val minY = Math.min(Math.min(a.y, b.y), c.y)
      val minZ = Math.min(Math.min(a.z, b.z), c.z)
      Point3D(minX, minY, minZ)
    }

    override def max: Point3D = {
      val maxX = Math.max(Math.max(a.x, b.x), c.x)
      val maxY = Math.max(Math.max(a.y, b.y), c.y)
      val maxZ = Math.max(Math.max(a.z, b.z), c.z)
      Point3D(maxX, maxY, maxZ)
    }

  }



  final case class Tetrahedron(a: Point3D, b: Point3D, c: Point3D, d: Point3D)
    extends NormalShape[Tetrahedron,Triangle3D,Point3D]
  {
    override def move(dr: Point3D): Tetrahedron = Tetrahedron(a.move(dr), b.move(dr), c.move(dr), d.move(dr))

    val ps: Array[Point3D] = Array(a,b,c,d)

    override def min: Point3D = Point3D( ps.map(_.x).min , ps.map(_.y).min , ps.map(_.z).min )
    override def max: Point3D = Point3D( ps.map(_.x).max , ps.map(_.y).max , ps.map(_.z).max )

    override def boundary: Array[Triangle3D] = for {
      p1 <- ps
      p2 <- ps if ps.indexOf(p2) < ps.indexOf(p1)
      p3 <- ps if ps.indexOf(p3) < ps.indexOf(p2)
    } yield Triangle3D(p1,p2,p3)

    override def center: Point3D = {
      val p = a move b move c move d
      Point3D(p.x/4,p.y/4,p.z/4)
    }

    override def measure: Double = ???  //<- TODO: use for comprehension
  }

}