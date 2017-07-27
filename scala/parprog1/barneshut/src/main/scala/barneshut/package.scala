import common._
import barneshut.conctrees._
import scala.annotation.switch

package object barneshut {

  class Boundaries {
    var minX = Float.MaxValue

    var minY = Float.MaxValue

    var maxX = Float.MinValue

    var maxY = Float.MinValue

    def width = maxX - minX

    def height = maxY - minY

    def size = math.max(width, height)

    def centerX = minX + width / 2

    def centerY = minY + height / 2

    override def toString = s"Boundaries($minX, $minY, $maxX, $maxY)"
  }

  sealed abstract class Quad {
    def massX: Float

    def massY: Float

    def mass: Float

    def centerX: Float

    def centerY: Float

    def size: Float

    def total: Int

    def insert(b: Body): Quad
  }

  case class Empty(centerX: Float, centerY: Float, size: Float) extends Quad {
    def massX: Float = centerX
    def massY: Float = centerY
    def mass: Float = 0
    def total: Int = 0
    def insert(b: Body): Quad = Leaf(centerX, centerY, size, Seq(b))
  }

  // The center of the Fork quadtree is then specified by, say, the
  // lower right corner of the quadtree nw. If the Fork quadtree is
  // empty, the center of mass coincides with the center.
  case class Fork(
    nw: Quad, ne: Quad, sw: Quad, se: Quad
  ) extends Quad {
    val centerX: Float = nw.centerX + nw.size / 2
    val centerY: Float = nw.centerY + nw.size / 2
    val size: Float = nw.size + ne.size + sw.size + se.size
    val mass: Float = nw.mass + ne.mass + sw.mass + se.mass
    val massX: Float =
      if (mass != 0) (nw.mass * nw.massX + ne.mass * ne.massX + sw.mass * sw.massX + se.mass * se.massX) / mass
      else centerX
    val massY: Float =
      if (mass != 0) (nw.mass * nw.massY + ne.mass * ne.massY + sw.mass * sw.massY + se.mass * se.massY) / mass
      else centerY
    val total: Int = nw.total + ne.total + sw.total + se.total

    def insert(b: Body): Fork = {
      if (b.x < centerX){
        if (b.y < centerY) Fork(nw.insert(b), ne, sw, se)
        else Fork(nw, ne, sw.insert(b), se)
      } else {
        if (b.y <= centerY) Fork(nw, ne.insert(b), sw, se)
        else Fork(nw, ne, sw, se.insert(b))
      }
    }
  }

  case class Leaf(centerX: Float, centerY: Float, size: Float, bodies: Seq[Body])
      extends Quad {

    val mass = bodies.foldLeft(0f)((m, b) => m + b.mass)
    val massX = bodies.foldLeft(0f)((xm, b) => xm + (b.mass * b.x) / mass)
    val massY = bodies.foldLeft(0f)((ym, b) => ym + (b.mass * b.y) / mass)
    val total: Int = bodies.size

    def insert(b: Body): Quad = {
      if (size > minimumSize){
        val newSize = size / 2
        val empties = Fork(
          Empty(centerX, centerY + size / 2, newSize),
          Empty(centerX, centerY, newSize),
          Empty(centerX, centerY, newSize),
          Empty(centerX, centerY, newSize)
        )
        bodies.foldLeft(empties)((f, b) => f.insert(b))

      } else {
        Leaf(centerX, centerY, size, b +: bodies)
      }
    }
  }

  def minimumSize = 0.00001f

  def gee: Float = 100.0f

  def delta: Float = 0.01f

  def theta = 0.5f

  def eliminationThreshold = 0.5f

  def force(m1: Float, m2: Float, dist: Float): Float = gee * m1 * m2 / (dist * dist)

  def distance(x0: Float, y0: Float, x1: Float, y1: Float): Float = {
    math.sqrt((x1 - x0) * (x1 - x0) + (y1 - y0) * (y1 - y0)).toFloat
  }

  class Body(val mass: Float, val x: Float, val y: Float, val xspeed: Float, val yspeed: Float) {


    def updated(quad: Quad): Body = {
      var netforcex = 0.0f
      var netforcey = 0.0f

      def addForce(thatMass: Float, thatMassX: Float, thatMassY: Float): Unit = {
        val dist = distance(thatMassX, thatMassY, x, y)
        /* If the distance is smaller than 1f, we enter the realm of close
         * body interactions. Since we do not model them in this simplistic
         * implementation, bodies at extreme proximities get a huge acceleration,
         * and are catapulted from each other's gravitational pull at extreme
         * velocities (something like this:
         * http://en.wikipedia.org/wiki/Interplanetary_spaceflight#Gravitational_slingshot).
         * To decrease the effect of this gravitational slingshot, as a very
         * simple approximation, we ignore gravity at extreme proximities.
         */
        if (dist > 1f) {
          val dforce = force(mass, thatMass, dist)
          val xn = (thatMassX - x) / dist
          val yn = (thatMassY - y) / dist
          val dforcex = dforce * xn
          val dforcey = dforce * yn
          netforcex += dforcex
          netforcey += dforcey
        }
      }

      /* empty quadtree does not affect the net force
       * each body in a leaf quadtree adds some net force
       * a fork quadtree that is sufficiently far away acts as a single point of mass
       * a fork quadtree that is not sufficiently far away must be recursively traversed
       *
       * When are we allowed to approximate a cluster of bodies with a single point? The heuristic that is used is that the size of the cell divided by the distance dist between the center of mass and the particle is less than some constant theta:
       *
       * Hint: make sure you use the distance to compute distance between points, the theta value for the condition, and addForce to add force contributions! */


      def traverse(quad: Quad): Unit = (quad: Quad) match {
        case Empty(_, _, _) =>
          // no force
        case Leaf(_, _, _, bodies) =>
          // add force contribution of each body by calling addForce
          bodies foreach(b => addForce(b.mass, b.mass * b.x / b.mass, b.mass * b.y / b.mass))
        case Fork(nw, ne, sw, se) =>
          // see if node is far enough from the body,
          // or recursion is needed
          val dist = distance(x, y, quad.massX, quad.massY)
          if (quad.size / dist < theta){
            // not enough far away, recursively traverse
            traverse(nw)
            traverse(ne)
            traverse(sw)
            traverse(se)
          } else {
            // far away, treat cluster of bodies as a single point
            addForce(quad.mass, quad.massX, quad.massY)
          }
      }

      traverse(quad)

      val nx = x + xspeed * delta
      val ny = y + yspeed * delta
      val nxspeed = xspeed + netforcex / mass * delta
      val nyspeed = yspeed + netforcey / mass * delta

      new Body(mass, nx, ny, nxspeed, nyspeed)
    }
  }

  val SECTOR_PRECISION = 8

  class SectorMatrix(val boundaries: Boundaries, val sectorPrecision: Int) {
    import scala.language.implicitConversions

    implicit def toInt(x: Float) = x.toInt

    val sectorSize = boundaries.size / sectorPrecision
    val matrix = new Array[ConcBuffer[Body]](sectorPrecision * sectorPrecision)
    for (i <- 0 until matrix.length) matrix(i) = new ConcBuffer

    def +=(b: Body): SectorMatrix = {
      val x0 = boundaries.width / sectorPrecision
      val y0 = boundaries.height / sectorPrecision
      val bx:Float = b.x / x0
      val by:Float = b.y / y0

      println(s"$bx, $by")

      val xLoc = if (bx > x0) x0 else bx
      val yLoc = if (by > y0) y0 else by

      apply(xLoc, yLoc) += b

      this
    }

    def apply(x: Int, y: Int) = matrix(y * sectorPrecision + x)

    def combine(that: SectorMatrix): SectorMatrix = {
      ???
    }


    def toQuad(parallelism: Int): Quad = {
      def BALANCING_FACTOR = 4
      def quad(x: Int, y: Int, span: Int, achievedParallelism: Int): Quad = {
        if (span == 1) {
          val sectorSize = boundaries.size / sectorPrecision
          val centerX = boundaries.minX + x * sectorSize + sectorSize / 2
          val centerY = boundaries.minY + y * sectorSize + sectorSize / 2
          var emptyQuad: Quad = Empty(centerX, centerY, sectorSize)
          val sectorBodies = this(x, y)
          sectorBodies.foldLeft(emptyQuad)(_ insert _)
        } else {
          val nspan = span / 2
          val nAchievedParallelism = achievedParallelism * 4
          val (nw, ne, sw, se) =
            if (parallelism > 1 && achievedParallelism < parallelism * BALANCING_FACTOR) parallel(
              quad(x, y, nspan, nAchievedParallelism),
              quad(x + nspan, y, nspan, nAchievedParallelism),
              quad(x, y + nspan, nspan, nAchievedParallelism),
              quad(x + nspan, y + nspan, nspan, nAchievedParallelism)
            )
            else (
              quad(x, y, nspan, nAchievedParallelism),
              quad(x + nspan, y, nspan, nAchievedParallelism),
              quad(x, y + nspan, nspan, nAchievedParallelism),
              quad(x + nspan, y + nspan, nspan, nAchievedParallelism)
            )
          Fork(nw, ne, sw, se)
        }
      }

      quad(0, 0, sectorPrecision, 1)
    }

    override def toString = s"SectorMatrix(#bodies: ${matrix.map(_.size).sum})"
  }

  class TimeStatistics {
    private val timeMap = collection.mutable.Map[String, (Double, Int)]()

    def clear() = timeMap.clear()

    def timed[T](title: String)(body: => T): T = {
      var res: T = null.asInstanceOf[T]
      val totalTime = /*measure*/ {
        val startTime = System.currentTimeMillis()
        res = body
        (System.currentTimeMillis() - startTime)
      }

      timeMap.get(title) match {
        case Some((total, num)) => timeMap(title) = (total + totalTime, num + 1)
        case None => timeMap(title) = (0.0, 0)
      }

      println(s"$title: ${totalTime} ms; avg: ${timeMap(title)._1 / timeMap(title)._2}")
      res
    }

    override def toString = {
      timeMap map {
        case (k, (total, num)) => k + ": " + (total / num * 100).toInt / 100.0 + " ms"
      } mkString ("\n")
    }
  }
}
