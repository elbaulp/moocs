class Pouring(capacity: Vector[Int]){
  // States
  type State = Vector[Int]

  val initialState = capacity map(_ => 0)

  // moves

  trait Move {
    // track how each move changes the state
    def change(state: State): State
  }
  case class Empty(glass: Int) extends Move {
    override def change(state: State): State = state updated (glass, 0)
  }
  case class Fill(glass: Int) extends Move {
    override def change(state: State): State = state updated (glass, capacity(glass))
  }
  case class Pour(from: Int, to: Int) extends Move {
    override def change(state: State): State = {
      val amountPoured = state(from) min (capacity(to) - state(to))
      state updated (from, state(from) - amountPoured) updated (to, state(to) + amountPoured)
    }
  }

  // Generate all possible moves

  val glasses = 0 until capacity.length

  val moves =
    (for (g <- glasses) yield Empty(g)) ++
    (for (g <- glasses) yield Fill(g)) ++
    (for (from <- glasses; to <- glasses; if from != to) yield Pour(from, to))

  // Pahts (Sequences of moves)

  class Path(history: List[Move], val endState: State) {
    def extend(move: Move) = new Path(move :: history, move change endState)

    override def toString = (history.reverse mkString " ") + " --> " + endState
    //override def toString = history mkString " "
    /* private def trackState(xs: List[Move]): State = xs match {
     *   case Nil => initialState
     *   case move :: xs1 => move change trackState(xs1)
     * } */
  }

  val initialPath = new Path(Nil, initialState)

  def from(paths: Set[Path], explored: Set[State] = Set(initialState)): Stream[Set[Path]] =
    if (paths.isEmpty) Stream.empty
    else {
      val more = for {
        path <- paths
        next <- moves map path.extend
        if !(explored contains next.endState)
      } yield next
      /* val more =
       *   paths flatMap { path =>
       *     println("PAHT: " + path)
       *     (moves map path.extend) map { next =>
       *       println("NEXT : " + next)
       *       next
       *     }
       *   } */
      paths #:: from(more, explored ++ (more map (_.endState)))
    }

  val pathSets = from(Set(initialPath))
  def solutions(target: Int): Stream[Path] =
    for {
      pathSet <- pathSets
      path <- pathSet
      if path.endState contains target
    } yield path
}


object main extends App {
  val problem = new Pouring(Vector(4, 9, 19))

  println(problem.moves)

  println(problem.solutions(2).take(10).toList.mkString("\n\n"))
}
