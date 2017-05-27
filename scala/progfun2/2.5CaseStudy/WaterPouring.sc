package casestudy2.5

class Pouring(capacity: Vector[int]) {

  // States
  type State = Vector[Int]
  // All empty glasses, the vector all set to zero
  val initialState = capacity map (x => 0)

  // Moves

  trait Move {
    def change(state: State): State
  }
  case class Empty(glass: Int) extends Move {
    def change(state: State) = state updated (glass, 0)
  }
  case class Fill(glass: Int) extends Move {
    def change(state: State) = state updated (glass, capacity(glass))
  }
  case class Pour(from: Int, to: Int) extends Move {
    def change(state: State) = {
      // Amount that gets pour from a glass to the other
      // Find the min value of this two ints
      val amount = state(from) min (capacity(to) - state(to))
      state updated(from, state(from) - amount) updated (to, state(to) + amount)
    }
  }

  // Generate all possible moves
  val glasses = 0 until capacity.length
  val moves =
    (for (g <- glasses) yield Empty(g)) ++
    (for (g <- glasses) yield Fill(g)) ++
    (for (from <- glasses; to <- glasses if from != to) yield Pour(from, to))

}
