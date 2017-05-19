package casestudy2.5

class Pouring(capacity: Vector[int]) {

  // States
  type State = Vector[Int]
  // All empty glasses, the vector all set to zero
  val initialState = capacity map (x => 0)

}
