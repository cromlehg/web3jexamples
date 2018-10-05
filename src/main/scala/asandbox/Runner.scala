package asandbox

object Runner extends App {

  val tes1: Option[String] = Some("1")

  val tes2: Option[String] = Some("2")

  val tes3: Option[String] = Some("3")

  (for {
    t1 <- tes1
    t2 <- tes2
    t3 <- tes3
  } yield (t1, t2, t3)) map {
    case (t1, t2, t3) =>
      println(t1 + " " + t2 + " " + t3)
  }
}