package ncreep

object OptionExamples {

  // choose a random item from the two
  def choose[A](x: A, y: A): A = if (util.Random.nextDouble() < 0.5) x else y

  object FP {
    case class Coffee(amount: Int) {
      def writeCode(): Option[Code] = choose(Some(Code(s"$amount coffee => code")), None)
    }
    case class Code(code: String) {
      def tryCompile(): Option[Compiled] = choose(Some(Compiled(code)), None)
    }

    case class Compiled(code: String)

    def getCoffee(): Option[Coffee] = choose(Some(Coffee(3)), None)

    val compiledCode: Option[Compiled] = getCoffee match {
      case Some(coffee) => coffee.writeCode() match {
        case Some(code) => code.tryCompile()
        case None => None
      }
      case None => None
    }

    val compiledCode2: Option[Compiled] =
      getCoffee flatMap { coffee =>
        coffee.writeCode() flatMap { code =>
          code.tryCompile()
        }
      }

    val compiledCode3: Option[Compiled] =
      for {
        coffee <- getCoffee
        code <- coffee.writeCode()
        compiled <- code.tryCompile()
      } yield compiled
  }

  object Null {
    case class Coffee(amount: Int) {
      def writeCode(): Code = choose(Code(s"$amount coffee => code"), null)
    }

    case class Code(code: String) {
      def tryCompile(): Compiled = choose(Compiled(code), null)
    }

    case class Compiled(code: String)

    def getCoffee(): Coffee = choose(Coffee(3), null)

    val compiledCode = {
      val coffee = getCoffee()
      if (coffee == null) null
      else {
        val code = coffee.writeCode()
        if (code == null) null
        else code.tryCompile()
      }
    }
  }

  object NullObject {
    trait Coffee { def writeCode(): Code }
    case class RealCoffee(amount: Int) extends Coffee {
      def writeCode(): Code = choose(RealCode(s"$amount coffee => code"), NoCode)
    }
    case object NoCoffee extends Coffee { def writeCode(): Code = NoCode }

    trait Code { def tryCompile(): Compiled }
    case class RealCode(code: String) extends Code {
      def tryCompile(): Compiled = choose(RealCompiled(code), NotCompiled)
    }
    case object NoCode extends Code { def tryCompile(): Compiled = NotCompiled }

    trait Compiled
    case class RealCompiled(code: String) extends Compiled
    case object NotCompiled extends Compiled

    def getCoffee(): Coffee = choose(RealCoffee(3), NoCoffee)

    val compiledCode = getCoffee().writeCode().tryCompile()
  }

  def main(args: Array[String]): Unit = {
    println(NullObject.compiledCode)
  }
}