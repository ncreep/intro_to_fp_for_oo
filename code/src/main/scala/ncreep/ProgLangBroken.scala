package ncreep

object ProgLangBroken {

  sealed trait ProgAction

  case class DrinkCoffee(c: Cups, next: Coffee => ProgAction) extends ProgAction
  case class WriteCode(c: Coffee, next: Code => ProgAction) extends ProgAction
  case class Compile(c: Code, next: CompileResponse => ProgAction) extends ProgAction
  case class CommitCode(c: Code) extends ProgAction
  case object BangHeadOnKeyboard extends ProgAction

  sealed trait CompileResponse
  case class WaitTime(t: Time) extends CompileResponse
  case object Exception extends CompileResponse
  case object ScaryTypeException extends CompileResponse

  def main(args: Array[String]): Unit = {
    val prog: ProgAction = DrinkCoffee(3,
      coffee => WriteCode(coffee,
        code => Compile(code,
          compileResponse => compileResponse match {
            case WaitTime(_) => CommitCode(code)
            case _ => BangHeadOnKeyboard
          })))

    val drinkWrite: ProgAction = DrinkCoffee(3,
      coffee => WriteCode(coffee,
        code => ???
      ))

    val compile: Code => ProgAction = code => Compile(code,
      compileResponse => ???)
  }
}