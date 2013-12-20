package ncreep

import scala.util.Random

object ProgLang {

  sealed trait ProgAction[+A] {
    def map[B](f: A => B): ProgAction[B] = this match {
      case DrinkCoffee(c, next: (Coffee => A)) => DrinkCoffee(c, next andThen f)
      case WriteCode(c, next: (Code => A)) => WriteCode(c, next andThen f)
      case Compile(c, next: (CompileResponse => A)) => Compile(c, next andThen f)
      case CommitCode(c) => CommitCode(c)
      case BangHeadOnKeyboard => BangHeadOnKeyboard
    }
  }

  case class DrinkCoffee[A](c: Cups, next: Coffee => A) extends ProgAction[A]
  case class WriteCode[A](c: Coffee, next: Code => A) extends ProgAction[A]
  case class Compile[A](c: Code, next: CompileResponse => A) extends ProgAction[A]
  case class CommitCode(c: Code) extends ProgAction[Nothing]
  case object BangHeadOnKeyboard extends ProgAction[Nothing]

  sealed trait CompileResponse
  case class WaitTime(t: Time) extends CompileResponse
  case object Exception extends CompileResponse
  case object ScaryTypeException extends CompileResponse

  sealed trait Program[+A] {
    def flatMap[B](f: A => Program[B]): Program[B] = next(f)

    def next[B](f: A => Program[B]): Program[B] = this match {
      case Return(a) => f(a)
      case Glue(pa: ProgAction[Program[A]]) => {
        val progActionB: ProgAction[Program[B]] =
          pa map { (p: Program[A]) =>
            val pb: Program[B] = p next f
            pb
          }
        Glue(progActionB)
      }
    }

    def map[B](f: A => B): Program[B] = flatMap(f andThen ret)
  }
  case class Glue[A](pa: ProgAction[Program[A]]) extends Program[A]
  case class Return[A](a: A) extends Program[A]

  def ret[A](a: A) = Return(a)

  val drinkCoffee: Cups => Program[Coffee] = cups => Glue(DrinkCoffee(cups, coffee => ret(coffee)))
  val writeCode: Coffee => Program[Code] = coffee => Glue(WriteCode(coffee, code => ret(code)))
  val compile: Code => Program[CompileResponse] = code => Glue(Compile(code, cr => ret(cr)))
  val bangHeadOnKeyboard: Program[Nothing] = Glue(BangHeadOnKeyboard)
  val commitCode: Code => Program[Nothing] = code => Glue(CommitCode(code))

  // factoring out a common function
  //  def withGlue[A](pa: ProgAction[A]): Program[A] = Glue(pa.map(ret))
  //  val drinkCoffee: Cups => Program[Coffee] = cups => withGlue(DrinkCoffee(cups, identity))
  //  val writeCode: Coffee => Program[Code] = coffee => withGlue(WriteCode(coffee, identity))
  //  val compile: Code => Program[CompileResponse] = code => withGlue(Compile(code, identity))
  //  val bangHeadOnKeyboard: Program[Nothing] = withGlue(BangHeadOnKeyboard)
  //  val commitCode: Code => Program[Nothing] = code => withGlue(CommitCode(code))

  val codeToResponse = Map(
    "java code" -> WaitTime(1),
    "scala code" -> WaitTime(20),
    "implicits spaghetti" -> WaitTime(400),
    "null abuse" -> Exception,
    "higher kinded madness" -> ScaryTypeException)

  def randomCode(): Code = Random.shuffle(codeToResponse.keys.toList).head

  def print(p: Program[_]): Unit = p match {
    case Return(a) => println(a)
    case Glue(pa) => pa match {
      case DrinkCoffee(cups, next) =>
        println(s"Drink coffee: $cups")
        print(next(cups * 3))
      case WriteCode(cofee, next) =>
        println(s"Write with $cofee coffee")
        print(next(randomCode()))
      case Compile(code, next) =>
        println(s"Compile: '$code'")
        print(next(codeToResponse(code)))
      case CommitCode(code) => println(s"Commit: '$code'")
      case BangHeadOnKeyboard => println("Bang head on keyboard")
    }
  }

  def getCode(p: Program[Code]): Either[ProgAction[_], Code] = p match {
    case Return(a) => Right(a)
    case Glue(pa) => pa match {
      case DrinkCoffee(cups, next) => getCode(next(cups * 3))
      case WriteCode(cofee, next) => getCode(next(randomCode()))
      case Compile(code, next) => getCode(next(codeToResponse(code)))
      case CommitCode(code) => Right(code)
      case BangHeadOnKeyboard => Left(BangHeadOnKeyboard)
    }
  }

  def countCoffee(p: Program[_], coffeeCount: Coffee = 0): Coffee = p match {
    case Return(a) => coffeeCount
    case Glue(pa) => pa match {
      case DrinkCoffee(cups, next) => {
        val coffee = cups * 3
        countCoffee(next(coffee), coffeeCount + coffee)
      }
      case WriteCode(cofee, next) => countCoffee(next(randomCode()), coffeeCount)
      case Compile(code, next) => countCoffee(next(codeToResponse(code)), coffeeCount)
      case CommitCode(code) => coffeeCount
      case BangHeadOnKeyboard => coffeeCount
    }
  }

  val drink: Program[Coffee] =
    Glue(
      DrinkCoffee(3,
        coffee => Return(coffee)))

  val drinkWrite: Program[Code] =
    Glue(
      DrinkCoffee(1,
        coffee => Glue(
          WriteCode(coffee,
            code => Return(code)))))

  val write: Program[Code] =
    Glue(WriteCode(3, code => Return(code)))
  val comp: Code => Program[CompileResponse] =
    code => Glue(Compile(code, compileResp => Return(compileResp)))

  val writeCompile: Program[CompileResponse] =
    write next comp
  val writeCompile2: Program[CompileResponse] =
    writeCode(3) next compile

  val idealProgrammer: Program[Code] =
    drinkCoffee(1) next writeCode next { code =>
      compile(code) next { response =>
        response match {
          case WaitTime(_) => ret(code)
          case Exception => idealProgrammer
          case ScaryTypeException => idealProgrammer
        }
      }
    }

  val nervousProgrammer: Program[Code] =
    drinkCoffee(1) next writeCode next { code =>
      compile(code) next { response =>
        response match {
          case WaitTime(t) => drinkCoffee(3 * t) next (_ => ret(code))
          case Exception => drinkCoffee(2) next (_ => nervousProgrammer)
          case ScaryTypeException => bangHeadOnKeyboard
        }
      }
    }

  def idealProgrammer2: Program[Code] =
    for {
      code <- drinkCoffee(1) next writeCode
      response <- compile(code)
      finalCode <- response match {
        case WaitTime(_) => ret(code)
        case Exception => idealProgrammer2
        case ScaryTypeException => idealProgrammer2
      }
    } yield finalCode

  def nervousProgrammer2: Program[Code] =
    for {
      code <- drinkCoffee(1) next writeCode
      response <- compile(code)
      finalCode <- response match {
        case WaitTime(t) => drinkCoffee(3 * t) next (_ => ret(code))
        case Exception => drinkCoffee(2) next (_ => nervousProgrammer2)
        case ScaryTypeException => bangHeadOnKeyboard
      }
    } yield finalCode

  def main(args: Array[String]): Unit = {
    print(idealProgrammer next commitCode)
    print(nervousProgrammer next commitCode)
    println(getCode(idealProgrammer next commitCode))
    println(countCoffee(idealProgrammer next commitCode))
  }
}