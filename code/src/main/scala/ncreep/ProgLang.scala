package ncreep

import scala.util.Random

object ProgLang {

  sealed trait ProgAction[+A] {
    def map[B](f: A => B): ProgAction[B] = this match {
      case DrinkCoffee(c, next) => DrinkCoffee(c, next andThen f)
      case WriteCode(c, next) => WriteCode(c, next andThen f)
      case Compile(c, next) => Compile(c, next andThen f)
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
      case Glue(pa: ProgAction[A]) => Glue(pa.map(_.next(f)))
    }

    def map[B](f: A => B): Program[B] = flatMap(f andThen ret)
    def >>[B](b: B): Program[B] = flatMap(_ => ret(b))
  }
  case class Glue[A](pa: ProgAction[Program[A]]) extends Program[A]
  case class Return[A](a: A) extends Program[A]
  def ret[A](a: A) = Return(a)

  def withGlue[A](pa: ProgAction[A]): Program[A] = Glue(pa.map(ret))

  def compile(c: Code): Program[CompileResponse] = withGlue(Compile(c, identity))
  def writeCode(c: Coffee): Program[Code] = withGlue(WriteCode(c, identity))
  def drinkCoffee(c: Cups): Program[Coffee] = withGlue(DrinkCoffee(c, identity))
  def bangHeadOnKeyboard: Program[Nothing] = withGlue(BangHeadOnKeyboard)
  def commitCode(c: Code): Program[Nothing] = withGlue(CommitCode(c))

  val codeToResponse = Map(
    "java code" -> WaitTime(1),
    "scala code" -> WaitTime(20),
    "implicits spaghetti" -> WaitTime(400),
    "null abuse" -> Exception,
    "higher kinded madness" -> ScaryTypeException)
  def getCode[A](p: Program[Code]): Either[BangHeadOnKeyboard.type, Code] = p match {
    case Return(a) => Right(a)
    case Glue(pa) => pa match {
      case Compile(code, next) => getCode(next(codeToResponse(code)))
      case WriteCode(cofee, next) => {
        val code = Random.shuffle(codeToResponse.keys).head
        getCode(next(code))
      }
      case DrinkCoffee(cups, next) => getCode(next(cups * 3))
      case BangHeadOnKeyboard => Left(BangHeadOnKeyboard)
      case CommitCode(code) => Right(code)
    }
  }
  def countCoffee(p: Program[_], coffeeCount: Coffee = 0): Coffee = p match {
    case Return(a) => coffeeCount
    case Glue(pa) => pa match {
      case Compile(code, next) => countCoffee(next(codeToResponse(code)), coffeeCount)
      case WriteCode(cofee, next) => {
        val code = Random.shuffle(codeToResponse.keys).head
        countCoffee(next(code), coffeeCount)
      }
      case DrinkCoffee(cups, next) => {
        val coffee = cups * 3
        countCoffee(next(coffee), coffeeCount + coffee)
      }
      case BangHeadOnKeyboard => coffeeCount
      case CommitCode(code) => coffeeCount
    }
  }

  def print[A](p: Program[_]): Unit = p match {
    case Return(a) => println(a)
    case Glue(pa) => pa match {
      case Compile(code, next) =>
        println(s"Compile: '$code'"); print(next(WaitTime(3)))
      case WriteCode(cofee, next) =>
        println(s"Write with $cofee coffee"); print(next("some code"))
      case DrinkCoffee(cups, next) =>
        println(s"Drink coffee: $cups"); print(next(cups * 3))
      case BangHeadOnKeyboard => println("Bang head on keyboard")
      case CommitCode(code) => println(s"Commit: '$code'")
    }
  }

  def idealProgrammer: Program[Code] =
    for {
      cof <- drinkCoffee(1)
      code <- writeCode(cof)
      cr <- compile(code)
      finalCode <- cr match {
        case WaitTime(_) => ret(code)
        case Exception => idealProgrammer
        case ScaryTypeException => idealProgrammer
      }
    } yield finalCode

  val drink: Program[Coffee] = Glue(
      DrinkCoffee(3, 
          coffee => Return(coffee)))
  val drink2: Program[Coffee] = Glue(DrinkCoffee(3, coffee1 => Glue(DrinkCoffee(6, coffee2 => Return(coffee1 + coffee2)))))
  val write: Program[Code] = Glue(
      WriteCode(3, 
          code => Return(code)))
  def write(c: Coffee): Program[Code] = Glue(WriteCode(c, code => Return(code)))
  val drinkWrite: Program[Code] = Glue(
      DrinkCoffee(1, 
          coffee => Glue(
              WriteCode(coffee, 
                  code => Return(code)))))
  val drinkWriteCompile: Program[CompileResponse] = Glue(DrinkCoffee(1, cof => Glue(WriteCode(cof, code => Glue(Compile(code, cr => Return(cr)))))))
  val drinkWrite2: Program[Code] = Glue(DrinkCoffee(1, cof => Return(cof))) flatMap (cof => Glue(WriteCode(cof, code => Return(code))))

  def makeCode: Program[Code] =
    for {
      cof <- drinkCoffee(1)
      code <- writeCode(cof)
    } yield code

  def makeCode2: Program[Code] = drinkCoffee(1) flatMap (cof => writeCode(cof))
  def makeCode3: Program[Code] = Glue(DrinkCoffee(1, cof => Glue(WriteCode(cof, code => Return(code)))))

  def idealProgrammer2: Program[Code] =
    drinkCoffee(1) flatMap (
      cof => writeCode(cof) flatMap (
        code => compile(code) flatMap (
          cr => cr match {
            case WaitTime(_) => ret(code)
            case Exception => idealProgrammer2
            case ScaryTypeException => idealProgrammer2
          })))

  def idealProgrammer3: Program[Code] = Glue(DrinkCoffee(1,
    cof => Glue(WriteCode(cof,
      code => Glue(Compile(code,
        cr =>
          cr match {
            case WaitTime(_) => ret(code)
            case Exception => idealProgrammer3
            case ScaryTypeException => idealProgrammer3
          }))))))

  def nervous: Program[Code] =
    for {
      coffee <- drinkCoffee(1)
      code <- writeCode(coffee)
      response <- compile(code)
      finalCode <- response match {
        case WaitTime(t) => drinkCoffee(3 * t) >> code
        case Exception => drinkCoffee(1) flatMap (_ => nervous)
        case ScaryTypeException => bangHeadOnKeyboard
      }
    } yield finalCode
    
    

  def main(args: Array[String]): Unit = {
    println(getCode(nervous flatMap (c => commitCode(c))))
  }
}