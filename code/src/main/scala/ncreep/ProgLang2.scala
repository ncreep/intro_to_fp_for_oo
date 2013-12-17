package ncreep

object ProgLang2 {
  sealed trait CompileResponse
  case class WaitTime(t: Time) extends CompileResponse
  case object Exception extends CompileResponse
  case object ScaryTypeException extends CompileResponse

  sealed trait ReviewResponse
  case object Good extends ReviewResponse
  case object Bad extends ReviewResponse

  sealed trait ProgAction[+A] {
    def map[B](f: A => B): ProgAction[B]
  }

  case class Compile[A](next: CompileResponse => A) extends ProgAction[A] {
    def map[B](f: A => B) = Compile(next andThen f)
  }

  case class WriteCode[A](c: Code, next: ReviewResponse => A) extends ProgAction[A] {
    def map[B](f: A => B) = WriteCode(c, next andThen f)
  }

  case class DrinkCoffee[A](c: Coffee, next: AboveCoffeeLimit => A) extends ProgAction[A] {
    def map[B](f: A => B) = DrinkCoffee(c, next andThen f)
  }

  case object BangHeadOnKeyboard extends ProgAction[Nothing] {
    def map[B](f: Nothing => B) = BangHeadOnKeyboard
  }

  case object GoHome extends ProgAction[Nothing] {
    def map[B](f: Nothing => B) = GoHome
  }

  sealed trait Program[+A] {
    def flatMap[B](f: A => Program[B]): Program[B] = this match {
      case Glue(pa) => Glue(pa.map(_.flatMap(f)))
      case Return(a) => f(a)
    }

    def next[B](f: A => Program[B]): Program[B] = this match {
      case Glue(pa) => Glue(pa.map(_.flatMap(f)))
      case Return(a) => f(a)
    }
    
    def map[B](f: A => B): Program[B] = flatMap(f andThen ret)
    def >>[B](b: B): Program[B] = flatMap(_ => ret(b))
  }
  case class Glue[A](pa: ProgAction[Program[A]]) extends Program[A]
  case class Return[A](a: A) extends Program[A]
  def ret[A](a: A) = Return(a)

  def withGlue[A](pa: ProgAction[A]): Program[A] = Glue(pa.map(ret))

  def compile: Program[CompileResponse] = withGlue(Compile(identity))
  def writeCode(c: Code): Program[ReviewResponse] = withGlue(WriteCode(c, identity))
  def drinkCoffee(c: Coffee): Program[AboveCoffeeLimit] = withGlue(DrinkCoffee(c, identity))
  def bangHeadOnKeyboard: Program[Nothing] = withGlue(BangHeadOnKeyboard)
  def goHome: Program[Nothing] = withGlue(GoHome)

  def idealProgrammer: Program[Unit] =
    Glue(WriteCode("abc", rr => rr match {
      case Good => Glue(Compile(cr => cr match {
        case _ => ret ()
      }))
      case Bad => idealProgrammer
    }))

  def idealProgrammer2: Program[Unit] =
    for {
      rr <- writeCode("rst")
    } yield rr match {
      case Good => compile >> ()
      case Bad => idealProgrammer2
    }

  def idealProgrammer3: Program[Unit] =
    writeCode("rst") flatMap (_ match {
      case Good => compile >> ()
      case Bad => idealProgrammer3
    })

  def interpret(p: Program[Unit]): Unit = p match {
    case Return(a) => println(a)
    case Glue(pa) => pa match {
      case Compile(n) => println('compile); interpret(n(Exception))
      case WriteCode(c, n) => println(s"write: $c"); interpret(n(Good))
      case DrinkCoffee(c, n) => println(s"drink: $c"); interpret(n(true))
      case BangHeadOnKeyboard => println('bang)
      case GoHome => println('go)
    }
  }
  
  def nervous: Program[Unit] = 
    writeCode("art") flatMap (_ match {
      case Good => compile flatMap (_ match  {
        case WaitTime(t) if t > 9 => drinkCoffee(3 * t) flatMap (if (_) bangHeadOnKeyboard else ret ())
        case WaitTime(_) => ret ()
        case Exception => drinkCoffee(3) flatMap (if (_) bangHeadOnKeyboard else nervous)
        case ScaryTypeException => bangHeadOnKeyboard
      })
      case Bad => drinkCoffee(3) >> nervous
    })

  def main(args: Array[String]): Unit = {

    val x = idealProgrammer3 flatMap (_ => idealProgrammer3)
    interpret(nervous >> idealProgrammer3)

  }
}