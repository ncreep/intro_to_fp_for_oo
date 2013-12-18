package ncreep

object OODecorator {
  trait Programmer {
    def makeCode(coffee: Coffee): LOC
    def price: Money
    def acceptProject(l: Language): Boolean
  }

  class BasicProgrammer(experience: XPPoints,
                        coffeeConsumption: Coffee,
                        languages: Set[Language]) extends Programmer {

    /* more coffee -> more code */
    def makeCode(c: Coffee): LOC =
      experience * math.max(0, c - coffeeConsumption)

    def price: Money = experience * languages.size

    def acceptProject(l: Language): Boolean = languages(l)
  }

  abstract class ProgrammerDecorator(p: Programmer) extends Programmer {
    def makeCode(c: Coffee): LOC = p.makeCode(c)
    def price: Money = p.price
    def acceptProject(l: Language): Boolean = p.acceptProject(l)
  }

  /* a programmer that writes tests */
  class TestingProgrammer(p: Programmer) extends ProgrammerDecorator(p) {
    override def makeCode(c: Coffee): LOC = super.makeCode(c - 5) * 3
    override def price: Money = super.price * 2
  }

  /* a programmer that writes documentation */
  class DocumentingProgrammer(p: Programmer) extends ProgrammerDecorator(p) {
    override def price: Money = super.price * 10
  }

  class SaneProgrammer(p: Programmer) extends ProgrammerDecorator(p) {
    override def acceptProject(l): Boolean =
      if (l == "PHP") false
      else super.acceptProject(l)
  }

  /* a programmer that likes learning new languages */
  class PolyglotProgrammer(p: Programmer) extends ProgrammerDecorator(p) {
    override def acceptProject(l): Boolean = !super.acceptProject(l)
  }

  def main(args: Array[String]): Unit = {
    val base = new BasicProgrammer(
      experience = 10,
      coffeeConsumption = 30,
      languages = Set("Scala", "Java", "PHP"))

    val saneDocTest = new SaneProgrammer(new DocumentingProgrammer(new TestingProgrammer(base)))
    val documentingPolyglot = new PolyglotProgrammer(new DocumentingProgrammer(base))

    base.price // 30
    base.makeCode(40) // 100
    base.acceptProject("PHP") // true

    saneDocTest.price // 600
    saneDocTest.makeCode(40) // 150
    saneDocTest.acceptProject("PHP") // false

    documentingPolyglot.price // 300
    documentingPolyglot.makeCode(40) // 100
    documentingPolyglot.acceptProject("PHP") // false
  }

}