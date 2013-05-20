
def printException[A](a: => A): Unit =
  try a catch { case t: Throwable => println(t) }

def example[A](name: String)(fn: => A): Unit = {
  println(" --- %s ------------------".format(name))
  fn
  println()
}

object OptionExamples extends App {

  example("Constructing Options") {
    import scalaz.std.option._

    val os = List(Some(42), None, Some(20))
    //os.foldLeft(None) { (acc, o) => acc orElse o }
    println(os.foldLeft(None: Option[Int]) { (acc, o) => acc orElse o })
    println(os.foldLeft(none[Int]) { (acc, o) => acc orElse o })
  }

  example("Conditional") {
    import scalaz.syntax.std.option._

    def xget(opt: Option[Int]) = opt | Int.MaxValue
    println(xget(Some(42)))
    println(xget(None))
  }

  example("Unary ~") {
    import scalaz.std.anyVal._
    import scalaz.std.option._
    import scalaz.syntax.std.option._

    def zget(opt: Option[Int]) = ~opt
    println(zget(Some(42)))
    println(zget(None))
  }

  example("Unary ~ with monoids") {
    import scalaz.Monoid
    import scalaz.std.anyVal._
    import scalaz.std.string._
    import scalaz.std.list._
    import scalaz.std.option._
    import scalaz.syntax.std.option._

    def mzget[A : Monoid](opt: Option[A]) = ~opt
    println(mzget(none[Int]))
    println(mzget(none[String]))
    println(mzget(none[List[Int]]))
  }

  example("err") {
    import scalaz.std.option._
    import scalaz.syntax.std.option._

    def knowBetter[A](opt: Option[A]) =
      opt err "You promised!"
    println(knowBetter(some(42)))
    printException(knowBetter(none))
  }

  example("cata") {
    import scalaz.syntax.std.option._
    import java.net.InetAddress

    def ipAddress(opt: Option[InetAddress]) =
      opt.cata(_.getHostAddress, "0.0.0.0")
    println(ipAddress(Some(InetAddress.getLocalHost)))
    println(ipAddress(None))
  }

  example("ifNone") {
    import scalaz.std.option._
    import scalaz.syntax.std.option._

    var cnt = 0
    some(42) ifNone { cnt += 1 }
    none ifNone { cnt += 1 }
    println(cnt)
  }

  example("monoid") {
    import scalaz.std.anyVal._
    import scalaz.std.option._
    import scalaz.syntax.monoid._
    import scalaz.syntax.std.option._

    println(some(20) |+| none |+| some(22))
    println(some(20).first |+| none.first |+| some(22).first)
    println(some(20).last |+| none.last |+| some(22).last)
  }

  example("traverse") {
    import scalaz.std.anyVal._
    import scalaz.std.option._
    import scalaz.std.list._
    import scalaz.syntax.traverse._

    println(List(some(42), none, some(51)).concatenate)
  }

  example("sequence") {
    import scalaz.std.option._
    import scalaz.std.list._
    import scalaz.syntax.traverse._

    println(List(some(42), none, some(51)).sequence)
    println(List(some(42), some(51)).sequence)
  }
}

object SemigroupAndMonoidExamples extends App {

  example("adding int via monoid") {
    import scalaz.syntax.monoid._
    import scalaz.std.anyVal._

    println(1 |+| 2)
  }

  example("adding lists via monoid") {
    import scalaz.syntax.monoid._
    import scalaz.std.anyVal._
    import scalaz.std.list._

    println(List(1, 2) |+| List(3, 4))
  }

  example("adding multimaps via monoid") {
    import scalaz.syntax.monoid._
    import scalaz.std.map._
    import scalaz.std.set._

    val low = Map('odd -> Set(1, 3), 'even -> Set(2, 4))
    val hi = Map('odd -> Set(5, 7), 'even -> Set(6, 8))
    println(low |+| hi)
  }
}

object ApplicativeExamples extends App {
  import scalaz.std.option._
  import scalaz.syntax.applicative._

  example("applying a value to a function in a context") {
    val oa5: Option[Int => Int] = Some((_: Int) + 5)
    println(some(15) <*> oa5)
  }

  example("applying a value to a function of multiple arguments") {
    println(some(15).<**>(some(5)) { _ + _ })
    println(some(15).<***>(some(5), some(6)) { _ + _ + _ })
    println(some(15).<****>(some(5), some(6), some(7)) { _ + _ + _ + _ })
  }

  example("using applicative builder") {
    println((some(15) |@| some(5)) apply { _ + _ })
    println((some(15) |@| some(5)).tupled)
  }

  example("using applicative builder with case classes") {
    case class Album(name: String, artist: String)
    println((some("Wilco") ⊛ some("Sky Blue Sky"))(Album))
  }
}

object ValidationExamples extends App {

  example("constructing validations") {
    import scalaz.{Success, Failure}
    println(Success(42))
    println(Failure("boom"))

    import scalaz.Validation
    println(Validation.success(42))
    println(Validation.failure("boom"))

    import scalaz.syntax.validation._
    println(42.success)
    println("boom".fail)

    println(42.success[String])
    println("boom".fail[Int])
  }

  example("option to validation") {
    import scalaz.std.option._
    import scalaz.syntax.std.option._

    println(some(42).toSuccess("boom"))
    println(none[Int].toSuccess("boom"))

    println(some(42).toFailure("boom"))
    println(none[Int].toFailure("boom"))
  }

  example("either to validation") {
    import scalaz.syntax.id._
    import scalaz.Validation._

    println(fromEither(Right(42)))
    println(fromEither(Left("boom")))
  }

  example("throwing to validation") {
    import scalaz.Validation._

    println(fromTryCatch("42".toInt))
    println(fromTryCatch("notAnInt".toInt))
    println(fromTryCatch("notAnInt".toInt).leftMap { _.getMessage })

    import scalaz.syntax.bifunctor._
    println(fromTryCatch("notAnInt".toInt).<-: { _.getMessage })
    println(fromTryCatch("notAnInt".toInt).bimap(_.getMessage, identity))
  }

  example("validation is monad") {
    import java.util.UUID
    import scalaz.Validation
    import Validation._
    import scalaz.syntax.std.option._
    import scalaz.syntax.bifunctor._

    def justMessage[S](v: Validation[Throwable, S]): Validation[String, S] =
      v.<-: { _.getMessage }

    def extractId(metadata: Map[String, String]): Validation[String, UUID] =
      for {
        str <- metadata.get("id").toSuccess("No id property")
        id <- justMessage(fromTryCatch(UUID.fromString(str)))
      } yield id

    println(extractId(Map.empty))
    println(extractId(Map("id" -> "notUuid")))
    println(extractId(Map("id" -> UUID.randomUUID.toString)))
  }

  example("monadic dsl") {
    import java.util.UUID
    import scalaz.Validation
    import Validation._
    import scalaz.syntax.std.option._
    import scalaz.syntax.id._
    import scalaz.syntax.bifunctor._

    def justMessage[S](v: Validation[Throwable, S]): Validation[String, S] =
      v.<-: { _.getMessage }

    def parseUUID(s: String): Validation[Throwable, UUID] =
      fromTryCatch(UUID.fromString(s))

    def extractId(metadata: Map[String, String]): Validation[String, UUID] =
      for {
        str <- metadata.get("id").toSuccess("No id property")
        id <- str |> parseUUID |> justMessage
      } yield id

    println(extractId(Map.empty))
    println(extractId(Map("id" -> "notUuid")))
    println(extractId(Map("id" -> UUID.randomUUID.toString)))
  }

  example("validation is applicative if error type is applicative") {
    import scalaz._
    import Scalaz._

    case class Employee(name: String, level: Int, manager: Boolean)
    type Metadata = Map[String, String]

    def justMessage[S](v: Validation[Throwable, S]): Validation[String, S] =
      v.leftMap { _.getMessage }

    def extractString(metadata: Metadata, key: String): Validation[String, String] =
      metadata.get(key).toSuccess("Missing required property %s".format(key))

    def extractName(metadata: Metadata): Validation[String, String] =
      extractString(metadata, "name") flatMap { name =>
        if (name.isEmpty) "Must have a non-empty name!".fail
        else name.success
      }

    def extractLevel(metadata: Metadata): Validation[String, Int] =
      extractString(metadata, "level").
        flatMap { _.parseInt |> justMessage }.
        flatMap { level =>
          if (level < 1) "Level must be at least 1".fail
          else if (level > 15) "Really?".fail
          else level.success
        }

    def extractManager(metadata: Metadata): Validation[String, Boolean] =
      extractString(metadata, "manager").flatMap { _.parseBoolean |> justMessage }

    def extractEmployee(metadata: Metadata): ValidationNel[String, Employee] = {
      extractName(metadata).toValidationNel |@|
      extractLevel(metadata).toValidationNel |@|
      extractManager(metadata).toValidationNel
    } apply Employee.apply

    println(extractEmployee(Map("name" -> "Turing", "level" -> "15", "manager" -> "false")))
    println(extractEmployee(Map("name" -> "Turing", "level" -> "0", "manager" -> "true")))
    println(extractEmployee(Map("name" -> "Turing", "level" -> "17", "manager" -> "true")))
    println(extractEmployee(Map("name" -> "Turing")))
    println(extractEmployee(Map("name" -> "", "level" -> "17", "manager" -> "notBool")))

    def extractEmployees(metadata: List[Metadata]): ValidationNel[String, List[Employee]] = {
      val vEmps: List[ValidationNel[String, Employee]] =
        metadata map extractEmployee

      //vEmps.sequence[({type λ[a] = ValidationNel[String, a]})#λ, Employee]
      vEmps.sequenceU
    }

    println(extractEmployees(List(
      Map("name" -> "Turing", "level" -> "15", "manager" -> "false"),
      Map("name" -> "Curry", "level" -> "14", "manager" -> "false")
    )))

    println(extractEmployees(List(
      Map("name" -> "Turing", "level" -> "17", "manager" -> "false"),
      Map("name" -> "Curry", "level" -> "14")
    )))
  }
}


object LensExamples extends App {
  import scalaz._
  import Scalaz._

  case class Contact(
    name: Name,
    phone: Phone)

  case class Name(
    salutation: String,
    first: String,
    last: String)

  case class Phone(
    digits: String)

  val contactNameLens = Lens.lensu[Contact, Name]((c, n) => c.copy(name = n), _.name)
  val contactPhoneLens = Lens.lensu[Contact, Phone]((c, p) => c.copy(phone = p), _.phone)

  val nameSalutationLens = Lens.lensu[Name, String]((n, s) => n.copy(salutation = s), _.salutation)
  val nameFirstLens = Lens.lensu[Name, String]((n, f) => n.copy(first = f), _.first)
  val nameLastLens = Lens.lensu[Name, String]((n, l) => n.copy(last = l), _.last)

  val phoneDigitsLens = Lens.lensu[Phone, String]((p, d) => p.copy(digits = d), _.digits)

  val contactFirstNameLens = contactNameLens >=> nameFirstLens

  val seth = Contact(Name("Mr.", "Seth", "Avett"), Phone("555-5555"))
  val scottNoLens = seth.copy(name = seth.name.copy(first = "Scott"))
  println(contactFirstNameLens.set(seth, "Scott"))
  println(contactFirstNameLens.get(seth))
}

OptionExamples.main(Array.empty)
SemigroupAndMonoidExamples.main(Array.empty)
ApplicativeExamples.main(Array.empty)
ValidationExamples.main(Array.empty)
LensExamples.main(Array.empty)
