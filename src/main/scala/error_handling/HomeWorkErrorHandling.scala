package error_handling

object HomeWorkErrorHandling {

  import cats.data._
  import cats.implicits._

  // Type Wrappers and Aliases
  implicit class Name(val unwrap: String) extends AnyVal
  implicit class Number(val unwrap: BigInt) extends AnyVal
  type Date = java.util.Date

  // Either => Validated
  implicit def eitherToValidateNel[A,B]: Either[A,B] => ValidatedNel[A,B] = {
    case Left(x) => x.invalidNel
    case Right(x) => x.validNel     //ValidNel(x)
  }

  implicit class toEitherTotoValidNel[A,B](f: A => Either[ValidationError,B]) {
    val toValidateNel: A => ValidatedNel[ValidationError,B] = f andThen eitherToValidateNel
  }

  /* Traits */

  sealed trait CommonValidationInterface[A] {

    val fs: List[A => Either[ValidationError,()]]  //  all basic error checkers

    // convert list of basic error checkers into combined validatedNel checker
    def validate: List[A => Either[ValidationError,A]] =>
      A => ValidatedNel[List[ValidationError], A] = {
      fs => fs.foldRight( (x:A)   => x.validNel )
      { (f,acc) => f *> acc }
    }

    def diagnostic[X,Y]: List[X => Either[ValidationError,Y]] => List[X] => List[ValidationError] = {
      fs => xs => ( fs <*> xs ) filter (_.isLeft) map {case Left(x) => x}
    }

    // Total Check:
    def check(x: A): List[ValidationError] = diagnostic(fs)(List(x))

  }

  // Number Trait <- CardNumber & SecurityCode
  sealed trait NumberValidationInterface extends CommonValidationInterface[Number] {

    val length: Int

    // Errors:
    final case object WrongLength extends ValidationError // wrong length
    final case object WrongSymbols extends ValidationError


    implicit def NameToString: Number => String = _.unwrap.toString
    final override val fs = List(checkLength,checkSymbols)

    // Basic Checks:
    def checkLength: Number => Either[ValidationError, ()] = {
      num => if (num.length != length) Left(this.WrongLength)
      else Right(())
    }

    def checkSymbols: Number => Either[ValidationError, ()] =
      num => if (NameToString(num) forall (_.isDigit)) Right(())
      else Left(this.WrongSymbols)

  }


  // Name Trait <- Name
  sealed trait NameValidationInterface extends CommonValidationInterface[Name] {

    val minLength: Int
    val maxLength: Int

    //Errors
    final case object TooLong extends ValidationError // wrong length
    final case object TooShort extends ValidationError // wrong length
    final case object WrongSymbols extends ValidationError
    final case object WrongCase extends ValidationError // e.g. ivan -> error ; Ivan -> ok!

    implicit def NameToString: Name => String = _.unwrap
    final override val fs = List(checkLength,checkSymbols,checkCase)

    def checkLength: Name => Either[ValidationError, ()] = {
      str => if (str.length > maxLength) Left(this.TooLong)
      else if (str.length < minLength) Left(this.TooShort)
      else Right(())
    }

    def checkSymbols: Name => Either[ValidationError, ()] =
      str => if (NameToString(str) forall (_.isLetter)) Right(())
      else Left(this.WrongSymbols)

    def checkCase: Name => Either[ValidationError, ()] =
      str => NameToString(str).toList match {
        case Nil => Left(this.TooShort)
        case (x :: xs) => if (x.isLower || xs.exists(_.isUpper)) Left(this.WrongCase)
        else Right(())
      }

  }

  sealed trait DateValidationInterface extends CommonValidationInterface[Date] {

    val today = new Date();
    final override val fs = List(checkExpiration)

    final case object Expired extends ValidationError // when today is after

    def checkExpiration: Date => Either[ValidationError, ()] =
      date => if (today before date ) Right(())
      else Left(this.Expired)

  }


  case class PaymentCard private (name: Name, number: Number, expirationDate: Date, securityCode: Number)
  object PaymentCard extends CommonValidationInterface[PaymentCard] {

    //private override val fs: List[PaymentCard => Either[ValidationError, ()]] = _
    override val fs: List[PaymentCard => Either[ValidationError, Unit]] = {

      def mapAdjCompose[A]: (List[A => Either[ValidationError, A]]) =>
        (PaymentCard => A) =>
          (List[PaymentCard => Either[ValidationError, A]]) = {
        fs => g => ( List(g), fs ).mapN( (f,g) => f.andThen(g) )
      }

      val fs_name = mapAdjCompose {ValidationError.Name.fs} {card: PaymentCard => card.name}
      val fs_number = mapAdjCompose {ValidationError.Number.fs} {card: PaymentCard => card.number}
      val fs_date = mapAdjCompose {ValidationError.ExpDate.fs} {card: PaymentCard => card.expirationDate}
      val fs_security = mapAdjCompose {ValidationError.SecurityCode.fs} {card: PaymentCard => card.securityCode}

      fs_name ::: fs_number ::: fs_date ::: fs_security

    }

    def create(name: String,
               number: Number,
               expirationDate: Date,
               securityCode: Number)
    : Either[List[ValidationError],PaymentCard] = {
      val protoCard = new PaymentCard(name,number,expirationDate,securityCode)
      val errorList = check(protoCard)
      if (errorList isEmpty) Right(protoCard)
      else Left(errorList)
    }

  }



  sealed trait ValidationError
  object ValidationError {

    object Name extends NameValidationInterface {
      override val minLength: Int = 3
      override val maxLength: Int = 25
    }

    object Number extends NumberValidationInterface {
      override val length: Int = 16
    }

    object SecurityCode extends NumberValidationInterface {
      override val length: Int = 3
    }

    object ExpDate extends DateValidationInterface



  }


  object PaymentCardValidator {

    type AllErrorsOr[A] = ValidatedNec[ValidationError, A]

    def validate( name: String,
                  number: String,
                  expirationDate: String,
                  securityCode: String): AllErrorsOr[PaymentCard] = {
      //PaymentCard.create(name,BigInt(number),expirationDate,BigInt(securityCode))
      ???
    }

    //TODO: Write converter: Either[List[E], A] => ValidatedNec[E, A]
    // List[E] => NonEmptyChain[E]
    //

    //Question about Writer

  }


}
