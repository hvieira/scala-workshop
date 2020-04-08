package katas.bank

import org.scalamock.scalatest.MockFactory
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class BankAccountSpec extends AnyWordSpec with Matchers with MockFactory {

  "BankAccount" should {

    "keep track of deposit operations" in {
      val bankAccountOperations = mock[BankAccountOperations]
      val dateProvider = mock[DateProvider]
      val statementPrinter = mock[StatementPrinter]
      val sut = new BankAccount(bankAccountOperations, dateProvider, statementPrinter)

      (dateProvider.get _).expects().returns("14/01/2012").once()
      (dateProvider.get _).expects().returns("14/02/2012").once()
      (dateProvider.get _).expects().returns("01/10/2015").once()

      (bankAccountOperations.registerOperation _).expects(BankAccountOperation("14/01/2012", 7)).once()
      (bankAccountOperations.registerOperation _).expects(BankAccountOperation("14/02/2012", 333)).once()
      (bankAccountOperations.registerOperation _).expects(BankAccountOperation("01/10/2015", 11)).once()

      sut.deposit(7)
      sut.deposit(333)
      sut.deposit(11)
    }

    "keep track of withdraw operations" in {
      val bankAccountOperations = mock[BankAccountOperations]
      val dateProvider = mock[DateProvider]
      val statementPrinter = mock[StatementPrinter]
      val sut = new BankAccount(bankAccountOperations, dateProvider, statementPrinter)

      (dateProvider.get _).expects().returns("14/01/2012").once()
      (dateProvider.get _).expects().returns("14/02/2012").once()
      (dateProvider.get _).expects().returns("01/10/2015").once()

      (bankAccountOperations.registerOperation _).expects(BankAccountOperation("14/01/2012", -7)).once()
      (bankAccountOperations.registerOperation _).expects(BankAccountOperation("14/02/2012", -333)).once()
      (bankAccountOperations.registerOperation _).expects(BankAccountOperation("01/10/2015", -11)).once()

      sut.withdraw(7)
      sut.withdraw(333)
      sut.withdraw(11)
    }

    "retrieve and print the statement" in {
      val bankAccountOperations = mock[BankAccountOperations]
      val dateProvider = mock[DateProvider]
      val statementPrinter = mock[StatementPrinter]
      val sut = new BankAccount(bankAccountOperations, dateProvider, statementPrinter)

      (bankAccountOperations.getOperations _).expects().returns(
        List(
          BankAccountOperation("01/10/2015", -11),
          BankAccountOperation("14/02/2012", 333),
          BankAccountOperation("14/01/2012", -7),
        )
      ).once()

      (statementPrinter.print _).expects(
        s""" Date || Amount
          | 01/10/2015 || -11
          | 14/02/2012 || 333
          | 14/01/2012 || -7""".stripMargin).once()

      sut.printStatement()
    }

  }

}
