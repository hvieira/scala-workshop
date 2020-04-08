package katas.bank

trait AccountService {
  def deposit(amount: Int): Unit
  def withdraw(amount: Int): Unit
  def printStatement(): Unit
}

trait BankAccountOperations {
  def registerOperation(op: BankAccountOperation): Unit
  def getOperations: List[BankAccountOperation]
}

trait DateProvider {
  def get: String
}

trait StatementPrinter {
  def print(statement: String): Unit
}

case class BankAccountOperation(date: String, amount: Int)


class BankAccount(ops: BankAccountOperations,
                  dateProvider: DateProvider,
                  statementPrinter: StatementPrinter) extends AccountService {

  def deposit(amount: Int): Unit =
    ops.registerOperation(BankAccountOperation(dateProvider.get, amount))

  def withdraw(amount: Int): Unit =
    ops.registerOperation(BankAccountOperation(dateProvider.get, -amount))

  def printStatement(): Unit =
    statementPrinter.print(
      ops.getOperations.foldLeft(" Date || Amount") {
        case (statement, op) => statement + s"\n ${op.date} || ${op.amount}"
      }
    )
}