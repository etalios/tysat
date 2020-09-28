package tysat

import scala.annotation.tailrec

final case class Literal(value: Int) extends AnyVal {

  def abs: Literal =
    Literal(math.abs(value))
}

case class Assignment(amap: Map[Literal, Boolean] = Map.empty) {

  def assign(lit: Literal): Option[Boolean] =
    amap
      .get(lit.abs)
      .map(lit.value < 0 ^ _)

  def isDefinedAt(lit: Literal): Boolean =
    amap.isDefinedAt(lit.abs)

  def +(litValue: (Literal, Boolean)) = {
    val (lit, value) = litValue

    Assignment(amap + (lit.abs -> (lit.value < 0 ^ value)))
  }
}

case class Clause(literals: Set[Literal]) {

  def value(assignemnt: Assignment): Option[Boolean] =
    literals.view
      .map(assignemnt.assign)
      .reduce { (x, y) =>
        for {
          xv <- x
          yv <- y
        } yield xv || yv
      }

  def definedLiterals(assignment: Assignment): Set[Literal] =
    literals.filter(!assignment.isDefinedAt(_))
}

case class DPLL(clauses: Set[Clause]) {

  @tailrec
  final def unitPropagation(assignment: Assignment, clauses: Set[Clause] = this.clauses): Assignment = {
    val updatedAssignment =
      clauses.view
        .headOption
        .filter(_.value(assignment).getOrElse(false))
        .map(_.definedLiterals(assignment))
        .filter(_.size == 1)
        .map(_.head)
        .map(lit => assignment + (lit -> true))

    updatedAssignment match {
      case None => assignment
      case Some(updated) => unitPropagation(updated, clauses)
    }
  }

  def select(assignment: Assignment): Literal = {
    clauses.view
      .filter(!_.value(assignment).getOrElse(false))
      .headOption
      .map(_.definedLiterals(assignment))
      .flatMap(_.headOption)
      .get
  }

  def decide(assignment: Assignment, lit: Literal, value: Boolean): Option[Assignment] =
    solve(assignment + (lit -> value))

  def solve(assignment: Assignment): Option[Assignment] = {
    val upAssignment = unitPropagation(assignment)

    if (clauses.exists(_.value(upAssignment) == Some(false))) {
      None
    } else if (clauses.forall(_.value(upAssignment) == Some(true))) {
      Some(upAssignment)
    } else {
      val p = select(upAssignment)

      decide(upAssignment, p, false).orElse(decide(upAssignment, p, true))
    }
  }

  def solve: Option[Assignment] =
    solve(Assignment())
}

object Main extends App {
  val clauses =
    Set(
      Clause(Set(Literal(1), Literal(2), Literal(3))),
      Clause(Set(Literal(-1), Literal(-2))),
      Clause(Set(Literal(-1), Literal(-3))),
      Clause(Set(Literal(-2), Literal(-3)))
    )
  val solver = DPLL(clauses)

  println(solver.solve)
}

