/**
 * Activate Library for Scala - Concurrency-Oriented Programming in the
 * Activity Activation Style.
 *
 * Copyright (c)2009-2011 by Corbin "Kip" Kohn
 * All Rights Reserved.
 *
 *  Released under the terms of the Artistic License 2.0; see:
 *   http://www.opensource.org/licenses/artistic-license-2.0.php
 *
*/
package k_k_.concurrent.activate.loiter

import k_k_.concurrent.activate.core.{Event, Activity, Activatom}


object Guard {
  implicit def Event2Guard(event: Event): Guard =
    Existential_Guard(event)
}

sealed abstract class Guard {
  import Guard._

  def unary_!        = Negated_Guard(this)
  def &&(lhs: Guard) = Conjoined_Guard(this, lhs)
  def ||(lhs: Guard) = Disjoined_Guard(this, lhs)

  def ^ (lhs: Guard) = Disjoined_Guard((this && !lhs), (!this && lhs))
 
  def ?+>(a: Activity): Activatom =
    new Activatom(this, a)
 
  override def toString: String =
    this match {
      case Non_Guard                        =>       "True"
      case Existential_Guard(event)         =>       event.toString
      case Negated_Existential_Guard(event) => "!" + event.toString
      case Negated_Guard(expr)              => "!(" + expr + ")"
      case Conjoined_Guard(lhs, rhs)        => "(" + lhs + " && " + rhs + ")"
      case Disjoined_Guard(lhs, rhs)        => "(" + lhs + " || " + rhs + ")"
    }
}


case object Non_Guard
    extends Guard

case class Existential_Guard(event: Event)
    extends Guard {
  override def unary_! = Negated_Guard(event)
}

case class Negated_Existential_Guard(event: Event)
    extends Guard

case class Conjoined_Guard(left: Guard, right: Guard)
    extends Guard

case class Disjoined_Guard(left: Guard, right: Guard)
    extends Guard

case class Negated_Guard(expr: Guard)
    extends Guard
