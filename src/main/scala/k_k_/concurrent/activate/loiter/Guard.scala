/**
 * Activate Library for Scala - Concurrency-Oriented Programming in the
 * Activity Activation Style.
 *
 * Copyright (c)2009-2012 by Corbin "Kip" Kohn
 * All Rights Reserved.
 *
 *  Released under the terms of the Artistic License 2.0; see:
 *   http://www.opensource.org/licenses/artistic-license-2.0.php
 *
*/
package k_k_.concurrent.activate.loiter

import k_k_.concurrent.activate.core.{Event, Activity, Activatom}


object Guard {
  implicit def fromEvent(event: Event) = Existential_Guard(event)
}

sealed abstract class Guard {

  // boolean algebraic operators:
  def unary_!       : Guard = Negated_Guard(this)
  def &&(lhs: Guard): Guard = Conjoined_Guard(this, lhs)
  def ||(lhs: Guard): Guard = Disjoined_Guard(this, lhs)
  // 'exclusive OR'
  def ^ (lhs: Guard): Guard = Disjoined_Guard((this && !lhs), (!this && lhs))

  // combine with an `Activity` 
  def ?+>(a: Activity): Activatom = new Activatom(this, a)
  // combine with a by-name 'expression'
  def ?=>[T](e: => T):  Activatom = new Activatom(this, new Activity(() => e))

 
  override def toString: String =
    this match {
      case Null_Guard                       =>       "True"
      case Existential_Guard(event)         =>       event.toString
      case Negated_Existential_Guard(event) => "!" + event.toString
      case Negated_Guard(expr)              => "!(" + expr + ")"
      case Conjoined_Guard(lhs, rhs)        => "(" + lhs + " && " + rhs + ")"
      case Disjoined_Guard(lhs, rhs)        => "(" + lhs + " || " + rhs + ")"
    }
}


case object Null_Guard
    extends Guard

case class Existential_Guard(event: Event)
    extends Guard {
  override def unary_! : Guard = Negated_Existential_Guard(event)
}

case class Negated_Existential_Guard(event: Event)
    extends Guard {
  override def unary_! : Guard = Existential_Guard(event)
}

case class Conjoined_Guard(left: Guard, right: Guard)
    extends Guard

case class Disjoined_Guard(left: Guard, right: Guard)
    extends Guard

case class Negated_Guard(expr: Guard)
    extends Guard
