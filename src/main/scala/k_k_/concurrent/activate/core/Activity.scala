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
package k_k_.concurrent.activate.core

import k_k_.concurrent.activate.loiter._


final class Activity(thunk: () => Unit) extends (() => Unit) {
  assert(!thunk.isInstanceOf[Activity]) // (expected never to happen)
  def apply() {
    thunk()
  }
}

object Activity {
  implicit def func2Activity[T](f: () => T): Activity = {
    // IMPORTANT: ascription to Unit satisfies Activity ctor param type exactly,
    // preventing an attempt to recursively call this very implicit (since
    // Activity extends the same type as its ctor param).
    new Activity(() => f(): Unit)
  }

  // NO implicit conversion from by-name 'expr', since found to mask errors:
  // implicit def expr2Activity[T](e: => T): Activity = new Activity(() => e)
}


object +> {
  def apply(a: Activity): Activatom =
    new Activatom(a)
}

// support conversion from by-name 'expr' only on request (by use of this obj.)
object +=> {
  def apply[T](e: => T): Activatom =
    new Activatom(new Activity(() => e))
}


object Activatom {
  implicit def Activity2Activatom(a: Activity): Activatom =
    new Activatom(a)

  /** a kind of temporal if/else for a Guard and two groups of Event`s */
  def upon(guard: Guard, true_events: List[Event], false_events: List[Event]):
      Activatom =
    new Activatom(guard, true_events, false_events, Nil)

  def upon(guard: Guard, true_event: Event, false_event: Event): Activatom =
    upon(guard, List(true_event), List(false_event))
}

/**
 * per `guard` determination, `activities` may be either 'activated' or
 * 'abandoned', at which point the associated events are affirmed.
 * terminology wise, while the `guard` is indeterminate, `activities` are said
 * to be 'inhibited'.
 */
class Activatom(
  val guard: Guard,
  val activate_events: List[Event],
  val abandon_events: List[Event],
  val activities: List[Activity]
  ) {

  def this(
    guard: Guard,
    activate_events: List[Event],
    abandon_events: List[Event],
    activities: Activity*
    ) =
    this(guard, activate_events, abandon_events, activities.toList)

  def this(guard: Guard, activities: Activity*) =
    this(guard, Nil, Nil, activities.toList)

  def this(activities: Activity*) =
    this(Null_Guard, Nil, Nil, activities.toList)


  def copy(
    guard: Guard                 = this.guard,
    activate_events: List[Event] = this.activate_events,
    abandon_events: List[Event]  = this.abandon_events,
    activities: List[Activity]   = this.activities
    ): Activatom =
    new Activatom(guard, activate_events, abandon_events, activities)


  def unary_!        = copy(!guard)
  def &&(rhs: Guard) = copy(guard && rhs)
  def ||(rhs: Guard) = copy(guard || rhs)
  def ^ (rhs: Guard) = copy(guard ^  rhs)


  def add_activates(activates: Event*) =
    copy(activate_events = (activates :\ activate_events) { _ :: _ })
  def add_abandons(abandons: Event*) =
    copy(abandon_events  = (abandons  :\ abandon_events)  { _ :: _ })

  def add_activities(acts: Activity*) =
    copy(activities      = (acts      :\ activities)      { _ :: _ })

  // aliases:
  def ++^ = add_activates  _
  def ++/ = add_abandons   _

  def ++& = add_activities _
}
