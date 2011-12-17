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
package k_k_.concurrent.activate.core

import k_k_.concurrent.activate.loiter._


//!!!!!parse error!!!!!!
// error: identifier expected but ')' found.
// abstract class Activity extends () => Unit
//                                  ^
abstract class Activity extends (() => Unit)
// {
//  def unary_~ : Activatom = new Activatom(this)
//}


object Activity {
  implicit def func2activity(f: () => Unit): Activity =
    new Activity {
      def apply() = f()
    }

  implicit def expr2activity(f: => Unit): Activity =
    new Activity {
      def apply() = f
    }
}

object +> {
  def apply(f: => Unit): Activatom =
    new Activatom(Null_Guard,
                  new Activity {
                    def apply() = f
                  }
                )
}

object ~> {
  def apply(f: () => Unit): Activatom =
    new Activatom(Null_Guard,
                  new Activity {
                    def apply() = f()
                  }
                )
}

/*???
object Activatom {
  implicit def activity2activatom(a: Activity): Activatom =
    new Activatom(Null_Guard, a)

  implicit def expr2activatom(f: => Unit): Activatom =
    new Activatom(Null_Guard, new Activity {
      def apply() = f
    }
                )
}
*/

class Activatom(
  g: Guard,
  act_evts: List[Event],
  drop_evts: List[Event],
  activities0: List[Activity]
  ) {
  import Activity._

  def this(g: Guard, act_evts: List[Event], drop_evts: List[Event],
           activity: Activity) =
    this(g, act_evts, drop_evts, List(activity))

  def this(g: Guard, activity: Activity) =
    this(g, Nil, Nil, List(activity))

//  def this(activity: Activity) =
//    this(Null_Guard, Nil, Nil, List(activity))

  //??????????
  // error: double definition:
  // ...have same type after erasure: (k_k_.concurrent.activate.loiter.guard.Guard,List,List,List)
  // def this(g: Guard, act_evts: List[Event], drop_evts: List[Event],
  //     ^
  //          activities: List[() => Unit]) =
  //   this(g, act_evts, drop_evts, activities map {Activity.func2activity(_)})

  def this(g: Guard, act_evts: List[Event], drop_evts: List[Event],
           activity: () => Unit) =
    this(g, act_evts, drop_evts, List[Activity](activity))

  def guard: Guard =
    g
  def activate_events: List[Event] =
    act_evts
  def drop_events: List[Event] =
    drop_evts
  def activities: List[Activity] =
    activities0

  def unary_! =
    new Activatom(!g, act_evts, drop_evts, activities0)
  def &&(lhs: Guard) =
    new Activatom((g && lhs), act_evts, drop_evts, activities0)
  def ||(lhs: Guard) =
    new Activatom((g || lhs), act_evts, drop_evts, activities0)
  def ^ (lhs: Guard) =
    new Activatom((g ^  lhs), act_evts, drop_evts, activities0)

  def add_activates(acts: List[Event]) =
    new Activatom(g, acts ::: act_evts, drop_evts, activities0)
  def add_drops(drops: List[Event]) =
    new Activatom(g, act_evts, drops ::: drop_evts, activities0)

  // def ++^(acts: List[Event]) = add_activates(acts)
  def ++^ = add_activates _

  // def ++/(drops: List[Event]) = add_drops(drops)
  def ++/ = add_drops _

  def add_activities(activities_0: List[Activity]) =
    new Activatom(g, act_evts, drop_evts, activities_0 ::: activities0)
  def add_activities(activity: Activity) =
    new Activatom(g, act_evts, drop_evts, activity :: activities0)
/*
  def add_activities(activity: () => Unit) =
    new Activatom(g, act_evts, drop_evts, activity :: activities0)

  def ++&(activity: () => Unit)         = add_activities (activity: Activity)
*/

  def ++&(activities_0: List[Activity]) = add_activities (activities_0:
                                                            List[Activity])
  def ++&(activity: Activity)           = add_activities (activity: Activity)

  //!!!!!!!should redefine all overloaded defs!!!!!!!!
  // error: ambiguous reference to overloaded definition,
  // ...both method ... and ... match expected type ?
  // def ++& = add_activities _
  //          ^
}
