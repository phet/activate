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
package k_k_.concurrent.activate.core.eval

import k_k_.concurrent.activate.loiter.eval.Event_Observer
import k_k_.concurrent.activate.loiter.eval.Transaction


sealed abstract class Event_Exposition

case class Event_Expectation(
  observers: List[Event_Observer]
  ) extends Event_Exposition {

  def this(observer: Event_Observer) =
    this(List(observer))

  def add_observer(observer: Event_Observer) =
    new Event_Expectation(observer :: observers)

  def fulfill(tx: Transaction) {
    // announce existence in reverse order (first, to those waiting longest)
    observers.reverse.map { _.exists(tx) }
  }
}

case class Event_Confirmation(
  tx: Transaction
  ) extends Event_Exposition
