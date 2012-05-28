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
package k_k_.concurrent.activate.loiter.eval

import k_k_.concurrent.activate.core.Event

import k_k_.concurrent.activate.util.opt_iterate

import scala.collection.mutable


/**
 * When an Activatable is only tentatively true, its activation must be deferred
 * until the transaction completes at which time it may be fulfilled (activated)
 */
trait Deferred {
  def fulfill(tx: Transaction)
}


object Transaction {
  type Id = Long

  def create_id_iterator: Iterator[Id] = {
    import java.util.concurrent.atomic.AtomicLong

    val next_id = new AtomicLong(Transaction.INITIALIZATION_ID)

    //!!!!TODO: fix potential for overflow!!!!
    Iterator.continually { next_id.getAndIncrement() }
  }

  private[eval] val NIL_ID: Id = 0L
  private val INITIALIZATION_ID: Id = 1L
}

abstract class Transaction(
  private val id: Transaction.Id,
  private val parent: Option[Transaction] = None
  ) {

  private val eventQueue = mutable.Queue.empty[Event]
  private val deferrals = mutable.ArrayBuffer.empty[Deferred]
  // check-and-set var must be protected by synchronization!
  private[this] var is_complete = false


  final def add_events(events: List[Event]): Unit = synchronized {
    if (!is_complete) {
      eventQueue ++= events
    } else {
      handle_add_events_after_complete(events)
    }
  }

  final def add_deferred(deferred: Deferred): Unit = synchronized {
    if (!is_complete) {
      deferrals += deferred
    } else {
      handle_add_deferred_after_complete(deferred)
    }
  }

  //???should this really perform a side-effect (which changes semantics of add_events, add_deferred)???
  final def has_more_events_or_else_complete(): Boolean = synchronized {
    if (is_complete) false
    else if (!eventQueue.isEmpty) true
    else {
      is_complete = true
      false
    }
  }

  final def take_event(): Event = synchronized {
    eventQueue.dequeue
  }

  override def equals(other: Any): Boolean =
    other match {
      case that: Transaction => (that can_equal this) && this.id == that.id
      case _ => false
    }

  override def hashCode: Int =
    id.hashCode

  protected def can_equal(other: Any): Boolean =
    other.isInstanceOf[Transaction]

  /** is `this` 'older than' `other`? */
  final def <(other: Transaction): Boolean =
    id < other.id

  /** is `this` an ancestor of `other`? */
  final def is_ancestor_of(other: Transaction): Boolean =
    opt_iterate(other)( _.parent ).exists { id == _.id }

  final def fulfill_deferred() = synchronized {
    if (!is_complete)
      sys.error("fulfill_deferred() on incomplete [%s]".format(this))
    if (!deferrals.isEmpty) {
      deferrals.foreach { _.fulfill(this) } // fulfill in order of added (FIFO)
      //???remove all afterward??? deferrals.clear() // defensive clear
    }
  }

  override def toString: String = {
    val n_parents = opt_iterate(this)( _.parent ).size - 1 // -1 for `this`
    "Transaction(%s%s)".format(
        id.toString,
        if (n_parents == 0) "" else (" * " + n_parents))
  }

  protected final def create_shadow(): Transaction =
    // (resulting shadow shall perform otherwise identically to `this`)
    new Transaction(id, Some(this)) {
      protected val execute = Transaction.this.execute

      protected def handle_add_events_after_complete(events: List[Event]) {
        Transaction.this.handle_add_events_after_complete(events)
      }

      protected def handle_add_deferred_after_complete(deferred: Deferred) {
        Transaction.this.handle_add_deferred_after_complete(deferred)
      }
    }


  protected val execute: Transaction => Unit

  protected def handle_add_events_after_complete(events: List[Event])

  protected def handle_add_deferred_after_complete(deferred: Deferred)
}


/** Intended as a sentinel value: not for actual use */
object Nil_Transaction extends Transaction(Transaction.NIL_ID) {

  protected val execute = (tx: Transaction) => { }

  protected def handle_add_events_after_complete(events: List[Event]) { }

  protected def handle_add_deferred_after_complete(deferred: Deferred) { }
}
