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
package k_k_.concurrent.activate.loiter.eval

import k_k_.concurrent.activate.core.Event
import k_k_.concurrent.activate.util.Reverse_Foreach

import scala.collection.mutable.Queue


trait Pending {
  def release(tx: Transaction)
}


object Transaction {
  type Id = Long

  val NIL_ID: Id = 0;
  val INITIALIZATION_ID: Id = 1;
}

abstract class Transaction(
  private val id: Transaction.Id,
  private val parent: Option[Transaction]
  ) extends Iterator[Event] with Reverse_Foreach {

  private var queue = new Queue[Event]
  private var pendings: List[Pending] = Nil
  @volatile private var is_complete = false

  protected def this(id: Transaction.Id) {
    this(id, None)
  }

  def completed_? : Boolean =
    is_complete

  def add_events(events: List[Event]) = synchronized {
    if (!completed_?) {
      queue ++= events
    } else {
      handle_add_events_after_complete(events)
    }
  }

  def add_pending(pending: Pending) = synchronized {
    if (!completed_?) {
      pendings = pending :: pendings
    } else {
      handle_add_pending_after_complete(pending)
    }
  }

  def complete: Boolean = synchronized {
    if (!completed_? && hasNext()) {
      false
    } else {
      is_complete = true
      true
    }
  }

  def hasNext(): Boolean = synchronized {
    !queue.isEmpty
  }

  def next(): Event = synchronized {
    queue.dequeue
  }

  override
  def equals(other: Any): Boolean =
    other match {
      case that: Transaction => this.id == that.id
      case _ => false
    }

  override
  def hashCode: Int =
    id.hashCode

  def <(other: Transaction): Boolean =
    id < other.id

  // is `this` an ancestor of other?
  def ancestor_of_?(other: Transaction): Boolean = {
    id == other.id ||
    (other.parent match {
      case Some(p) => p.ancestor_of_?(other)
      case None => false
    })
  }

  def close {
    if (!pendings.isEmpty) {
      val shadow = create_shadow
      // release in reverse order (first, to those waiting longest)
      reverse_foreach((p: Pending) => p.release(this))(pendings)
      shadow.execute(shadow)
    }
  }

  override
  def toString: String = {
    def count_parents(tx: Transaction, num: Int): String = {
      parent match {
        case None => fmt(num)
        case Some(parent) => count_parents(parent, num + 1)
      }
    }
    def fmt(n_parents: Int): String = {
      ("Transaction(" +
       (if (n_parents == 0) id.toString else (id.toString + '*' + n_parents)) +
       ")")
    }
    count_parents(this, 0)
  }

  protected def create_shadow: Transaction = {
    new Transaction(id, Some(this)) {
      // (shadow.execute`s in the same manner as this)
      protected val execute = Transaction.this.execute

      protected def handle_add_events_after_complete(events: List[Event]) {
        Transaction.this.handle_add_events_after_complete(events)
      }

      protected def handle_add_pending_after_complete(pending: Pending) {
        Transaction.this.handle_add_pending_after_complete(pending)
      }
    }
  }

  protected val execute: Transaction => Unit

  protected def handle_add_events_after_complete(events: List[Event])

  protected def handle_add_pending_after_complete(pending: Pending)
}


object Nil_Transaction
    extends Transaction(Transaction.NIL_ID) {

  protected val execute = (tx: Transaction) => { }

  protected def handle_add_events_after_complete(events: List[Event]) { }

  protected def handle_add_pending_after_complete(pending: Pending) { }
}


import java.util.concurrent.atomic.AtomicLong

abstract class Transaction_Sequencer {
  def next: Transaction

  protected def next_id: Transaction.Id =
    id_gen.getAndIncrement()

  private var id_gen = new AtomicLong(Transaction.INITIALIZATION_ID);
}
