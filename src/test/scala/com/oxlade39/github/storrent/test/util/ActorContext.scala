package com.oxlade39.github.storrent.test.util

import org.specs2.specification.Scope
import org.specs2.mutable.After
import akka.actor.ActorSystem

/**
 * @author dan
 */
trait ActorContext extends Scope with After {
  val sys = ActorSystem("testHttpAnnounceClient")

  def after = sys.shutdown()
}
