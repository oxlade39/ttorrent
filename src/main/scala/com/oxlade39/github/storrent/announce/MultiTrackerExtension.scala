package com.oxlade39.github.storrent.announce

import akka.actor._
import java.net.URI

/**
 * Multitracker Metadata Extension BEP 12
 * Add support for multiple trackers
 *
 * @see http://www.bittorrent.org/beps/bep_0012.html
 */
object MultiTrackerExtension {
  def props(announceUrls: List[List[URI]]) = Props(new MultiTrackerExtension(announceUrls))
}

class MultiTrackerExtension(announceUrls: List[List[URI]])
  extends Actor
  with ActorLogging {

  var trackerTiers: TrackerHierarchy = TrackerHierarchy(announceUrls.map { trackerUrls =>
    TrackerTier(trackerUrls).shuffle
  })

  var currentTracker = newTracker

  def newTracker =
    context.watch(context.actorOf(HttpAnnounceClient.props(trackerTiers.current)))

  def receive = {
    case request: TrackerRequest => currentTracker.forward(request)
    case Terminated(client) => {
      trackerTiers = trackerTiers.next
      currentTracker = newTracker
    }
  }
}

case class TrackerHierarchy(tiers: List[TrackerTier]) {
  assert(!tiers.isEmpty)

  def current = tiers.head.current

  def next: TrackerHierarchy = tiers match {
    case head :: rest if head.trackers.size > 1 => copy(head.nextTracker :: rest)
    /**
     * this is O(n) due to moving head to the tail
     * TODO improve me
     */
    case head :: rest => copy(rest :+ head.reset)
  }
}

case class TrackerTier(trackers: List[URI], failed: List[URI] = Nil) {
  def shuffle = copy(util.Random.shuffle(trackers))

  def current = trackers.head
  def nextTracker = copy(trackers.tail, trackers.head :: failed)
  def reset = TrackerTier(failed ++ trackers)
}
