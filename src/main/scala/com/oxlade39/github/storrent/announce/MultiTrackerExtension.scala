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

  /**
   * <a href="http://www.bittorrent.org/beps/bep_0012.html">Order of Processing</a>
   * The tiers of announces will be processed sequentially; all URLs in each tier must be checked before the client
   * goes on to the next tier. URLs within each tier will be processed in a randomly chosen order; in other words,
   * the list will be shuffled when first read, and then parsed in order. In addition, if a connection with a
   * tracker is successful, it will be moved to the front of the tier
   *
   */
  var trackerTiers: TrackerHierarchy = TrackerHierarchy(announceUrls.map { trackerUrls =>
    TrackerTier(trackerUrls).shuffle
  })

  var currentTracker = newTracker

  def newTracker =
    context.watch(context.actorOf(HttpAnnounceClient.props(trackerTiers.current)))

  def receive = {
    case request: TrackerRequest => currentTracker.forward(request)
    case Terminated(client) => {
      log.info("client actor {} terminated so moving on to next client", client)
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
