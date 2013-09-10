package com.oxlade39.github.storrent.announce

import akka.actor.{Props, ActorRef, Actor, ActorLogging}
import java.net.URI
import com.oxlade39.github.storrent.peer.PeerTracking

object Announcer {
  def props(peerTracking: ActorRef) = Props(new Announcer(peerTracking))

  sealed trait Message
  case class StartAnnouncing(req: TrackerRequest, trackers: List[List[URI]]) extends Message
  case object StopAnnouncing extends Message
  case object Finished extends Message
}

class Announcer(peerTracking: ActorRef) extends Actor with ActorLogging {

  import scala.concurrent.duration._
  import context._

  def receive = {
    case Announcer.StartAnnouncing(request, trackers) ⇒ {
      val child: ActorRef = context.actorOf(MultiTrackerExtension.props(trackers))
      child ! request
      context.become(announcing(child, request))
    }
  }

  def announcing(multiTrackerExtension: ActorRef, request: TrackerRequest): Receive = {
    case response: NormalTrackerResponse ⇒ {
      log.debug("response {}", response)

      peerTracking ! PeerTracking.PeersAnnounced(response.peers)

      log.debug("scheduling another request in {} seconds", response.clientRequestInterval)
      context.system.scheduler.scheduleOnce(response.clientRequestInterval seconds,
                                            multiTrackerExtension,
                                            request.copy(trackerId = response.trackerId,
                                                         event = None))
    }

    case Announcer.StopAnnouncing ⇒ {
      multiTrackerExtension ! request.copy(event = Some(Stopped))
      log.info("stopping {} due to stop request", self)
      context.stop(self)
    }

    case Announcer.Finished ⇒ {
      multiTrackerExtension ! request.copy(event = Some(Completed))
      log.info("stopping {} due to being finished", self)
      context.stop(self)
    }
  }
}
