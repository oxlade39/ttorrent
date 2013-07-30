package com.oxlade39.github.storrent.announce

import akka.actor._
import java.net.{MalformedURLException, URI}
import java.io.IOException
import scala.io.Source
import akka.util.ByteString

class HttpAnnounceClient(trackerUri: URI)
  extends Actor
  with ActorLogging {

  import akka.actor.OneForOneStrategy
  import akka.actor.SupervisorStrategy._
  import scala.concurrent.duration._

  var listeners = List.empty[ActorRef]

  var child = context.watch(context.actorOf(HttpAnnounceClient.child(trackerUri)))

  override val supervisorStrategy =
    OneForOneStrategy(maxNrOfRetries = 10, withinTimeRange = 1.minute) {
      case _: MalformedURLException    ⇒ Restart
      case _: IOException              ⇒ Restart
      case _: Exception                ⇒ Escalate
    }

  def receive = {
    case HttpAnnounceClient.Register(actor) => listeners :+= actor
    case r: TrackerRequest => child.tell(r, sender)
    case Terminated(c) => child = context.watch(context.actorOf(HttpAnnounceClient.child(trackerUri)))
  }
}

object HttpAnnounceClient {
  def props(uri: URI): Props = Props(new HttpAnnounceClient(uri))

  private[HttpAnnounceClient] def child(trackerUri: URI): Props = Props(new Actor with ActorLogging {
    def receive = {
      case r: TrackerRequest => {
        val respondTo = sender
        val url = trackerUri.toURL
        val withParams = r.appendParams(url)
        val connection = withParams.openConnection
        val is = connection.getInputStream
        val stream = Source.fromInputStream(is)
        try {
          ByteString(stream.map(_.toByte).toArray) match {
            case norm : NormalTrackerResponse => respondTo ! norm
            case warn : WarningTrackerResponse => respondTo ! warn
            case fail : FailureTrackerResponse => respondTo ! fail
          }
        } finally {
          stream.close()
        }
      }
    }
  })

  case class Register(listener: ActorRef)

}
