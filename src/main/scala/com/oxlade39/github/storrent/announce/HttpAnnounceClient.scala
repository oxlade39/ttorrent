package com.oxlade39.github.storrent.announce

import akka.actor._
import java.net.{MalformedURLException, URI}
import java.io.IOException
import akka.util.ByteString
import org.apache.commons.io.output.ByteArrayOutputStream

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
    case HttpAnnounceClient.Register(actor) ⇒ listeners :+= actor
    case r: TrackerRequest ⇒ {
      val replyTo = sender
      child ! HttpAnnounceClient.ChildRequest(r, replyTo)
    }
    case HttpAnnounceClient.ChildResponse(response, originalSender) ⇒ {
      response match {
        case normal: NormalTrackerResponse ⇒ originalSender ! normal
        case fail: FailureTrackerResponse ⇒ {
          log.info("{} so stopping {}", fail, self)
          context.stop(self)
        }
      }
    }
    case Terminated(c) ⇒ child = context.watch(context.actorOf(HttpAnnounceClient.child(trackerUri)))
  }
}

object HttpAnnounceClient {
  def props(uri: URI): Props = Props(new HttpAnnounceClient(uri))

  private[HttpAnnounceClient] def child(trackerUri: URI): Props = Props(new Actor with ActorLogging {
    def receive = {
      case ChildRequest(originalRequest, originalSender) ⇒ {
        val respondTo = sender
        val url = trackerUri.toURL
        val withParams = originalRequest.appendParams(url)
        log.info("making request to {}", withParams)
        val connection = withParams.openConnection
        val is = connection.getInputStream
        try {
          val baos: ByteArrayOutputStream = new ByteArrayOutputStream
          baos.write(is)
          val bs = ByteString(baos.toByteArray)

          val responseOption = NormalTrackerResponse.parse(bs).orElse(FailureTrackerResponse.parse(bs))

          log.debug("tracker responded with {}", responseOption)

          responseOption map (response ⇒ respondTo ! ChildResponse(response, originalSender))
          
        } finally {
          is.close()
        }
      }
    }
  })

  case class Register(listener: ActorRef)
  case class ChildRequest(request: TrackerRequest, originalSender: ActorRef)
  case class ChildResponse(request: TrackerResponse, originalSender: ActorRef)

}
