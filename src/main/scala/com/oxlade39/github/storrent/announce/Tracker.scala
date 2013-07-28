package com.oxlade39.github.storrent.announce

import akka.actor.{ActorLogging, Actor}

trait TrackerClient

class HttpTrackerClient
  extends Actor
  with ActorLogging
  with TrackerClient {

  def receive = {
    case _ => ""
  }
}