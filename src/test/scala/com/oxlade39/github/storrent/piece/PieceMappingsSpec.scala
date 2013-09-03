package com.oxlade39.github.storrent.piece

import org.specs2.mutable.Specification
import org.specs2.mock.Mockito
import akka.actor._
import akka.testkit.TestProbe

/**
 * @author dan
 */
class PieceMappingsSpec extends Specification with Mockito {
  "PieceMapping" should {
    "give me the rarest pieces" in {

      val system = ActorSystem("whocares")
      def probe() = TestProbe.apply()(system).ref


      val (actorOne, actorTwo, actorThree) = (probe(),probe(),probe())

      val actorOnePieces = Seq(0, 1, 2, 3, 4)
      val actorTwoPieces = Seq(0)
      val actorThreePieces = Seq(0, 2, 3)

      def add(actor: ActorRef): (PieceMappings, Int) ⇒ PieceMappings = {
        (m, i) ⇒ m + (actor -> i)
      }

      val withActorOnePieces = actorOnePieces.foldLeft(PieceMappings())(add(actorOne))
      val withActorOneAndTwoPieces = actorTwoPieces.foldLeft(withActorOnePieces)(add(actorTwo))
      val withActorOneTwoAndThreePieces = actorThreePieces.foldLeft(withActorOneAndTwoPieces)(add(actorThree))

      withActorOneTwoAndThreePieces.rarestPieces mustEqual List(1, 4, 2, 3, 0)

    }

    "map actors to pieces" in {
      val system = ActorSystem("whocares")
      def probe() = TestProbe.apply()(system).ref


      val (actorOne, actorTwo, actorThree) = (probe(),probe(),probe())

      val actorOnePieces = Seq(0, 1, 2, 3, 4)
      val actorTwoPieces = Seq(0)
      val actorThreePieces = Seq(0, 2, 3)

      def add(actor: ActorRef): (PieceMappings, Int) ⇒ PieceMappings = {
        (m, i) ⇒ m + (actor -> i)
      }

      val withActorOnePieces = actorOnePieces.foldLeft(PieceMappings())(add(actorOne))
      val withActorOneAndTwoPieces = actorTwoPieces.foldLeft(withActorOnePieces)(add(actorTwo))
      val withActorOneTwoAndThreePieces = actorThreePieces.foldLeft(withActorOneAndTwoPieces)(add(actorThree))

      withActorOneTwoAndThreePieces.actorMapping mustEqual Map(
        actorOne -> Set(actorOnePieces:_*),
        actorTwo -> Set(actorTwoPieces:_*),
        actorThree -> Set(actorThreePieces:_*)
      )
    }
  }

}
