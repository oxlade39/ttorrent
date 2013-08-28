package com.oxlade39.github.storrent.announce

import org.specs2.mutable.Specification
import java.net.URI

class TrackerHierarchySpec extends Specification {

  "TrackerHierarchy" should {
    "support iteration" in {
      val trackerOne = new URI("google.com")
      val trackerTwo = new URI("yahoo.com")

      val tierOne = TrackerTier(trackerOne :: Nil)
      val tierTwo = TrackerTier(trackerTwo :: Nil)
      val underTest = TrackerHierarchy(tierOne :: tierTwo :: Nil)

      underTest.current mustEqual trackerOne
      underTest.next.current mustEqual trackerTwo
    }

    "be circular" in {
      val trackerOne = new URI("google.com")
      val trackerTwo = new URI("yahoo.com")

      val tierOne = TrackerTier(trackerOne :: Nil)
      val tierTwo = TrackerTier(trackerTwo :: Nil)
      val underTest = TrackerHierarchy(tierOne :: tierTwo :: Nil)

      underTest.current mustEqual trackerOne
      underTest.next.current mustEqual trackerTwo
      underTest.next.next.current mustEqual trackerOne
    }
  }

  "TrackerTier" should {
    "shuffle" in {

      // presumably 1 in 100 chance of shuffle producing the same order and failing the test
      val uris = 1.to(100).map(i ⇒ new URI("google%s.com".format(i)))

      val tier = TrackerTier(uris.toList)

      tier.shuffle mustNotEqual tier
    }

    "store a current value as the head" in {
      val trackerOne = new URI("google.com")
      val trackerTwo = new URI("yahoo.com")

      val tier = TrackerTier(trackerOne :: trackerTwo :: Nil)
      tier.current mustEqual trackerOne
    }

    "be iterable" in {
      val trackerOne = new URI("google.com")
      val trackerTwo = new URI("yahoo.com")

      val tier = TrackerTier(trackerOne :: trackerTwo :: Nil)
      tier.nextTracker.current mustEqual trackerTwo
    }

    "reset back to original state" in {
      val trackerOne = new URI("google.com")
      val trackerTwo = new URI("yahoo.com")
      val trackerThree = new URI("altavista.com")

      val tier = TrackerTier(trackerOne :: trackerTwo :: trackerThree :: Nil)
      tier.nextTracker.reset mustEqual tier
    }
  }

}
