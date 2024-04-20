package ex2

import org.scalatest.matchers.should.Matchers.*

class ConferenceReviewingTest extends org.scalatest.funsuite.AnyFunSuite:

  private val cr = ConferenceReviewing()
  cr.loadReview(1, 8, 8, 6, 8)
  cr.loadReview(1, 9, 9, 6, 9)
  cr.loadReview(2, 9, 9, 10, 9)
  cr.loadReview(2, 4, 6, 10, 6)
  cr.loadReview(3, 3, 3, 3, 3)
  cr.loadReview(3, 4, 4, 4, 4)
  cr.loadReview(4, 6, 6, 6, 6)
  cr.loadReview(4, 7, 7, 8, 7)
  private val map = Map(Question.RELEVANCE -> 8, Question.SIGNIFICANCE -> 8, Question.CONFIDENCE -> 7, Question.FINAL -> 8)
  cr.loadReview(4, map)
  cr.loadReview(5, 6, 6, 6, 10)
  cr.loadReview(5, 7, 7, 7, 10)


  test("Ordered scores"):
    cr.orderedScores(2, Question.RELEVANCE) shouldBe List(4, 9)
    cr.orderedScores(4, Question.CONFIDENCE) shouldBe List(6, 7, 8)
    cr.orderedScores(5, Question.FINAL) shouldBe List(10, 10)

  test("Average final score"):
    cr.averageFinalScore(1) shouldBe 8.5
    cr.averageFinalScore(2) shouldBe 7.5
    cr.averageFinalScore(3) shouldBe 3.5
    cr.averageFinalScore(4) shouldBe 7.0
    cr.averageFinalScore(5) shouldBe 10.0

  test("Accepted articles"):
    cr.acceptedArticles() shouldBe Set(1, 2, 4)

  test("Sorted accepted articles"):
    cr.sortedAcceptedArticles() shouldBe List((4, 7.0), (2, 7.5), (1, 8.5))

  test("Average weighted final score"):
    cr.averageWeightedFinalScoreMap().getOrElse(1, -1.0) shouldBe (4.8 + 5.4) / 2
    cr.averageWeightedFinalScoreMap().getOrElse(2, -1.0) shouldBe (9.0 + 6.0) / 2
    cr.averageWeightedFinalScoreMap().getOrElse(3, -1.0) shouldBe (0.9 + 1.6) / 2
    cr.averageWeightedFinalScoreMap().getOrElse(4, -1.0) shouldBe (3.6 + 5.6 + 5.6) / 3
    cr.averageWeightedFinalScoreMap().getOrElse(5, -1.0) shouldBe (6.0 + 7.0) / 2
    cr.averageWeightedFinalScoreMap().size shouldBe 5


