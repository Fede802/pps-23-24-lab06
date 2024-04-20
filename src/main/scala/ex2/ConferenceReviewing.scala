package ex2

import ex2.Question.{CONFIDENCE, FINAL, RELEVANCE, SIGNIFICANCE}

import scala.+:

enum Question:
  case RELEVANCE, SIGNIFICANCE, CONFIDENCE, FINAL

trait ConferenceReviewing:
  def loadReview(article: Int, scores: Map[Question,Int]): Unit
  def loadReview(article: Int, relevance: Int, significance: Int, confidence: Int, fin: Int): Unit
  def orderedScores(article: Int, question: Question): List[Int]
  def averageFinalScore(article: Int): Double
  def acceptedArticles(): Set[Int]
  def sortedAcceptedArticles(): List[(Int, Double)]
  def averageWeightedFinalScoreMap(): Map[Int, Double]

object ConferenceReviewing:
  def apply(): ConferenceReviewing = ConferenceReviewingImpl()
  private case class ConferenceReviewingImpl() extends ConferenceReviewing:
    private var articles: List[(Int, Map[Question, Int])] = List()
    override def loadReview(article: Int, scores: Map[Question, Int]): Unit =
      articles ::= (article, scores)
    override def loadReview(article: Int, relevance: Int, significance: Int, confidence: Int, fin: Int): Unit =
      loadReview(article, Map(RELEVANCE -> relevance, SIGNIFICANCE -> significance, CONFIDENCE -> confidence, FINAL -> fin))
    override def orderedScores(article: Int, question: Question): List[Int] =
      scores(article,question).sorted
    override def averageFinalScore(article: Int): Double =
      averageScore(article, FINAL)
    private def averageScore(article: Int, question: Question): Double =
      average(articles.filter(_._1 == article).map(_._2(question)))
    private def average(l: List[Double]): Double =
      l.sum / l.length
    private def scores(article: Int, question: Question): List[Int] =
      articles.filter(_._1 == article).map(_._2(question))
    override def acceptedArticles(): Set[Int] =
      articles.map(_._1).toSet.filter(accepted)
    override def sortedAcceptedArticles(): List[(Int, Double)] =
      articles.map(_._1).distinct.collect({case a if accepted(a) => (a,averageFinalScore(a))}).sortBy(_._2)
    private def accepted(a: Int): Boolean =
      averageFinalScore(a) > 5 && scores(a, RELEVANCE).exists(_ >= 8)
    override def averageWeightedFinalScoreMap(): Map[Int, Double] =
      articles.map(_._1).distinct.map(a => (a, average(articles.filter(_._1 == a).map((i,q) => q(CONFIDENCE)*q(FINAL)/10.0)))).toMap

