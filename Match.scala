package o1.football3

import scala.collection.mutable.Buffer

/** The class `Match` represents match results in a football match statistics program.
  * A match is played between teams from two clubs: a home club and an away club.
  * Goals scored by players of either team can be added to the match object with the
  * method `addGoal`.
  *
  * The class is expected to be used so that a match object with no goals is initially
  * created as a real-life match starts. Goals are added incrementally as the match
  * progresses. (A match object has mutable state.)
  *
  * @param home  the club whose team plays at home in the match
  * @param away  the club whose team plays away in the match */
class Match(val home: Club, val away: Club):

  private val homeScorers = Buffer[Player]()    // container: goalscorers of the home team are added here
  private val awayScorers = Buffer[Player]()    // container: goalscorers of the away team are added here


  def winnerName =
    if this.goalDifference < 0 then
      this.away.name
    else if this.goalDifference < 0 then
      this.away.name
    else
      "no winner"

  def winner: Option[Club] =
    if this.goalDifference < 0 then
      Some(this.away)
    else if this.goalDifference > 0 then
      Some(this.home)
    else None


  def winningScorer =
    if isHomeWin then
      Some(homeScorers(awayGoals))
    else if isAwayWin then
      Some(awayScorers(homeGoals))
    else None


  def addGoal(scorer: Player): Unit =
    if scorer.employer == this.home then
      this.homeScorers += scorer
    else if scorer.employer == this.away then
      this.awayScorers += scorer
    // not done ??

  def awayGoals = awayScorers.size
  def homeGoals = homeScorers.size
  def totalGoals = homeGoals + awayGoals

  def goalDifference = homeGoals - awayGoals
  def isHomeWin = this.homeGoals > this.awayGoals
  def isAwayWin = this.homeGoals < this.awayGoals
  def isTied = this.homeGoals == this.awayGoals
  def isGoalless = this.totalGoals == 0
  def isHigherScoringThan(anotherMatch: Match) =
    this.totalGoals > anotherMatch.totalGoals

  def allScorers = homeScorers.toVector ++ awayScorers.toVector
  def location = home.stadium

  def hasScorer(scorer: Player) = allScorers.contains(scorer)

  override def toString: String =
    if this.isGoalless then
      home.name + " vs. " + away.name + " at " + location +": tied at nil-nil"
    else if this.isTied then
      home.name + " vs. " + away.name + " at " + location +": tied at " + homeGoals + "-all"
    else if isHomeWin then
      home.name + " vs. " + away.name + " at " + location +": " + homeGoals + "-" + awayGoals + " to " + home.name
    else
      home.name + " vs. " + away.name + " at " + location +": " + awayGoals + "-" + homeGoals + " to " + away.name


end Match

