package o1.football3

import scala.collection.mutable.Buffer

class Season:
  private val matches = Buffer[Match]()
  private var highest: Option[Match] = None

  def addResult(newResult: Match) =
    matches += newResult
    this.highest match
      case None =>
        this.highest = Some(newResult)
      case Some(oldResult) =>
        if math.abs(newResult.goalDifference) > math.abs(oldResult.goalDifference) then
          this.highest = Some(newResult)
        else
          this.highest = Some(oldResult)

  def biggestWin =
    if numberOfMatches == 0 then
      None
    else
      this.highest

  def latestMatch: Option[Match] =
    if numberOfMatches == 0 then
      None
    else
      Some(matches.last)

  def matchNumber(number: Int): Option[Match] =
    matches.lift(number)

  def numberOfMatches: Int =
    matches.size
