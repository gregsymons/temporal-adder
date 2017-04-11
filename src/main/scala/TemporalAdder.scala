/**
 * 
 * © Copyright 2017 Greg Symons <gsymons@gsconsulting.biz>.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not
 * use this file except in compliance with the License. You may obtain a copy of
 * the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 * License for the specific language governing permissions and limitations under
 * the License.
 *
 */

package biz.gsconsulting.add_minutes.scaladsl

import scala.util.parsing.combinator._

/**
 * Some constants that I seem to be using everywhere.
 */
trait TimeConstants {
  val HOURS_PER_DAY = 24
  val MERIDIAN = HOURS_PER_DAY / 2
  val MINUTES_PER_HOUR = 60
  val MINUTES_PER_DAY = HOURS_PER_DAY * MINUTES_PER_HOUR
}

/**
 * A Meridian is a signifier for which half of the day we're in on a 12-hour
 * clock: AM or PM
 */
sealed trait Meridian extends TimeConstants
final case object AM extends Meridian {
  override def toString: String = "AM"
}

final case object PM extends Meridian {
  override def toString: String = "PM"
}

final object Meridian extends Meridian {
  /**
   * Return the Meridian for a given string
   */
  def of(meridian: String) = {
    meridian match {
      case "AM"            => AM
      case "PM"            => PM
      case invalid: String => scala.sys.error("Invalid meridian: [$invalid]")
    }
  }

  /**
   * Return the Meridian for a given Instant
   */
  def of(instant: Instant) = {
    if (instant.hours >= MERIDIAN) PM
    else AM
  }
}

final case class Instant(minutes: Long) extends TimeConstants {
  /**
   * The hours portion of the instant on a 24-hour clock (i.e. 0-23)
   */
  lazy val hours = (minutes / MINUTES_PER_HOUR) % HOURS_PER_DAY

  /**
   * The hour portion of the instant on a 12-hour clock (i.e. 1-12)
   */
  lazy val hour = {
    if (hours == 0 || hours == MERIDIAN) MERIDIAN
    else hours % MERIDIAN
  }

  /**
   * The minute portion of the instant
   */
  lazy val minute = minutes % MINUTES_PER_HOUR 

  /**
   * The meridian of the instant (i.e. AM or PM)
   */
  lazy val meridian = Meridian.of(this)

  /**
   * Add m minutes to the instant and return a new instant
   */
  def addMinutes(m: Long): Instant = {
    if (m < 0) {
      val lessMinutes = (m % MINUTES_PER_HOUR)
      val lessHoursInMinutes = ((m / MINUTES_PER_HOUR) % HOURS_PER_DAY) * MINUTES_PER_HOUR
      val totalLessMinutes = lessMinutes + lessHoursInMinutes

      // Check for overflow, but the max overflow is 23 hours, 59 minutes because of
      // how we calculated totalLessMinutes above, so if we add a day, the modular
      // arithmetic on the accessors will give us the correct results. I suppose
      // in theory, we could make sure that the final minutes value is in the correct
      // range for a single day, but since these Instants don't _really_ have a base
      // epoch (or a time zone), it doesn't actually make a difference. Also, we'd have
      // to account for it on the other side of the branch, too
      if (totalLessMinutes.abs <= minutes) Instant(minutes + totalLessMinutes)
      else Instant(minutes + totalLessMinutes + MINUTES_PER_DAY)
    }
    //Positive modular math is easy...
    else Instant(minutes + m)
  }

  /**
   * Format the instant as "[H]H:mm AM|PM"
   */
  override def toString = f"${hour}%d:${minute}%02d ${meridian}"
}

/**
 * A proper parser for parsing a time in the form "[H]H:mm AM|PM"
 *
 * Sure, I could've just used a single regex to parse this format, but
 * using a parser combinator lets me write nicer error messages that 
 * show you what specifically was wrong.
 */
final object ParseTime extends RegexParsers 
  with TimeConstants
{
  def hour    : Parser[Long]      = """(1[0-2]|[1-9])""".r   ^^ { _.toLong }
  def minutes : Parser[Long]      = """[0-5]\d""".r ^^ { _.toLong }
  // I suppose I could be more permissive by making the regex case-insensitive, but
  // hey, it wasn't in the spec.
  def meridian: Parser[Meridian]  = """AM|PM""".r   ^^ { (m: String) => Meridian.of(m) }
  def sep     : Parser[String]    = ":"
  def time    : Parser[Instant]   = hour ~ sep ~ minutes ~ whiteSpace ~ meridian ^^ { 
    case h ~ _ ~ m ~ _ ~ AM if h == MERIDIAN => Instant(m)
    case h ~ _ ~ m ~ _ ~ PM if h == MERIDIAN => Instant(h * MINUTES_PER_HOUR + m)
    case h ~ _ ~ m ~ _ ~ AM => Instant(h * MINUTES_PER_HOUR + m)
    case h ~ _ ~ m ~ _ ~ PM => Instant((h + MERIDIAN) * MINUTES_PER_HOUR + m )
  }

  final case class TimeParseFailure(msg: String, cause: Throwable = null) extends RuntimeException(msg, cause)

  override val skipWhitespace = false

  def parseException[T](msg: String, input: Input): T = {
    throw TimeParseFailure(s"""
        |  Unable to parse time: [${input.source}]!
        |
        |  $msg:
        |
        |  ${input.pos.longString}
      """.stripMargin)
  }

  def apply(input: String): Instant = {
    if (input.isEmpty) throw TimeParseFailure("Input string was empty!")

    parseAll(time, input) match {
      case Success(result, _) => result
      case NoSuccess(msg, next) => parseException(msg, next)
    }
  }
}

final class TemporalAdder {
  def addMinutes(time: String, minutes: Int): String = {
    ParseTime(time).addMinutes(minutes).toString
  }
}
