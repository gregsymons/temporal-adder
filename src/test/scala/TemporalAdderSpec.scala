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

import org.scalatest._
import org.scalatest.prop._
import org.scalacheck._

import java.time._
import java.time.format._

import biz.gsconsulting.add_minutes.scaladsl._

// Wrap our time strings to prevent scalacheck from doing silly shrinks
case class TimeString(time: String)

class AdderSpec extends FunSpec
  with Matchers 
  with PropertyChecks {

  describe("An Adder") {
    val adder = new TemporalAdder
    val formatter = format.DateTimeFormatter.ofPattern("h:mm a")

    describe("legal values") {
      val LegalHours     = Gen.choose(1,12)
      val LegalMinutes   = Gen.choose(0,59)
      val LegalMeridians = Gen.oneOf("AM", "PM")
      val Addends        = Gen.choose(Integer.MIN_VALUE, Integer.MAX_VALUE)
      val LegalTimes = for {
        hour     <- LegalHours
        minute   <- LegalMinutes
        meridian <- LegalMeridians
      } yield (TimeString(f"$hour%d:$minute%02d $meridian"))

      it("returns a legal value for a legal value") {
        forAll (LegalTimes, Addends) { case (TimeString(time), minutes) =>
          noException should be thrownBy formatter.parse(adder.addMinutes(time, minutes))
        }
      }

      it ("adds the minutes to the input time") { 
        forAll (LegalTimes, Addends) { case (TimeString(time), minutes) =>
          val expectedTime = formatter.format(LocalTime.parse(time, formatter).plusMinutes(minutes))

          adder.addMinutes(time, minutes) should equal(expectedTime)
        }
      }
    }

    describe("illegal values") {
      val legalTime = """^(1[012]|[1-9]):[0-5][0-9] (AM|PM)$""".r
      
      //This is probably far too generous a definition of an illegal time
      //to really find edge cases.
      val IllegalTimes = Gen.alphaNumStr suchThat (legalTime.findFirstIn(_).isEmpty)

      it("Throws a TimeParseFailure if the time is illegal") {
        forAll (IllegalTimes) { time =>
          a [ParseTime.TimeParseFailure] should be thrownBy adder.addMinutes(time, 0)
        }
      }
    }
  }
}
