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

package biz.gsconsulting.add_minutes;

import java.time.*;
import java.time.temporal.*;
import java.time.format.*;

/**
 *
 * A TemporalAdder can add an arbitrary number of minutes to a time 
 * represented as a String with the format "[H]H:MM AM|PM".
 *
 */
public final class TemporalAdder {
  /**
   * A DateTime formatter that can parse a time in the format "[H]H:MM AM|PM"
   */
  private final DateTimeFormatter formatter = DateTimeFormatter.ofPattern("h:mm a");

  /**
   * Add `minutes` to `time`, which must be in the format "[H]H:MM AM|PM"
   *
   * @param time The time to be incremented. Must be in the format "[H]H:MM AM|PM"
   * @param minutes The number of minutes to increment the time
   *
   * @return The time `minutes` minutes from `time`, formatted as "[H]H:MM AM|PM"
   * @throws DateTimeParseException if `time` is not in the correct format.
   *
   */
  public String addMinutes(String time, int minutes) {
    return formatter.format(LocalTime.parse(time, formatter).plusMinutes(minutes));
  }
}
