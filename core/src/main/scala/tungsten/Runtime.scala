/* Copyright 2009-2011 Jay Conrod
 *
 * This file is part of Tungsten.
 *
 * Tungsten is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as 
 * published by the Free Software Foundation, either version 2 of 
 * the License, or (at your option) any later version.
 *
 * Tungsten is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public 
 * License along with Tungsten.  If not, see 
 * <http://www.gnu.org/licenses/>.
 */

package tungsten

import Utilities._

object Runtime {
  lazy val runtime64: Module = {
    val input = getClass.getResourceAsStream("runtime.w")
    val reader = new java.io.InputStreamReader(input)
    ModuleIO.readText(reader, "runtime.w")
  }
  lazy val runtime32 = runtime64.copyWith(is64Bit = false)

  def getRuntime(is64Bit: Boolean): Module = if (is64Bit) runtime64 else runtime32
}
