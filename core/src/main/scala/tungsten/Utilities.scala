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

import java.io._

object Utilities {
  val ERROR_CODE = 127
  val FAILURE_CODE = 1

  def align(n: Long, alignment: Long): Long = {
    assert(isPowerOf2(alignment))
    (n + alignment - 1) & ~(alignment - 1)
  }

  def charIsPrintable(ch: Char): Boolean = {
    val block = Character.UnicodeBlock.of(ch)
    !Character.isISOControl(ch) &&
      block != null &&
      block != Character.UnicodeBlock.SPECIALS
  }

  def checkNonNullPointerType(given: Type, location: Location): List[CompileException] = {
    if (!given.isPointer || given == NullType)
      List(TypeMismatchException(given.toString, "non-null pointer type", location))
    else
      Nil
  }

  def checkObjectType(given: Type, location: Location): List[CompileException] = {
    if (!given.isInstanceOf[ObjectType])
      List(TypeMismatchException(given.toString, "object type", location))
    else
      Nil
  }

  def checkObjectDefinitionType(given: Type, location: Location): List[CompileException] = {
    if (!given.isInstanceOf[ObjectDefinitionType])
      List(TypeMismatchException(given.toString, "class or interface type", location))
    else
      Nil
  }

  def checkReifiedTypeParameters(given: Type, location: Location): List[CompileException] = {
    def check(errors: List[CompileException], ty: Type): List[CompileException] = {
      ty match {
        case vty: VariableType =>
          NonReifiedTypeParameterException(vty.variableName, location) :: errors
        case _ => errors
      }
    }
    given.foldTypes(Nil, check)
  }

  def checkType(given: Type, expected: Type, location: Location): List[CompileException] = {
    if (given != expected)
      List(TypeMismatchException(given.toString, expected.toString, location))
    else
      Nil
  }

  def compileString(program: String): Module = {
    val Left(module) = ModuleIO.parse(program, "<TEST>")
    module
  }

  def exitWithError(message: String) {
    System.err.println(message)
    System.exit(ERROR_CODE)
  }

  def exitWithFailure(message: String) {
    System.err.println(message)
    System.exit(FAILURE_CODE)
  }

  def fileWithExtension(file: File, oldExt: String, newExt: String): File = {
    val oldFilename = file.getCanonicalPath
    val newFilename = if (oldFilename.endsWith(oldExt))
      oldFilename.substring(0, oldFilename.length - oldExt.length) + newExt
    else
      oldFilename + newExt
    new File(newFilename)
  }

  def hash(code: Int, x: Any): Int = {
    val hashA = 17
    val hashB = 37
    val c = if (x == null) 0 else x.hashCode
    c * hashA + hashB
  }
  def hash(elements: Any*): Int = elements.foldLeft(0)(hash _)

  def humanReadableClassName[T <: Definition](implicit m: Manifest[T]) = {
    val className = m.toString.split("\\.").last
    className.charAt(0).toLower + className.tail.map({c =>
      if (c.isUpper) " " + c.toLower else c.toString
    }).mkString
  }

  def isPowerOf2(x: Long) = (x & (x - 1)) == 0

  def linkRuntime(module: Module): Module = {
    Linker.linkModules(List(module, Runtime.getRuntime(module.is64Bit)),
                       module.name,
                       module.ty,
                       module.version,
                       module.filename,
                       module.dependencies,
                       module.searchPaths)
  }

  def padMap[K, V](map: Map[K, V], keys: Set[K], defaultValue: V): Map[K, V] = {
    (map /: keys) { (map, k) =>
      if (map.contains(k))
        map
      else
        map + (k -> defaultValue)
    }
  }

  def parseVersion(string: String): Version = {
    tryParseVersion(string) match {
      case Some(v) => v
      case None => throw new IllegalArgumentException
    }
  }

  def readContentsOfFile(file: File): String = {
    val reader = new FileReader(file)
    val contents = readContentsOfFile(reader)
    reader.close
    contents
  }

  def readContentsOfFile(input: InputStream): String = {
    val reader = new InputStreamReader(input)
    readContentsOfFile(reader)
  }

  def readContentsOfFile(input: Reader): String = {
    val buffer = new StringBuffer
    val block = new Array[Char](4096)
    var count = input.read(block)
    while (count != -1) {
      for (i <- 0 until count)
        buffer.append(block(i))
      count = input.read(block)
    }
    buffer.toString 
  }    

  implicit def listToStagedList[T](list: List[T]): StagedList[T] = new StagedList(list)

  def stage[T](a: List[T], b: => List[T]): List[T] = {
    if (!a.isEmpty) a else b
  }

  def stage[T](a: List[T], b: => List[T], c: => List[T]): List[T] = {
    if (!a.isEmpty) a else stage(b, c)
  }

  def stage[T](a: List[T], b: => List[T], c: => List[T], d: => List[T]): List[T] = {
    if (!a.isEmpty) a else stage(b, c, d)
  }

  def stage[T](a: List[T], b: => List[T], c: => List[T], d: => List[T], 
               e: => List[T]): List[T] =
  {
    if (!a.isEmpty) a else stage(b, c, d, e)
  }

  def stage[T](a: List[T], b: => List[T], c: => List[T], d: => List[T],
               e: => List[T], f: => List[T]): List[T] =
  {
    if (!a.isEmpty) a else stage(b, c, d, e, f)
  }

  def stage[T](a: List[T], b: => List[T], c: => List[T], d: => List[T],
               e: => List[T], f: => List[T], g: => List[T]): List[T] =
  {
    if (!a.isEmpty) a else stage(b, c, d, e, f, g)
  }

  def stage[T](a: List[T], b: => List[T], c: => List[T], d: => List[T],
               e: => List[T], f: => List[T], g: => List[T], h: => List[T]): List[T] =
  {
    if (!a.isEmpty) a else stage(b, c, d, e, f, g, h)
  }

  def stage[T](a: List[T], b: => List[T], c: => List[T], d: => List[T],
               e: => List[T], f: => List[T], g: => List[T], h: => List[T],
               i: => List[T]): List[T] =
  {
    if (!a.isEmpty) a else stage(b, c, d, e, f, g, h, i)
  }

  def stage[T](a: List[T], b: => List[T], c: => List[T], d: => List[T],
               e: => List[T], f: => List[T], g: => List[T], h: => List[T],
               i: => List[T], j: => List[T]): List[T] =
  {
    if (!a.isEmpty) a else stage(b, c, d, e, f, g, h, i, j)
  }

  def stage[T](a: List[T], b: => List[T], c: => List[T], d: => List[T],
               e: => List[T], f: => List[T], g: => List[T], h: => List[T],
               i: => List[T], j: => List[T], k: => List[T]): List[T] =
  {
    if (!a.isEmpty) a else stage(b, c, d, e, f, g, h, i, j, k)
  }

  implicit def symbolFromString(string: String) = {
    Lexer.test(string, Lexer.generalSymbol(true))
  }

  def testModule(definitions: Definition*): Module = {
    import scala.collection.immutable.TreeMap
    val empty = TreeMap[Symbol, Definition]()
    val defnMap = (empty /: definitions) { (defnMap, defn) => defnMap + (defn.name -> defn) }
    new Module(definitions = defnMap)
  }

  def tryParseVersion(string: String): Option[Version] = {
    val versionRegex = "[0-9]+(\\.[0-9]+)*"
    if (string.matches(versionRegex)) {
      val elements = string.split("\\.").toList.map(_.toInt)
      Some(new Version(elements))
    } else
      None
  }

  def typeToString(ty: Type): String = {
    val writer = new TextModuleWriter(new Module, null)
    writer.localType(ty, None)
  }

  def valueToString(value: Value): String = {
    val writer = new TextModuleWriter(new Module, null)
    writer.localValue(value, None)
  }

  def wordSize(module: Module) = if (module.is64Bit) 8 else 4
}

class StagedList[T](left: List[T]) {
  def ++?[S >: T](right: => List[S]): List[S] = {
    if (left.isEmpty) right else left
  }
}
