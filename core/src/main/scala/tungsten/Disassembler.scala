package tungsten

import java.io._
import Utilities._

object Disassembler extends Converter[Module, Module] {
  def readSource(filename: String, input: InputStream): Option[Module] = {
    Some(ModuleIO.readBinary(input))
  }

  def writeTarget(target: Module, filename: String, output: OutputStream) {
    val writer = new OutputStreamWriter(output)
    ModuleIO.writeText(target, writer)
  }

  def convert(source: Module): Option[Module] = Some(source)

  def convertFile(sourceFile: File): File = fileWithExtension(sourceFile, ".wo", ".w")
}
