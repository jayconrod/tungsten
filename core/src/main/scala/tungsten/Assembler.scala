package tungsten

import java.io._
import Utilities._

object Assembler extends Converter[Module, Module] {
  def readSource(filename: String, input: InputStream): Option[Module] = {
    val reader = new InputStreamReader(input)
    ModuleIO.readText(reader, filename) match {
      case Left(source) => Some(source)
      case Right(errors) => {
        errors.foreach(System.err.println _)
        None
      }
    }
  }

  def writeTarget(target: Module, filename: String, output: OutputStream) {
    ModuleIO.writeBinary(target, output)
  }

  def convert(source: Module): Option[Module] = Some(source)

  def convertFile(sourceFile: File): File = fileWithExtension(sourceFile, ".w", ".wo")
}

