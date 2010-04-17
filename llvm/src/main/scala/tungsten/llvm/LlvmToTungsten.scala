package tungsten.llvm

import java.io._
import tungsten.Utilities._

object LlvmToTungsten extends tungsten.Converter[Module, tungsten.Module] {
  def readSource(filename: String, input: InputStream): Option[Module] = {
    val text = readContentsOfFile(input)
    val reader = new Lexer.Scanner(text)
    Parser.phrase(Parser.module)(reader) match {
      case Parser.Success(source, _) => Some(source)
      case error: Parser.NoSuccess => throw new IOException(error.msg)
    }
  }

  def writeTarget(target: tungsten.Module, filename: String, output: OutputStream) {
    tungsten.ModuleIO.writeBinary(target, output)
  }

  def convert(source: Module): Option[tungsten.Module] = {
    Some(new tungsten.Module)
  }

  def convertFile(sourceFilename: File): File = {
    fileWithExtension(sourceFilename, ".ll", ".wo")
  }
}