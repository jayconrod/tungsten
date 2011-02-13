package tungsten.llvm

import java.io._
import tungsten.ModuleIO
import tungsten.Utilities._

object LlvmToTungsten extends tungsten.Converter[Module, tungsten.Module] {
  def usageSynopsis = "llvm-to-tungsten file1 file2..."

  def readSource(filename: String, input: InputStream): Option[Module] = {
    val text = readContentsOfFile(input)
    val reader = new Lexer.Scanner(text)
    Parser.phrase(Parser.module)(reader) match {
      case Parser.Success(source, _) => Some(source)
      case error: Parser.NoSuccess => throw new IOException(error.msg)
    }
  }

  def writeTarget(target: tungsten.Module, filename: String, output: OutputStream) {
    ModuleIO.writeBinary(target, output)
  }

  def convert(source: Module): Option[tungsten.Module] = {
    val target = LlvmToTungstenConverter(source)
    target.validate match {
      case Nil => Some(target)
      case errors => {
        errors.foreach(System.err.println _)
        None
      }
    }
  }

  def convertFile(sourceFilename: File): File = {
    fileWithExtension(sourceFilename, ".ll", ".wo")
  }
}
