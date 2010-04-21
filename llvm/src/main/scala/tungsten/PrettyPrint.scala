package tungsten.llvm

import java.io._
import tungsten.Utilities._

object PrettyPrint extends tungsten.Converter[Module, String] {
  def readSource(filename: String, input: InputStream): Option[Module] = {
    val text = readContentsOfFile(input)
    val reader = new Lexer.Scanner(text)
    Parser.phrase(Parser.module)(reader) match {
      case Parser.Success(source, _) => Some(source)
      case error: Parser.NoSuccess => throw new IOException(error.msg)
    }
  }

  def writeTarget(target: String, filename: String, output: OutputStream) {
    val writer = new OutputStreamWriter(output)
    writer.write(target)
    writer.flush
  }

  def convert(source: Module): Option[String] = {
    Some(source.toString)
  }

  def convertFile(sourceFilename: File): File = {
    fileWithExtension(sourceFilename, ".ll", ".ll~")
  }
}
