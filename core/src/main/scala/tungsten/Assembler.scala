package tungsten

import java.io._
import Utilities._

object Assembler extends Converter[Module, Module] 
{
  def usageSynopsis = "assembler file1 file2..."

  override def availableOptions: List[CommandLineOption] = {
    super.availableOptions ++
      List(CommandLineOption(Some('r'), "no-runtime", "skips linking runtime definitions"))
  }

  def readSource(filename: String, input: InputStream): Option[Module] = {
    val code = readContentsOfFile(input)
    ModuleIO.parse(code, filename) match {
      case Left(module) => Some(module)
      case Right(errors) => {
        System.err.println("%s: parse errors occurred\n".format(filename))
        errors.foreach(System.err.println _)
        None
      }
    }
  }

  def writeTarget(target: Module, filename: String, output: OutputStream) {
    ModuleIO.writeBinary(target, output)
  }

  def convert(source: Module): Option[Module] = {
    val linkedModule = if (hasOption("no-runtime"))
      source
    else
      linkRuntime(source)
    val errors = linkedModule.validate
    if (errors.isEmpty)
      Some(linkedModule)
    else {
      val filename = linkedModule.filename.map(_.toString + ": ").getOrElse("")
      System.err.println(filename + "validation errors occurred\n")
      errors.foreach(System.err.println _)
      None
    }
  }

  def convertFile(sourceFile: File): File = fileWithExtension(sourceFile, ".w", ".wo")
}

