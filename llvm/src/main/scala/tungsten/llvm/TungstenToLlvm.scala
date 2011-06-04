package tungsten.llvm

import scala.util.matching.Regex
import java.io.{InputStream, OutputStream, OutputStreamWriter, File}
import tungsten.Converter
import tungsten.CommandLineOption
import tungsten.ModuleIO
import tungsten.Utilities._

object TungstenToLlvm extends Converter[tungsten.Module, Module] {
  def usageSynopsis = "tungsten-to-llvm file1 file2..."

  override def availableOptions: List[CommandLineOption] = {
    super.availableOptions ++ 
      List(CommandLineOption(Some('t'), "target", "target triple (cpu-os-vendor)",
                             Some("target"),
                             _.matches("\\w+-\\w+-\\w+")))
  }

  def readSource(filename: String, input: InputStream): Option[tungsten.Module] = {
    val module = ModuleIO.readBinary(input)
    val errors = module.validate
    if (errors.isEmpty)
      Some(module)
    else {
      System.err.println(filename + ": validation errors occurred:")
      errors.foreach(System.err.println _)
      setErrorOccurred
      None
    }
  }

  def writeTarget(module: Module, filename: String, output: OutputStream) {
    val writer = new OutputStreamWriter(output)
    writer.write(module.toString)
    writer.flush
  }

  def convert(module: tungsten.Module): Option[Module] = {
    val targetTriple = getOption("target").orElse(autoDetectTargetTriple(module.is64Bit))
    val lowered = tungsten.LowerPass(module)
    val compatible = LlvmCompatibilityPass(lowered)
    val converted = TungstenToLlvmConverter(compatible, targetTriple)
    Some(converted)
  }

  def autoDetectTargetTriple(is64Bit: Boolean): Option[String] = {
    val cpu = (is64Bit, System.getProperty("os.arch")) match {
      case (true, "amd64" | "x86_64" | "i386") => "x86_64"
      case (false, "amd64" | "x86_64" | "i386") => "i386"
      case (_, arch) => arch
    }
    val (vendor, os) = System.getProperty("os.name") match {
      case "Linux" => ("pc-linux", "gnu")
      case "Mac OS X" => ("apple", "darwin10.0.0")
      case os => (os, "unknown")
    }
    val triple = Some("%s-%s-%s".format(cpu, vendor, os))
    triple
  }

  def convertFile(sourceFile: File): File = fileWithExtension(sourceFile, ".wo", ".ll")
}
