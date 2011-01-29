package tungsten

import java.io.{File, InputStream, OutputStream}

object Opt extends Converter[Module, Module]
{
  private var passes = Map[String, Pass]()

  def registerPass(pass: Pass) {
    if (passes.contains(pass.name)) {
      System.err.println("error: duplicate pass registered: " + pass.name)
      System.exit(1)
    }
    passes += pass.name -> pass
  }
  registerPass(LowerPass)

  def usageSynopsis = "opt [-pass1 -pass2...] file1 file2..."

  override def availableOptions: List[CommandLineOption] = {
    val options = passes.values.map { pass => 
      CommandLineOption(None, pass.name, pass.description)
    }
    super.availableOptions ++ options
  }

  def readSource(filename: String, input: InputStream): Option[Module] = {
    val module = ModuleIO.readBinary(input)
    val errors = module.validate
    if (errors.isEmpty)
      Some(module)
    else {
      System.err.println(filename + ": validation errors occurred")
      errors.foreach(System.err.println _)
      System.err.println()
      None
    }
  }

  def writeTarget(target: Module, filename: String, output: OutputStream) {
    ModuleIO.writeBinary(target, output)
  }

  def convert(source: Module): Option[Module] = {
    var m = source
    for (passName <- options;
         pass     <- passes.get(passName))
      m = pass(m)
    Some(m)
  }

  def convertFile(sourceFile: File): File = sourceFile
}
