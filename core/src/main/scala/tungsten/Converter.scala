package tungsten

import java.io._
import Utilities._

trait Converter[S, T] {
  var errorOccurred = false

  def main(args: Array[String]) {
    if (args.isEmpty) {
      readSourceFromStdin.foreach { source =>
        val target = convert(source)
        writeTargetToStdout(target)
      }
    } else {
      for (filename <- args;
           val inputFile =  new File(filename);
           source <- readSourceFromFile(inputFile))
      {
        val outputFile = convertFile(inputFile)
        val target = convert(source)
        writeTargetToFile(target, outputFile)
      }
    }

    if (errorOccurred)
      System.exit(FAILURE_CODE)
  }

  def readSourceFromStdin: Option[S] = {
    readSource("<stdin>", System.in)
  }

  def readSourceFromFile(file: File): Option[S] = {
    val input = new BufferedInputStream(new FileInputStream(file))
    val source = readSource(file.getName, input)
    input.close
    source
  }

  def readSource(filename: String, input: InputStream): Option[S]

  def writeTargetToStdout(target: T) {
    writeTarget(target, "<stdout>", System.out)
  }

  def writeTargetToFile(target: T, file: File) {
    val output = new BufferedOutputStream(new FileOutputStream(file))
    writeTarget(target, file.getName, output)
    output.close
  }

  def writeTarget(target: T, filename: String, output: OutputStream)

  def convert(source: S): T

  def convertFile(sourceFile: File): File
}
