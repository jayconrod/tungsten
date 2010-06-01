package tungsten.llvm

import org.junit.Test
import org.junit.Assert._
import tungsten.{ModuleIO, Symbol, Graph}
import tungsten.Utilities._

class PhiAnalysisTest {
  val code = "function int64 @f {\n" +
             "  block %bb0 {\n" +
             "    cond true ? @f.bb1(int64 12, int64 34) : @f.bb2(int64 12, int64 56)\n" +
             "  }\n" +
             "  block %bb1(int64 %a, int64 %b) {\n" +
             "    branch @f.bb3(int64 %a, int64 %b)\n" +
             "  }\n" +
             "  block %bb2(int64 %c, int64 %d) {\n" +
             "    branch @f.bb3(int64 %c, int64 %d)\n" +
             "  }\n" +
             "  block %bb3(int64 %e, int64 %f) {\n" +
             "    binop int64 %x = int64 %e + int64 %f\n" +
             "    return int64 %f\n" +
             "  }\n" +
             "}\n"
  val Left(module) = ModuleIO.readText(code)
  val function = module.getFunction("f")
  val blockNames = List(0, 1, 2, 3).map { i: Int => symbolFromString("f.bb" + i) }
  val blocks = module.getBlocks(blockNames)
  val graph = PhiConversion.cfg(function, module)
  val analysis = new PhiAnalysis(module)

  @Test
  def cfg {
    val expected = new Graph[tungsten.Block](blocks,
                                             Map((blocks(0), Set(blocks(1), blocks(2))),
                                                 (blocks(1), Set(blocks(3))),
                                                 (blocks(2), Set(blocks(3))),
                                                 (blocks(3), Set())))
    assertEquals(graph, expected)
  }

  @Test
  def bottom {
    val expected = Map((symbolFromString("f.bb1.a"), tungsten.IntValue(12, 64)),
                       (symbolFromString("f.bb1.b"), tungsten.IntValue(34, 64)))                                                    
    val data = analysis.bottom(blocks(0), blocks(1))
    assertEquals(expected, data)
  }

  @Test
  def flow {
    val expected = Map((blocks(3), Map((symbolFromString("f.bb3.e"), tungsten.IntValue(12, 64)),
                                       (symbolFromString("f.bb3.f"), tungsten.IntValue(34, 64)))))
    val inData = Map(blocks(0) -> analysis.bottom(blocks(0), blocks(1)))
    val outData = analysis.flow(graph, blocks(1), inData)
    assertEquals(expected, outData)
  }
}
