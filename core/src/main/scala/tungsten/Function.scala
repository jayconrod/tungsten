package tungsten

import Utilities._

final case class Function(name: Symbol,
                          returnType: Type,
                          typeParameters: List[Symbol],
                          parameters: List[Symbol],
                          blocks: List[Symbol],
                          annotations: List[AnnotationValue] = Nil)
  extends Definition
{
  override def isGlobal = true

  def ty(module: Module): FunctionType = {
    FunctionType(returnType, 
                 typeParameters,
                 module.getParameters(parameters).map(_.ty))
  }

  def isDefined = !blocks.isEmpty

  override def validateComponents(module: Module) = {
    super.validateComponents(module) ++ 
      validateComponentsOfClass[TypeParameter](module, typeParameters) ++
      validateComponentsOfClass[Parameter](module, parameters) ++
      validateComponentsOfClass[Block](module, blocks)
  }

  override def validateScope(module: Module, scope: Set[Symbol]): List[CompileException] = {
    val tpErrors = validateComponentsScope(module, scope, typeParameters)
    validateTypeAndValueScope(scope ++ typeParameters) ++
      validateComponentsScope(module, scope ++ typeParameters, parameters) ++
      validateComponentsScope(module, scope ++ typeParameters ++ parameters ++ blocks, blocks)
  }

  override def validate(module: Module) = {
    def validateReturnType = {
      blocks flatMap { blockName =>
        val block = module.getBlock(blockName)
        block.instructions.lastOption match {
          case Some(retName) => module.get[ReturnInstruction](retName) match {
            case Some(ret) => {          
              val retTy = ret.value.ty
              if (returnType != retTy)
                List(TypeMismatchException(retTy.toString, returnType.toString, ret.getLocation))
              else
                Nil
            }
            case None => Nil
          }
          case None => Nil
        }
      }
    }

    def validateEntryParameters = {
      blocks match {
        case Nil => Nil
        case entryName :: _ => {
          val entry = module.getBlock(entryName)
          if (entry.parameters.isEmpty)
            Nil
          else
            List(EntryParametersException(name, entry.name, getLocation))
        }
      }
    }

    def validateBranches = {
      blocks flatMap { blockName =>
        val block = module.getBlock(blockName)
        block.instructions flatMap { instName =>
          val inst = module.getInstruction(instName)
          val blockNames = inst match {
            case BranchInstruction(_, _, target, _, _) => List(target)
            case ConditionalBranchInstruction(_, _, _, trueTarget, _, falseTarget, _, _) =>
              List(trueTarget, falseTarget)
            case _ => Nil
          }
          blockNames flatMap { n =>
            if (!blocks.contains(n)) {
              module.getDefn(n) match {
                case Some(_) => List(NonLocalBranchException(name, n, inst.getLocation))
                case None => List(UndefinedSymbolException(n, inst.getLocation))
              }
            } else
              Nil
          }
        }
      }
    }

    def validateVariance = {
      val parameterTypes = module.getParameters(parameters).map(_.ty)
      returnType.validateVariance(Variance.COVARIANT, module, getLocation) ++
        parameterTypes.flatMap(_.validateVariance(Variance.CONTRAVARIANT, module, getLocation))
    }

    def validateAbstract = {
      if (isAbstract && isDefined)
        List(AbstractMethodDefinedException(name, getLocation))
      else if (isAbstract && isFinal)
        List(AbstractFinalMethodException(name, getLocation))
      else
        Nil
    }

    def validatePredecessors = {
      val cfg = controlFlowGraph(module)
      for (block <- cfg.nodes.toList;
           if !block.parameters.isEmpty && cfg.incident(block).isEmpty)
        yield BlockPredecessorException(block.name, block.getLocation)
    }

    stage(super.validate(module),
          validateEntryParameters,
          validateBranches,
          validateReturnType,
          validateVariance,
          validateAbstract,
          validatePredecessors)
  }

  def controlFlowGraph(module: Module): Graph[Block] = {
    val blockDefns = module.getBlocks(blocks)
    val successorMap = (Map[Block, Set[Block]]() /: blockDefns) { (deps, defn) =>
      deps + (defn -> defn.successors(module))
    }
    new Graph(blockDefns, successorMap)
  }
}
