package tungsten.interpreter

import scala.collection.immutable.Stack
import tungsten._
import tungsten.BinaryOperator._
import tungsten.RelationalOperator._

final class Environment(val module: Module) {
  val init = Array[Instruction](StaticCallInstruction(new Symbol("init"), 
                                                      new Symbol("main"),
                                                      Nil),
                                IntrinsicCallInstruction(new Symbol("exit"), 
                                                      Intrinsic.EXIT,
                                                      List(tungsten.Int32Value(0))))

  var stack = new Stack[State]

  var state = new State(new Symbol("init"), init, 0, Map[Symbol, IValue]())

  var globalState = {
    module.definitions.valuesIterator.foldLeft(Map[Symbol, IValue]()) { (st, defn) =>
      defn match {
        case Global(name, _, Some(value), _) => {
          val ivalue = ScalarReference(create(value))
          st + (name -> ivalue)
        }
        case Global(name, ty, None, _) => {
          val ivalue = ScalarReference(create(ty.defaultValue(module)))
          st + (name -> ivalue)
        }
        case _ => st
      }
    }
  }

  var returnCode = 0

  def run = {
    while (state != null)
      step
    returnCode
  }

  def step = {
    val oldState = state
    val result = evalInst(state.inst)
    if (state == oldState)  // normal non-control instruction
      state = state.add(result).next
  }

  def evalInst(inst: Instruction): IValue = {
    inst match {
      case AddressInstruction(_, base, indices, _) => {
        val iBase = create(base).asInstanceOf[Reference]
        val iIndices = indices.map(create _)
        def address(ptr: Reference, indices: List[IValue]): IValue = {
          indices match {
            case Nil => ptr
            case (i: Int64Value) :: is => {
              val newPtr = ptr.value match {
                case v: ArrayValue => ArrayIndexReference(v, i)
                case v: StructValue => StructIndexReference(v, i)
                case _ => throw new RuntimeException("non-indexable value " + ptr.value)
              }
              address(newPtr, is)
            }
            case _ => throw new RuntimeException("could not calculate address with index " + iIndices.head)
          }
        }
        address(iBase, iIndices)
      }
      case AssignInstruction(_, value, _) => create(value)
      case BinaryOperatorInstruction(_, op, left, right, _) => {
        val l = create(left)
        val r = create(right)
        evalBinop(op, l, r)
      }
      case BranchInstruction(name, bbName, args, _) => {
        val iargs = args.map(create _)
        val block = module.get[Block](bbName).get
        branch(block, iargs)
        state = state.add(name, UnitValue)
        UnitValue
      }
      case ConditionalBranchInstruction(_, cond, 
                                        trueTarget, trueArgs, 
                                        falseTarget, falseArgs, _) =>
      {
        val BooleanValue(c) = create(cond)
        val iTrueArgs = trueArgs.map(create _)
        val iFalseArgs = falseArgs.map(create _)
        val (target, args) = if (c) 
          (trueTarget, iTrueArgs)
        else 
          (falseTarget, iFalseArgs)
        val block = module.getBlock(target)
        branch(block, args)
        UnitValue
      }        
      case IndirectCallInstruction(_, target, arguments, _) => {
        val function = create(target).asInstanceOf[FunctionValue].value
        val args = arguments.map(create(_))
        call(function, args)
        UnitValue
      }
      case IntrinsicCallInstruction(_, intrinsic, arguments, _) => {
        import tungsten.Intrinsic._
        intrinsic match {
          case EXIT => {
            returnCode = create(arguments(0)).asInstanceOf[Int32Value].value
            state = null
          }
        }
        UnitValue
      }
      case LoadInstruction(_, pointer, _) => {
        val scalar = create(pointer).asInstanceOf[Reference]
        scalar.value
      }
      case LoadElementInstruction(_, base, indices, _) => {
        def loadElement(ptr: IValue, indices: List[IValue]): IValue = {
          indices match {
            case Nil => ptr
            case i :: is => {
              val index = i.asInstanceOf[Int64Value].value.asInstanceOf[Int]
              ptr match {
                case ArrayValue(array) => loadElement(array(index), is)
                case StructValue(struct) => loadElement(struct(index), is)
                case _ => throw new RuntimeException("indexed non-array value")
              }
            }
          }
        }
        loadElement(create(base), indices.map(create _))
      }
      case RelationalOperatorInstruction(_, op, left, right, _) => {
        val l = create(left)
        val r = create(right)
        evalRelop(op, l, r)
      }
      case ReturnInstruction(_, v, _) => {
        val ret = create(v)
        state = stack.head
        stack = stack.pop
        state = state.add(state.inst.name, ret).next
        UnitValue   // gets assigned to the actual return instruction
      }
      case StackAllocateInstruction(_, ty, _) => {
        val elementType = ty.asInstanceOf[PointerType].elementType
        val defaultValue = create(elementType.defaultValue(module))
        new ScalarReference(defaultValue)
      }
      case StackAllocateArrayInstruction(_, count, elementType, _) => {
        val cCount = create(count)
        val n = cCount.asInstanceOf[Int64Value].value.asInstanceOf[Int]
        val a = new Array[IValue](n)
        for (i <- 0 until n)
          a(i) = create(elementType.defaultValue(module))
        val array = new ArrayValue(a)
        new ScalarReference(array)
      }
      case StaticCallInstruction(name, target, arguments, _) => {
        val function = module.get[Function](target).get
        val args = arguments.map(create _)
        call(function, args)
        UnitValue
      }
      case StoreInstruction(_, pointer, value, _) => {
        val ipointer = create(pointer).asInstanceOf[Reference]
        val ivalue = create(value)
        ipointer.value = ivalue
        UnitValue
      }
      case StoreElementInstruction(_, base, indices, value, _) => {
        val v = create(value)
        def storeElement(ptr: IValue, indices: List[IValue]): Unit = {
          indices match {
            case i :: Nil => {
              val index = i.asInstanceOf[Int64Value].value.asInstanceOf[Int]
              ptr match {
                case ArrayValue(array) => array(index) = v
                case StructValue(struct) => struct(index) = v
                case _ => throw new RuntimeException("indexed non-array value")
              }
            }
            case i :: is => {
              val index = i.asInstanceOf[Int64Value].value.asInstanceOf[Int]
              ptr match {
                case ArrayValue(array) => storeElement(array(index), is)
                case StructValue(struct) => storeElement(struct(index), is)
                case _ => throw new RuntimeException("indexed non-array value")
              }
            }
            case _ => throw new RuntimeException("can't have empty index list")
          }
        }
        storeElement(create(base), indices.map(create _))
        UnitValue
      }
      case UpcastInstruction(_, value, _, _) => create(value)
    }
  }

  def setArguments(paramNames: List[Symbol], arguments: List[IValue]) = {
    assert(paramNames.length == arguments.length)
    state = (paramNames zip arguments).foldLeft(state) { (st, arg) =>
      val (k, v) = arg
      st.add(k, v)
    }
  }         

  def branch(block: Block, arguments: List[IValue]) = {
    state = new State(block.name, 
                      module.getInstructions(block.instructions).toArray,
                      0,
                      state.values)
    setArguments(block.parameters, arguments)
  }

  def call(function: Function, arguments: List[IValue]) = {
    stack = stack.push(state)
    state = new State(function, module)
    setArguments(function.parameters, arguments)
  }

  def evalBinop(op: BinaryOperator, left: IValue, right: IValue) = {
    val methodName = scala.util.NameTransformer.encode(op.name)
    val method = left.getClass.getMethod(methodName, right.getClass)
    method.invoke(left, right).asInstanceOf[IValue]
  }

  def evalRelop(op: RelationalOperator, left: IValue, right: IValue): BooleanValue = {
    if (op == RelationalOperator.EQUAL)
      BooleanValue(left equals right)
    else if (op == RelationalOperator.NOT_EQUAL)
      BooleanValue(!(left equals right))
    else {
      val methodName = scala.util.NameTransformer.encode(op.name)
      val method = left.getClass.getMethod(methodName, right.getClass)
      method.invoke(left, right).asInstanceOf[BooleanValue]
    }
  }

  def create(value: tungsten.Value): tungsten.interpreter.IValue = {
    value match {
      case tungsten.UnitValue(_) => tungsten.interpreter.UnitValue
      case tungsten.BooleanValue(v, _) => tungsten.interpreter.BooleanValue(v)
      case tungsten.Int8Value(v, _) => tungsten.interpreter.Int8Value(v)
      case tungsten.Int16Value(v, _) => tungsten.interpreter.Int16Value(v)
      case tungsten.Int32Value(v, _) => tungsten.interpreter.Int32Value(v)
      case tungsten.Int64Value(v, _) => tungsten.interpreter.Int64Value(v)
      case tungsten.Float32Value(v, _) => tungsten.interpreter.Float32Value(v)
      case tungsten.Float64Value(v, _) => tungsten.interpreter.Float64Value(v)
      case tungsten.NullValue(_) => tungsten.interpreter.NullReference
      case tungsten.ArrayValue(_, vs, _) => {
        tungsten.interpreter.ArrayValue(vs.toArray.map(create _))
      }
      case tungsten.StructValue(_, vs, _) => {
        tungsten.interpreter.StructValue(vs.toArray.map(create _))
      }
      case tungsten.DefinedValue(sym, _) => {
        module.getDefn(sym) match {
          case Some(_: Parameter) | Some(_: Instruction) => state.get(sym)
          case Some(_: Global) => globalState(sym)
          case Some(f: Function) => tungsten.interpreter.FunctionValue(f)
          case _ => throw new RuntimeException("definition " + sym + " is not a value")
        }
      }
    }
  }
}
