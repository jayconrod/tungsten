package tungsten

import java.lang.reflect.{Field => ReflectedField}

trait Mapping[T]
{
  def mapFields(function: (ReflectedField, AnyRef) => AnyRef): T = {
    val clas = getClass
    val constructor = clas.getDeclaredConstructors.head    
    val argumentCount = constructor.getParameterTypes.size
    if (argumentCount == 0) 
      this.asInstanceOf[T]
    else {
      val fields = clas.getDeclaredFields
      assert(argumentCount <= fields.size)
      val arguments = for (i <- 0 until argumentCount) yield {
        val fieldName = fields(i).getName
        val getter = clas.getMethod(fieldName)
        val oldValue = getter.invoke(this)
        function(fields(i), oldValue)
      }
      constructor.newInstance(arguments: _*).asInstanceOf[T]
    }
  }

  def mapFieldsRecursively[S <: AnyRef](function: S => S)(implicit m: Manifest[S]): T = {
    val SClass = m.erasure.asInstanceOf[Class[S]]
    def mapper(field: ReflectedField, oldValue: AnyRef): AnyRef = {
      oldValue match {
        case s if SClass.isInstance(s) => function(s.asInstanceOf[S])
        case v: Mapping[_] => v.asInstanceOf[Mapping[AnyRef]].mapFieldsRecursively(function)
        case list: List[_] if !list.isEmpty => {
          if (SClass.isInstance(list.head))
            list.map { s => function(s.asInstanceOf[S]) }
          else if (list.head.isInstanceOf[Mapping[_]])
            list.map { e => e.asInstanceOf[Mapping[AnyRef]].mapFieldsRecursively(function) }
          else
            list
        }
        case _ =>
          oldValue
      }
    }
    mapFields(mapper)
  }

  def mapSymbols(function: Symbol => Symbol): T = mapFieldsRecursively(function)
  def mapValues(function: Value => Value): T = mapFieldsRecursively(function)
  def mapTypes(function: Type => Type): T = mapFieldsRecursively(function)
}
