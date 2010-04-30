package tungsten

trait Copying[T] 
  extends Mapping[T]
{
  def copyWith(changes: (String, AnyRef)*): T = {
    def mapper(field: java.lang.reflect.Field, oldValue: AnyRef): AnyRef = {
      changes.find(_._1 == field.getName) match {
        case Some((_, newValue)) => newValue
        case None => oldValue
      }
    }
    mapFields(mapper)
  }
}
