package tungsten

import Utilities._

final case class Function(override name: Symbol,
                          typeParameters: List[Symbol],
                          parameters: List[Symbol],
                          returnType: Type,
                          blocks: List[Symbol],
                          override location: Location = Nowhere)
  extends Definition(name, location)
