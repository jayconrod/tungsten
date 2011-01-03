package tungsten

trait CommandLine {
  def parseArguments(args: Seq[String]): Either[(Map[String, String], List[String]), String] = {
    val optionList = availableOptions
    val optionShortMap = (Map[Char, CommandLineOption]() /: optionList) { (optionMap, option) =>
      option.shortName match {
        case None => optionMap
        case Some(name) => optionMap + (name -> option)
      }
    }
    val optionLongMap = (Map[String, CommandLineOption]() /: optionList) { (optionMap, option) =>
      optionMap + (option.longName -> option)
    }

    def getOption(optionStr: String): Option[CommandLineOption] = {
      if (2 == optionStr.size && 
          optionStr.startsWith("-") &&
          optionShortMap.contains(optionStr(1)))
        Some(optionShortMap(optionStr(1)))
      else if (2 > optionStr.size &&
               optionStr.startsWith("--") &&
               optionLongMap.contains(optionStr.drop(2)))
        Some(optionLongMap(optionStr.drop(2)))
      else
        None
    }        

    def hasValue(optionStr: String): Boolean = {
      getOption(optionStr) match {
        case Some(option) if option.valueName.isDefined => true
        case _ => false
      }
    }

    def parseNext(remainingArgs: List[String],
                  options: Map[String, String],
                  realArgs: List[String]): Either[(Map[String, String], List[String]), String] =
    {
      remainingArgs match {
        case Nil => Left((options, realArgs.reverse))
        case "--" :: more => Left((options, (realArgs ++ more).reverse))
        case arg :: more => {
          getOption(arg) match {
            case None => parseNext(more, options, arg :: realArgs)
            case Some(option) => {
              if (option.requiresValue) {
                more match {
                  case Nil => Right("option %s requires an argument".format(arg))
                  case optArg :: _ if !option.valueFilter(optArg) => 
                    Right("invalid argument for option %s".format(optArg))
                  case optArg :: more =>
                    parseNext(more, options + (option.longName -> optArg), realArgs)
                }
              } else
                parseNext(more, options + (option.longName -> null), realArgs)
            }
          }
        }
      }
    }

    parseNext(args.toList, Map(), Nil)
  }

  def usage: String = {
    val optionDescriptions = availableOptions map { opt =>
      val shortNameStr = opt.shortName match {
        case None => ""
        case Some(name) => name + ", "
      }
      val valueStr = opt.valueName match {
        case None => ""
        case Some(valueName) => "=" + valueName
      }
      "  %s%s%s %-40s\n".format(shortNameStr, opt.longName, valueStr, opt.valueName)
    }
    "%s\n\n%s\n".format(usageSynopsis, optionDescriptions)
  }

  def usageSynopsis: String

  def availableOptions: List[CommandLineOption]
}

trait CommandLineProgram extends CommandLine {
  private var _arguments: List[String] = null
  private var _options: Map[String, String] = null

  def arguments = _arguments
  def options = _options
  def hasOption(name: String) = _options.contains(name)
  def getOption(name: String) = _options.get(name)

  def main(args: Array[String]) {
    parseArguments(args) match {
      case Right(errorMessage) => {
        System.err.println(errorMessage)
        System.exit(Utilities.FAILURE_CODE)
      }
      case Left((opts, args)) => {
        _arguments = args
        _options = opts
      }
    }

    if (hasOption("help")) {
      System.err.println(usage)
      System.exit(Utilities.FAILURE_CODE)
    }
  }

  override def availableOptions: List[CommandLineOption] = {
    List(CommandLineOption(Some('h'), "help", "prints these usage instructions"))
  }
}

case class CommandLineOption(shortName: Option[Char],
                             longName: String,
                             description: String,
                             valueName: Option[String] = None,
                             valueFilter: String => Boolean = { arg => true })
{
  def requiresValue: Boolean = valueName.isDefined
}

