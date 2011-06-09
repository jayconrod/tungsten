/* Copyright 2009-2011 Jay Conrod
 *
 * This file is part of Tungsten.
 *
 * Tungsten is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as 
 * published by the Free Software Foundation, either version 2 of 
 * the License, or (at your option) any later version.
 *
 * Tungsten is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public 
 * License along with Tungsten.  If not, see 
 * <http://www.gnu.org/licenses/>.
 */

package tungsten

trait CommandLine {
  def parseArguments(args: Seq[String]): Either[(Map[String, String], List[String], List[String]), String] = {
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
      else if (2 < optionStr.size &&
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
                  optionValues: Map[String, String],
                  options: List[String],
                  realArgs: List[String]): Either[(Map[String, String], List[String], List[String]), String] =
    {
      remainingArgs match {
        case Nil => Left((optionValues, options.reverse, realArgs.reverse))
        case "--" :: more => Left((optionValues, options.reverse, (realArgs ++ more).reverse))
        case arg :: more if arg.size > 1 && arg.startsWith("-") => {
          getOption(arg) match {
            case None => Right("invalid option: " + arg)
            case Some(option) => {
              if (option.requiresValue) {
                more match {
                  case Nil => 
                    Right("option %s requires an argument".format(arg))
                  case optArg :: _ if !option.valueFilter(optArg) => 
                    Right("invalid argument for option %s: %s".format(arg, optArg))
                  case optArg :: more => {
                    parseNext(more, 
                              optionValues + (option.longName -> optArg), 
                              option.longName :: options,
                              realArgs)
                  }
                }
              } else {
                parseNext(more, 
                          optionValues + (option.longName -> null), 
                          option.longName :: options,
                          realArgs)
              }
            }
          }
        }
        case arg :: more => parseNext(more, optionValues, options, arg :: realArgs)
      }
    }

    parseNext(args.toList, Map(), Nil, Nil)
  }

  def usage: String = {
    val optionDescriptions = availableOptions map { opt =>
      val shortNameStr = opt.shortName match {
        case None => ""
        case Some(name) => "-%s, ".format(name)
      }
      val valueStr = opt.valueName match {
        case None => ""
        case Some(valueName) => "=" + valueName
      }
      val optionStr = "%s--%s%s".format(shortNameStr, opt.longName, valueStr)
      "  %-30s  %s".format(optionStr, opt.description)
    }
    val optionDescriptionStr = optionDescriptions.mkString("Available Options:\n", "\n", "\n")
    usageSynopsis + "\n\n" + optionDescriptionStr
  }

  def usageSynopsis: String

  def availableOptions: List[CommandLineOption]
}

trait CommandLineProgram extends CommandLine {
  private var _arguments: List[String] = null
  private var _optionValues: Map[String, String] = null
  private var _options: List[String] = null

  def arguments = _arguments
  def optionValues = _optionValues
  def options = _options
  def hasOption(name: String) = _optionValues.contains(name)
  def getOption(name: String) = _optionValues.get(name)

  def main(args: Array[String]) {
    parseArguments(args) match {
      case Right(errorMessage) => {
        System.err.println(errorMessage)
        System.exit(Utilities.FAILURE_CODE)
      }
      case Left((vals, opts, args)) => {
        _arguments = args
        _optionValues = vals
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

