package chicala

import scala.tools.nsc
import nsc.Global
import nsc.Phase
import nsc.plugins.Plugin
import nsc.plugins.PluginComponent

import chicala.convert.ChiselToScalaComponent

object ChicalaPlugin {
  val name: String        = "chicala"
  val description: String = "Convert Chisel to semantically equivalent Scala program"
}

object ChicalaConfig {
  var simulation = false
  var lean       = false
}

class ChicalaPlugin(val global: Global) extends Plugin {
  import global._

  val name: String        = ChicalaPlugin.name
  val description: String = ChicalaPlugin.description

  val components: List[PluginComponent] = List(
    new ChiselToScalaComponent(global)
  )

  override def init(options: List[String], error: String => Unit): Boolean = {
    for (option <- options) {
      if (option.startsWith("simulation:")) {
        val emitFormat = option.substring("simulation:".length)
        emitFormat match {
          case "false" => ChicalaConfig.simulation = false
          case "true"  => ChicalaConfig.simulation = true
          case _: String =>
            error("simulation not understood: " + emitFormat)
        }
      } else if (option.startsWith("lean:")) {
        val emitFormat = option.substring("lean:".length)
        emitFormat match {
          case "false" => ChicalaConfig.lean = false
          case "true"  => ChicalaConfig.lean = true
          case _: String =>
            error("lean not understood: " + emitFormat)
        }
      } else {
        error("Option not understood: " + option)
      }
    }
    true
  }

  override val optionsHelp: Option[String] = Some(
    "  -P:chicala:simulation:<true/false>             set emit mode, for simulation or not\n" +
      "  -P:chicala:lean:<true/false>                   emit Lean4 model output"
  )
}
