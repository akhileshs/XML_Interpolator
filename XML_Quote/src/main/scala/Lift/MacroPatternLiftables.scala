package MyPackage

import reflect.macros.blackbox.Context

trait MacroPLiftables extends PLiftables with Unliftables with Nodes {
  val c: Context
  protected lazy val __universe: c.universe.type = c.universe
}

