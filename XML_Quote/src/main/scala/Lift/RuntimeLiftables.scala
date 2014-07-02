package MyPackage

import reflect.runtime.universe

object RuntimeLiftables extends PLiftables with Unliftables with Nodes {
  protected lazy val __universe: universe.type = universe
}

