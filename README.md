XML String Interpolator for Scala (WIP)
=================================
Basic macro-based XML string interpolator for Scala.
Primarily intended as replacement for Scala XML Literals.

Usage
=================================

To try all the examples below, do `sbt console` and then import the required package.
```scala
import xmlquote._
```

To create regular Scala XML Nodes, import only the Scala XML Package
```scala
import ScalaXMLPackage._
```
To create DOM Nodes, 
```scala
import DOMPackage._
```

Note that importing both simultaneously will lead to ambiguous implicit values and the compiler won't know which kind of nodes to construct.

Since this package aims to replace existing XML literal support, it provides all the currently existing features as well. So, currently in Scala, something like
```scala
val name = "John"
val span = <span>{ name }</span>
val node = <div>{ span }</div>
node match {
  case <div><span> { inner }</span></div> => inner }
```
can now be more conveniently written as,
```scala
val name = "John"
val span = xml"<span>$name</span>"
val node = xml"<div>$span</div>"
node match {
  case xml"<div><span>$inner</span></div>" => inner
}
```
Also, pattern matching on attributes has been a pain in Scala for quite a while. This string interpolation syntax fixes this and makes it much easier to use.
Originally in Scala, something like,
```scala
<foo attr="someAttr"/> match {
  case <foo/> => println("matched foo")
  case <bar/> => println("matched bar")
  case _ => println("no match")
}
```
would result in the first expression getting matched although the attributes don't match. With the current syntax, this is no longer a problem. Also, matching on attributes is much easier. Consider, for example,
```scala
xml"""<foo attr="fooAttribute"><bar bttr="barAttribute">baz</bar></foo>""" match {
  case xml"""<foo attr="$x"><bar bttr="$y">baz</bar</foo>""" => true
  case _ => false
}
```
It matches the first case with *x* and *y* getting the appropriate values.
Check the test suite for more examples.

Since this XML interpolator is generic, we can construct DOM nodes directly as shown. For eg,
```scala
xml"<foo/>"
```
will result in the appropriate DOM node getting constructed, like,
`res1: org.w3c.dom.Node = [foo: null]`. (this feature is still experimental.)

To construct nodes/modify them, only the corresponding typeclass interfaces have to be modified. 

TODO
=============================================
1) Make error reporting more positioned.

2) fix $scope resolution in nested interpolators.

