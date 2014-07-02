import xmlquote._
import reflect.runtime.universe._
object Test extends App{
  //simple XML element
  val t = xml"<foo>bar</foo>"
  //unquote
  val x = xml"<baz/>"
  val i = 2
  val t1 = xml"<foo>$x</foo>"
  val t2 = xml"<foo>bar<baz/>$i</foo>"
  //simple pattern match
  val xml"<foo>bar</foo>" = xml"<foo>bar</foo>"
  //simple interpolated capture
  val xml"<foo><bar>$y6</bar></foo>" = xml"<foo><bar>2</bar></foo>"
  val xml"<foo>$x1</foo>" = xml"<foo>bar</foo>"
  val xml"<foo>$y</foo>" = xml"<foo><bar/></foo>"
  //nested interpolated captures
  val xml"<foo><bar>$x3</bar></foo>" = xml"<foo><bar><baz>someText</baz></bar></foo>"
  //interpolated attribute capture
  val xml"""<foo attr="$x4">bar</foo>""" = xml"""<foo attr="someString">bar</foo>"""
  val xml"""<foo fooattr="$x5">2<bar barattr="$y5"/></foo>""" = xml"""<foo fooattr="fooString">2<bar barattr="barString"/></foo>"""




}
