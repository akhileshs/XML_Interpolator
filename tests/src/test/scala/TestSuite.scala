import org.scalatest.FunSuite
import xmlquote._
import reflect.runtime.universe._
import org.scalamacros.xml.RuntimeLiftables._

class ConstructionSuite extends FunSuite {

  test("reconstruct comment") {
    assert(xml"<wrapNode><!--foo--></wrapNode>" == <wrapNode/>)
  }
  
  test("reconstruct text") {
    assert(xml"<wrapNode><![CDATA[foo]]></wrapNode>" == <wrapNode><![CDATA[foo]]></wrapNode>)
  }
  
  test("reconstruct entity ref") {
    assert(xml"<foo>&#038;</foo>" == <foo>&#038;</foo>)
  }

  test("reconstruct parameter entity ref"){
    assert(xml"<foo>%param;</foo>" == <foo>%param;</foo>)
  } 

  test("reconstruct proc instr") {
    assert(xml"<foo><?foo bar?></foo>" == <foo><?foo bar?></foo>)
  }

  test("reconstruct unparsed") {
    assert(xml"<wrapNode><![CDATA[foo]]></wrapNode>" == <wrapNode><xml:unparsed>foo</xml:unparsed></wrapNode>)
  }

  test("reconstruct minimized elem") {
    assert(xml"<foo/>" == <foo/>)
  }

  test("reconstruct maximized elem") {
    assert(xml"<foo></foo>" == <foo></foo>)
  }

  test("reconstruct prefixed elem") {
    assert(xml"<foo:bar/>" == <foo:bar/>)
  }

  test("reconstruct nested elem") {
    assert(xml"<foo><bar/></foo>" == <foo><bar/></foo>)
  }

  test("reconstruct elem with unprefixed attributes") {
    assert(xml"""<foo a="a" b="b"/>""" == <foo a="a" b="b"/>)
  }

  test("reconstruct elem with prefixed attributes") {
    assert(xml"""<foo a:a="a" b:b="b"/>""" == <foo a:a="a" b:b="b"/>)
  }

  test("reconstruct unquote within elem") {
    assert(xml"<foo>${2 + 3}</foo>" == <foo>{2 + 3}</foo>)
  }

  test("reconstruct unquote within unprefixed attribute") {
    assert(xml"<foo a=${"foo" + "bar"}/>" == <foo a={"foo" + "bar"}/>)
  }

  test("reconstruct unquote within prefixed attribute") {
    assert(xml"<foo a:b=${"foo" + "bar"}/>" == <foo a:b={"foo" + "bar"}/>)
  }

  test("reconstruct namespaced elem") {
    assert(xml"""<foo xmlns:pre="uri"/>""" == <foo xmlns:pre="uri"/>)
  }

  test("reconstruct multi-namespaced elem") {
    assert(xml"""<foo xmlns:a="uri1" xmlns:b="uri2"/>""" == <foo xmlns:a="uri1" xmlns:b="uri2"/>)
  }

  test("reconstruct nested namespaced elem") {
    assert(xml"""<foo xmlns:pre1="uri1"><bar xmlns:pre2="uri2"/></foo>""" == <foo xmlns:pre1="uri1"><bar xmlns:pre2="uri2"/></foo>)
  }

  test("reconstruct shadowed namespaced elem") {
    assert(xml"""<foo xmlns:pre="a"><bar xmlns:pre="b"/></foo>""" == <foo xmlns:pre="a"><bar xmlns:pre="b"/></foo>)
  }

  test("basic pattern matching"){
    val xml"""<foo attr="someAttr"><baz>bar</baz></foo>""" = <foo attr="someAttr"><baz>bar</baz></foo>
  }

  test("simple interpolated capture"){
    val xml"<foo>$x</foo>" = <foo>bar</foo>
  }

  test("simple nested interpolated capture"){
    val xml"<foo><bar>$x</bar></foo>" = <foo><bar>baz</bar></foo>
  }

  test("element capture"){
    val xml"<foo>$x<baz>$y</baz></foo>" = <foo><bar/><baz><trialNode>2</trialNode></baz></foo>
  }

  test("simple attribute capture"){
    val xml"""<foo attr="$x">bar</foo>""" = <foo attr="someString">bar</foo>
  }

  test("multiple/nested attribute capture"){
    val xml"""<foo attr="$x">bar<baz bazAttr="$y"/></foo>""" = <foo attr="trialText">bar<baz bazAttr="bazText"/></foo>
  }
}

  
