import scala.Predef.{any2stringadd => _, _}
import scala.language.experimental.macros
import scala.reflect.macros._
import customContextLift._
import PatternLiftables.MacroPLiftables
import java.util.UUID.randomUUID
import scala.xml._

package object xmlquote { 
  implicit class XMLQuote(val sc: StringContext) { 
    object xml {
      def apply[T](args: T*)(implicit ctx: XMLContext): ctx.Node = macro XMLQuoteImpl.apply
      def unapply(scrutinee: Any)(implicit ctx: XMLContext): Any = macro PMatch.unapply
    }
  }  
}

package xmlquote {
  private[xmlquote] class XMLQuoteImpl(val c: blackbox.Context) extends MacroLiftables { import c.universe._   
    
    lazy val sessionSuffix = randomUUID().toString.replace("-", "").substring(0, 8)
    lazy val q"$_($_(..${parts: List[String]})).xml.apply[..$_](..$args)($ctx)" = c.macroApplication

    val context = c.inferImplicitValue(typeOf[XMLContext], silent = true)

    type Lifted = (List[Tree], Tree)

    implicit def liftNodes[U <: xml.Node]: Liftable[List[U]] = Liftable { nodes =>
      def prepend(nodes: List[xml.Node], t: Tree) = 
        nodes.foldRight(t) { case (n, acc) => q"$n :: $acc" }
      def append(t: Tree, nodes: List[xml.Node]) = 
        nodes.foldLeft(t) { case (acc, n) => q"$acc :+ $n" }

      q"" //fix this part for splicing nested elements using .. syntax to get stuff like 
          //xml"<foo>..$xs</foo>" => xml"<foo><bar/><baz/></foo>; xs = List(<bar/>, <baz/>) (if its really needed!)
    }

    lazy val (xmlString, keyMap) = {
      val sb = new StringBuilder
      var keyMap = Map.empty[String, c.Tree]    
      var i = 0
      args.zip(parts.init).foreach { case (arg, part) =>
        sb.append(part)
        val key1 = sessionSuffix + i
        i += 1
        val key2 = "\"" + key1 + "\""
        sb.append(key2)
        keyMap += key1 -> arg
        keyMap += key2 -> arg
      }
      sb.append(parts.last)
      (sb.toString, keyMap)
    }
 
    def transformText(text: xml.Text) = {
      val s = text.text
      val regex = "(?<=\"" + sessionSuffix + "\\d{1,3}\")|(?=\"" + sessionSuffix + "\\d{1,3}\")"
      val splitted = s.split(regex).toList
      val subs = splitted.collect {
        case part if keyMap contains part => {          
          Unquote(keyMap(part))
        }     
        case part if part.nonEmpty => {
          xml.Text(part)
        }
      }
      if (subs.length == 1) subs.head
      else subs
    }  

    def transformMetaData(md: xml.MetaData): xml.MetaData = md match {
      case xml.UnprefixedAttribute(namespace, value: xml.Text, rest) =>
        new xml.UnprefixedAttribute(namespace, transformText(value), transformMetaData(rest))
      case xml.PrefixedAttribute(pre, namespace, value: xml.Text, rest) =>
        new xml.PrefixedAttribute(pre, namespace, transformText(value), transformMetaData(rest))
      case xml.Null => xml.Null
    }

    def transformNode[U <: xml.Node](n: U): Seq[xml.Node] = n match {
      case text: xml.Text if text.text.contains(sessionSuffix) =>
        transformText(text)
      case elem: xml.Elem =>
        elem.copy(attributes = transformMetaData(elem.attributes),
                  child = elem.child.flatMap(transformNode))
      case  _ => n
    }
    
    def liftApply(node: xml.Node): Lifted = {   
      (Nil, q"val $$scope = _root_.scala.xml.TopScope; $node")
    }

    def wrap(node: xml.Node): Tree = {
      val (preamble, lifted) = liftApply(node)
      q"..$preamble; $lifted"
    }

    val parsed = xml.XML.loadString(xmlString)    
    val transformed = transformNode(parsed).head    
    def apply(args: Tree*)(ctx: Tree) = wrap(transformed)
  }

  private[xmlquote] object Hole{
    val pattern = java.util.regex.Pattern.compile("^x(\\d)$")
    def apply(i: Int) = s"x$i"
    def unapply(s: String): Option[Int] = {
      val m = pattern.matcher(s)
      if (m.find()) Some(m.group(1).toInt) else None
    }
  }

  private[xmlquote] class PMatch(val c: whitebox.Context) extends MacroPLiftables{ import c.universe._
    lazy val q"$_($_(..${parts: List[String]})).xml.unapply(..$args)($ctx)" = c.macroApplication  

    val context = c.inferImplicitValue(typeOf[XMLContext], silent = true)

    def code() = 
      parts.init.zipWithIndex.map { case (part, i) =>
        s"$part${(Hole(i))}"
      }.mkString("", "", parts.last)

    def wrap(node: xml.Node): Tree = {
      val (thenp, elsep) = 
        if (parts.length == 1) (q"true", q"false")
        else {
          val xs = parts.init.zipWithIndex.map{ case (_, i) => val x = TermName(s"x$i"); q"$x" }
          (q"_root_.scala.Some((..$xs))", q"_root_.scala.None")
        }   

      q"""
      new{
        val $$scope = _root_.scala.xml.TopScope
        def unapply(input: $ctx.Node) = {
          input match{
            case $node => $thenp
            case _ => $elsep
          }
        }
      }.unapply(..$args)
      """
    }
    
    def expand = wrap(xml.XML.loadString(code()))    
    def unapply(scrutinee: Tree)(ctx: Tree) = expand
  }
}


