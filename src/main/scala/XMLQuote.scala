import scala.Predef.{any2stringadd => _, _}
import scala.language.experimental.macros
import scala.reflect.macros._
import customContextLift._
import PatternLiftables.MacroPLiftables
import java.util.UUID.randomUUID
import scala.xml._
import org.w3c.dom._
import javax.xml.parsers._
import javax.xml.transform._

package object xmlquote {
  implicit class XMLQuote(val sc: StringContext) { 
    object xml {
      def apply[T](args: T*)(implicit ctx: XMLContext): ctx.Node = macro XMLQuoteImpl.apply
      def unapply(scrutinee: Any)(implicit ctx: XMLContext): Any = macro PMatch.unapply
    }
  }  
}

package xmlquote {
  object ScalaXMLPackage {
    implicit object ScalaXML extends XMLContext {
      type Node = scala.xml.Node
      type MetaData = scala.xml.MetaData
      type NamespaceBinding = scala.xml.NamespaceBinding
      
      type Text = scala.xml.Text
      val Text: TextCtor = ScalaTextCtor
      object ScalaTextCtor extends TextCtor {
        def apply(data: String) = new _root_.scala.xml.Text(data)
        def unapply(t: Any) = t match {
            case s: Text => Some(s.data)
            case _       => None
          }
      }

      type Comment = scala.xml.Comment
      val Comment: CommentCtor = ScalaCommentCtor
      object ScalaCommentCtor extends CommentCtor {
        def apply(comment: String) = new _root_.scala.xml.Comment(comment)
        def unapply(c: Any) = c match {
          case x: Comment => Some(x.commentText)
          case _          => None
        }
      }
      
      type EntityRef = scala.xml.EntityRef
      val EntityRef: EntityRefCtor = ScalaEntityRefCtor
      object ScalaEntityRefCtor extends EntityRefCtor {
        def apply(entityName: String) = new _root_.scala.xml.EntityRef(entityName)
        def unapply(e: Any) = e match {
          case er: EntityRef => Some(er.entityName)
          case _             => None
        }
      }

      type ProcInstr = scala.xml.ProcInstr
      val ProcInstr: ProcInstrCtor = ScalaProcInstrCtor
      object ScalaProcInstrCtor extends ProcInstrCtor {
        def apply(target: String, procText: String) = new _root_.scala.xml.ProcInstr(target, procText)
        def unapply(pi: Any) = pi match {
          case p: ProcInstr => Some((p.target, p.proctext))
          case _            => None
        }
      }

      type Unparsed = scala.xml.Unparsed
      val Unparsed: UnparsedCtor = ScalaUnparsedCtor
      object ScalaUnparsedCtor extends UnparsedCtor {
        def apply(data: String) = new _root_.scala.xml.Unparsed(data)
        def unapply(x: Any) = x match {
          case u: Unparsed => Some(u.data)
          case _           => None
        }
      }

      type PCData = scala.xml.PCData
      val PCData: PCDataCtor = ScalaPCDataCtor
      object ScalaPCDataCtor extends PCDataCtor {
        def apply(data: String) = new _root_.scala.xml.PCData(data)
        def unapply(other: Any) = other match {
          case x: PCData => Some(x.data)
          case _         => None
        }
      }

      type Elem = scala.xml.Elem
      val Elem: ElemCtor = ScalaElemCtor
      object ScalaElemCtor extends ElemCtor {
        def apply(prefix: String, label: String, attributes: xml.MetaData, scope: xml.NamespaceBinding, minimize: Boolean, child: Node*) = 
          new _root_.scala.xml.Elem(prefix, label, attributes, _root_.scala.xml.TopScope, minimize, child: _*)

        def unapplySeq(n: xml.Node) = {
          n match {
            case _: SpecialNode | _: Group  => None
            case _: xml.Elem                => Some((n.prefix, n.label, n.attributes, n.scope, n.child.isEmpty, n.child))
          }
        }
      }
    }
  }
  
  object DOMPackage {
    implicit object DOMNode extends XMLContext {
      type Node = org.w3c.dom.Node
      type MetaData = org.w3c.dom.NamedNodeMap
      type NamespaceBinding = xml.NamespaceBinding
      type NodeList = org.w3c.dom.NodeList

      val factory = DocumentBuilderFactory.newInstance()
      val builder = factory.newDocumentBuilder()
      val document = builder.newDocument()

      type Text = org.w3c.dom.Text
      val Text: TextCtor = DomTextCtor
      object DomTextCtor extends TextCtor {
        def apply(data: String) = document.createTextNode(data)
        def unapply(t: Any) = t match {
          case s: Text => Some(s.getNodeValue)
          case _       => None
        }
      }

      type Comment = org.w3c.dom.Comment
      val Comment: CommentCtor = DomCommentCtor
      object DomCommentCtor extends CommentCtor {
        def apply(comment: String) = document.createComment(comment)
        def unapply(c: Any) = c match {
          case x: Comment => Some(x.getNodeValue)
          case _          => None
        }
      }

      type EntityRef = org.w3c.dom.EntityReference
      val EntityRef: EntityRefCtor = DomEntityRefCtor
      object DomEntityRefCtor extends EntityRefCtor {
        def apply(entityName: String) = document.createEntityReference(entityName)
        def unapply(e: Any) = e match {
          case er: EntityRef => Some(er.getNodeName)
          case _             => None
        }
      }

      type ProcInstr = org.w3c.dom.ProcessingInstruction
      val ProcInstr: ProcInstrCtor = DomProcInstrCtor
      object DomProcInstrCtor extends ProcInstrCtor {
        def apply(target: String, procText: String) = document.createProcessingInstruction(target, procText)
        def unapply(p: Any) = p match {
          case pi: ProcInstr => Some((pi.getNodeName, pi.getNodeValue))
          case _             => None
        }
      }

      type Unparsed = org.w3c.dom.CDATASection
      val Unparsed: UnparsedCtor = DomUnparsedCtor
      object DomUnparsedCtor extends UnparsedCtor {
        def apply(data: String) = document.createCDATASection(data)     //check this part
        def unapply(x: Any) = x match {
          case u: Unparsed => Some(u.getNodeValue)
          case _           => None
        }
      }

      type PCData = org.w3c.dom.CDATASection
      val PCData: PCDataCtor = DomPCDataCtor
      object DomPCDataCtor extends PCDataCtor {
        def apply(data: String) = document.createCDATASection(data)     //check this part
        def unapply(x: Any) = x match {
          case pc: PCData => Some(pc.getNodeValue)
          case _          => None
        }
      }

      type Elem = org.w3c.dom.Element
      val Elem: ElemCtor = DomElemCtor
      object DomElemCtor extends ElemCtor {
        def apply(prefix: String, label: String, attributes: xml.MetaData, scope: NamespaceBinding, minimize: Boolean, child:
          scala.xml.Node*) = {
          document.createElement(label)
        }

        def unapplySeq(n: Node) = n match {
          case _: Node => Some((n.getPrefix, n.getNodeName, null, xml.TopScope, n.hasChildNodes, null)) //check this part
          case _       => None          
        }
      }
    }
  }
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


