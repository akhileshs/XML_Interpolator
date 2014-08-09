package customContextLift
import xml._
import scala.reflect.api.Universe
import reflect.macros.blackbox.Context

trait XMLContext {
  type Node
  
  type Text <: Node
  val Text: TextCtor
  trait TextCtor {
    def apply(data: String): Text
    def unapply(t: Any): Option[String]
  }

  type Comment <: Node
  val Comment: CommentCtor
  trait CommentCtor {
    def apply(comment: String): Comment
    def unapply(c: Any): Option[String]
  }

  type EntityRef <: Node
  val EntityRef: EntityRefCtor
  trait EntityRefCtor {
    def apply(entityName: String): EntityRef
    def unapply(e: Any): Option[String]
  }

  type ProcInstr <: Node
  val ProcInstr: ProcInstrCtor
  trait ProcInstrCtor {
    def apply(target: String, procText: String): ProcInstr
    def unapply(pi: Any): Option[(String, String)]
  }

  type Unparsed <: Node
  val Unparsed: UnparsedCtor
  trait UnparsedCtor {
    def apply(data: String): Unparsed
    def unapply(x: Any): Option[String]
  }

  type PCData <: Node
  val PCData: PCDataCtor
  trait PCDataCtor {
    def apply(data: String): PCData
    def unapply(other: Any): Option[String]
  }

  type Elem <: Node
  val Elem: ElemCtor
  trait ElemCtor {
    def apply(prefix: String, label: String, attributes: xml.MetaData, scope: NamespaceBinding, minimize: Boolean, child: Node*): Node
    def unapplySeq(n: Node): Option[(String, String, xml.MetaData, xml.NamespaceBinding, Boolean, Seq[scala.xml.Node])]
  }
}

object XMLContext {
  implicit object ScalaXML extends XMLContext {
    type Node = scala.xml.Node
    
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
      def apply(prefix: String, label: String, attributes: xml.MetaData, scope: xml.NamespaceBinding, minimize: Boolean, child: Node*) = new
      _root_.scala.xml.Elem(prefix, label, attributes, _root_.scala.xml.TopScope, minimize, child: _*)
      def unapplySeq(n: xml.Node) = {
        n match {
          case _: SpecialNode | _: Group  => None
          case _: xml.Elem                => Some((n.prefix, n.label, n.attributes, n.scope, n.child.isEmpty, n.child))
        }
      }
    }
  }
}


trait Liftables extends Nodes {

  protected val __universe: Universe
  import __universe._
  import __universe.internal.reificationSupport.{SyntacticBlock => SynBlock}

  val context: Tree

  implicit val liftComment = Liftable[xml.Comment] { c =>
    q"$context.Comment(${c.commentText})"
  }

  implicit val liftText = Liftable[xml.Text] { t =>
    q"$context.Text(${t.text})"
  }

  implicit val liftEntityRef = Liftable[xml.EntityRef] { er =>
    q"$context.EntityRef(${er.entityName})"
  }

  implicit val liftUnquote = Liftable[Unquote] { _.tree }

  implicit val liftProcInstr = Liftable[xml.ProcInstr] { pi =>
    q"$context.ProcInstr(${pi.target}, ${pi.proctext})" 
  }

  implicit val liftUnparsed = Liftable[xml.Unparsed] { u =>
    q"$context.Unparsed(${u.data})"
  }

  implicit val liftPCData = Liftable[xml.PCData] { pcd =>
    q"$context.PCData(${pcd.data})"
  }

  implicit def liftElem(implicit outer: xml.NamespaceBinding = xml.TopScope) = Liftable[xml.Elem] { elem =>
    def liftMeta(meta: xml.MetaData): List[Tree] = meta match {
      case xml.Null =>
        q"var $$md: _root_.scala.xml.MetaData = _root_.scala.xml.Null" :: Nil
      case xml.UnprefixedAttribute(key, Seq(value), rest) =>
        q"$$md = new _root_.scala.xml.UnprefixedAttribute($key, $value, $$md)" :: liftMeta(rest)
      case xml.PrefixedAttribute(pre, key, Seq(value), rest) =>
        q"$$md = new _root_.scala.xml.PrefixedAttribute($pre, $key, $value, $$md)" :: liftMeta(rest)
    }

    val (metapre, metaval) =
      if (elem.attributes.isEmpty) (Nil, q"_root_.scala.xml.Null")
      else (liftMeta(elem.attributes).reverse, q"$$md")

    val children =
      if (elem.child.isEmpty) q""
      else {
        val outer = 'shadowed
        implicit val current: xml.NamespaceBinding = elem.scope
        val additions = elem.child.map { node => q"$$buf &+ $node" }
        q"""{
          val $$buf = new _root_.scala.xml.NodeBuffer
          ..$additions
          $$buf
        }: _*"""
      }

    def scoped(tree: Tree) = {
      def distinct(ns: xml.NamespaceBinding): List[(String, String)] =
        if (ns == null || ns.eq(outer) || ns.eq(xml.TopScope)) Nil
        else {
          val xml.NamespaceBinding(pre, uri, innerns) = ns
          (pre, uri) :: distinct(innerns)
        }

      val bindings = distinct(elem.scope)
      if (bindings.isEmpty) tree
      else {
        val q"..$stats" = tree
        val scopes = bindings.reverse.map { case (pre, uri) =>
          q"$$tmpscope = new _root_.scala.xml.NamespaceBinding($pre, $uri, $$tmpscope)"
        }
        q"""
          var $$tmpscope: _root_.scala.xml.NamespaceBinding = $$scope
          ..$scopes
          ${SynBlock(q"val $$scope: _root_.scala.xml.NamespaceBinding = $$tmpscope" :: stats)}
        """
      }
    }

    scoped(q"""
      ..$metapre
      $context.Elem(${elem.prefix}, ${elem.label}, $metaval, $$scope, ${elem.minimizeEmpty}, ..$children)
    """)
  }

  // TODO: what to do with Atom[T]?
  implicit val liftAtom = Liftable[xml.Atom[String]] {
    case pcdata:   xml.PCData   => liftPCData(pcdata)
    case text:     xml.Text     => liftText(text)
    case unparsed: xml.Unparsed => liftUnparsed(unparsed)
  }

  implicit val liftSpecialNode = Liftable[xml.SpecialNode] {
    case atom:      xml.Atom[String] => liftAtom(atom)
    case comment:   xml.Comment      => liftComment(comment)
    case procinstr: xml.ProcInstr    => liftProcInstr(procinstr)
    case entityref: xml.EntityRef    => liftEntityRef(entityref)
    case unquote:   Unquote          => liftUnquote(unquote)
  }

  implicit def liftNode(implicit outer: xml.NamespaceBinding = xml.TopScope): Liftable[xml.Node] = Liftable[xml.Node] {
    case elem:  xml.Elem         => liftElem(outer)(elem)
    case snode: xml.SpecialNode  => liftSpecialNode(snode)
  }
}
