package customContextLift
import xml._
import scala.reflect.api.Universe
import reflect.macros.blackbox.Context


//basic typeclass structure
trait XMLContext {
  type Node
  type MetaData
  type NamespaceBinding
  
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
    def apply(prefix: String, label: String, attributes: xml.MetaData, scope: NamespaceBinding, minimize: Boolean, child: scala.xml.Node*): Node
    def unapplySeq(n: Node): Option[(String, String, xml.MetaData, NamespaceBinding, Boolean, Seq[Node])]
  }
}

//generate the right kind of tree for each node. (check Denys' guide on Lifting/Unlifting for quasiquotes
//(http://docs.scala-lang.org/overviews/quasiquotes/lifting.html)

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
