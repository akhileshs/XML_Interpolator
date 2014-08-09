package PatternLiftables

import scala.reflect.api.Universe
import java.util.UUID.randomUUID

trait PatternPackageLiftables extends Nodes {
  protected val __universe: Universe
  import __universe._
  import __universe.internal.reificationSupport.{SyntacticBlock => SynBlock}

  val context: Tree

  val suid = Integer.parseInt(randomUUID().toString.replace("-","").substring(0,4), 16)

  object PatternPackageHole{
    val pattern = java.util.regex.Pattern.compile("^x(\\d+)$")
    def apply(i: Int) = s"x$i"
    def unapply(node: xml.Node): Option[Int] = {
      val m = (pattern.matcher(node.toString))
      if (m.find()) Some(m.group(1).toInt) else None
    }
  }

  def PatternLift(i: Int, dotted: Boolean = false) = {
    val x = TermName(s"x$i")
    pq"$x @ _"
  }

  implicit val liftComment = Liftable[xml.Comment] { 
    case PatternPackageHole(i) => PatternLift(i)
    case xml.Comment(commentText) => q"$context.Comment(${commentText})"
  }

  implicit val liftText = Liftable[xml.Text] { 
    case PatternPackageHole(i) => PatternLift(i)
    case xml.Text(text) => q"$context.Text($text)"
  }

  implicit val liftEntityRef = Liftable[xml.EntityRef] { 
    case PatternPackageHole(i) => PatternLift(i)
    case xml.EntityRef(entityName) => q"$context.EntityRef(${entityName})"
  }

  implicit val liftUnquote = Liftable[Unquote] { _.tree }

  implicit val liftProcInstr = Liftable[xml.ProcInstr] { 
    case PatternPackageHole(i) => PatternLift(i)
    case xml.ProcInstr(target, proctext) => q"$context.ProcInstr(${target}, ${proctext})"
  }

  implicit val liftUnparsed = Liftable[xml.Unparsed] { 
    case PatternPackageHole(i) => PatternLift(i)
    case xml.Unparsed(data) => q"$context.Unparsed(${data})"
  }

  implicit val liftPCData = Liftable[xml.PCData] {
    case PatternPackageHole(i) => PatternLift(i)
    case xml.PCData(data) => q"$context.PCData(${data})"
  }

  implicit def liftElem(implicit outer: xml.NamespaceBinding = xml.TopScope) = Liftable[xml.Elem] { 
    case PatternPackageHole(i) => PatternLift(i)
    case elem: xml.Elem =>
      def liftMeta(meta: xml.MetaData): List[Tree] = meta match {
        case xml.Null =>
          q"var $$md: _root_.scala.xml.MetaData = _root_.scala.xml.Null" :: Nil
        case xml.UnprefixedAttribute(key, Seq(value), rest) =>
          q"$$md = new _root_.scala.xml.UnprefixedAttribute($key, $value, $$md)" :: liftMeta(rest)
        case xml.PrefixedAttribute(pre, key, Seq(value), rest) =>
          q"$$md = new _root_.scala.xml.PrefixedAttribute($pre, $key, $value, $$md)" :: liftMeta(rest)
      }

      val (metapre, metaval) = elem.attributes match{
        case xml.Null => (Nil, q"_root_.scala.xml.Null")
        case xml.UnprefixedAttribute(key, PatternPackageHole(i), rest) => (Nil, q"""_root_.scala.xml.UnprefixedAttribute($key, ${PatternLift(i)},
          ${PatternLift(i+suid)})""")
        case xml.PrefixedAttribute(pre, key, PatternPackageHole(i), rest) => (Nil, q"""_root_.scala.xml.PrefixedAttribute($pre, $key,
          ${PatternLift(i)}, ${PatternLift(i+suid)})""")
        case _ => (liftMeta(elem.attributes).reverse, q"$$md")
      }

      val children =
        if (elem.child.isEmpty) q""
        else {
          val outer = 'shadowed
          implicit val current: xml.NamespaceBinding = elem.scope
          val additions = elem.child.map { node => q"$node" }
          q"""..$additions"""
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
        $context.Elem(${elem.prefix}, ${elem.label}, ${metaval}, $$scope, ${elem.child.isEmpty}, ..$children)
      """)
  }

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
