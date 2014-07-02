package MyPackage

import scala.reflect.api.Universe
import java.util.UUID.randomUUID

trait MyPackageLiftables extends Nodes {
  protected val __universe: Universe
  import __universe._
  import __universe.internal.reificationSupport.{SyntacticBlock => SynBlock}

  val suid = Integer.parseInt(randomUUID().toString.replace("-","").substring(0,4), 16)

  object MyPackageHole{
    val pattern = java.util.regex.Pattern.compile("^x(\\d+)$")
    def apply(i: Int) = s"x$i"
    def unapply(node: xml.Node): Option[Int] = {
      val m = (pattern.matcher(node.toString))
      if (m.find()) Some(m.group(1).toInt) else None
    }
  }

  def MyLift(i: Int, dotted: Boolean = false) = {
    val x = TermName(s"x$i")
    pq"$x @ _"
  }

  implicit val liftComment = Liftable[xml.Comment] { 
    case MyPackageHole(i) => MyLift(i)
    case xml.Comment(commentText) => q"_root_.scala.xml.Comment(${commentText})"
  }

  implicit val liftText = Liftable[xml.Text] { 
    case MyPackageHole(i) => MyLift(i)
    case xml.Text(text) => q"_root_.scala.xml.Text(${text})"
  }

  implicit val liftEntityRef = Liftable[xml.EntityRef] { 
    case MyPackageHole(i) => MyLift(i)
    case xml.EntityRef(entityName) => q"_root_.scala.xml.EntityRef(${entityName})"
  }

  implicit val liftUnquote = Liftable[Unquote] { _.tree }

  implicit val liftProcInstr = Liftable[xml.ProcInstr] { 
    case MyPackageHole(i) => MyLift(i)
    case xml.ProcInstr(target, proctext) => q"_root_.scala.xml.ProcInstr(${target}, ${proctext})"
  }

  implicit val liftUnparsed = Liftable[xml.Unparsed] { 
    case MyPackageHole(i) => MyLift(i)
    case xml.Unparsed(data) => q"_root_.scala.xml.Unparsed(${data})"
  }

  implicit val liftPCData = Liftable[xml.PCData] {
    case MyPackageHole(i) => MyLift(i)
    case xml.PCData(data) => q"_root_.scala.xml.PCData(${data})"
  }

  implicit def liftElem(implicit outer: xml.NamespaceBinding = xml.TopScope) = Liftable[xml.Elem] { 
    case MyPackageHole(i) => MyLift(i)
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
        case xml.UnprefixedAttribute(key, MyPackageHole(i), rest) => (Nil, q"""_root_.scala.xml.UnprefixedAttribute($key, ${MyLift(i)},
          ${MyLift(i+suid)})""")
        case xml.PrefixedAttribute(pre, key, MyPackageHole(i), rest) => (Nil, q"""_root_.scala.xml.PrefixedAttribute($pre, $key,
          ${MyLift(i)}, ${MyLift(i+suid)})""")
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
        _root_.scala.xml.Elem(${elem.prefix}, ${elem.label}, ${metaval}, $$scope, ..$children)
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
