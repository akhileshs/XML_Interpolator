package MyPackage
import scala.reflect.api.Universe
import reflect.macros.whitebox.Context
import java.util.UUID.randomUUID

trait PLiftables extends MyPackageLiftables{
  protected val __universe: Universe
  import __universe._
  import __universe.internal.reificationSupport.{SyntacticBlock => SynBlock}
  val sid = Integer.parseInt(randomUUID().toString.replace("-","").substring(0,4), 16)

  object LiftHole{
    val pattern = java.util.regex.Pattern.compile("^x(\\d+)$")
    def apply(i: Int) = s"x$i"
    def unapply(node: xml.Node): Option[Int] = {
      val m = (pattern.matcher(node.toString))
      if (m.find()) Some(m.group(1).toInt) else None
    }    
  }

  def argLift(i: Int, dotted: Boolean = false) = {
    val x = TermName(s"x$i")
    pq"$x @ _"
  }

  implicit val liftCommentu = Liftable[xml.Comment]{
    case xml.Comment(commentText) => pq"_root_.scala.xml.Comment($commentText)"
  }

  implicit def liftTextu  = Liftable[xml.Text]{
    case xml.Text(text) => pq"_root_.scala.xml.Text(${text})"
  }

  implicit val liftEntityRefu = Liftable[xml.EntityRef]{ 
    case xml.EntityRef(entityName) => pq"_root_.scala.xml.EntityRef($entityName)"
  }

  implicit val liftUnquoteu = Liftable[Unquote]{ _.tree }

  implicit val liftProcInstru = Liftable[xml.ProcInstr]{ 
    case xml.ProcInstr(target, proctext) => pq"_root_.scala.xml.ProcInstr($target, $proctext)"
  }

  implicit val liftUnparsedu = Liftable[xml.Unparsed] {
    case xml.Unparsed(data) => pq"_root_.scala.xml.Unparsed($data)"
  }

  implicit val liftPCDatau = Liftable[xml.PCData]{ 
    case xml.PCData(data) => pq"_root_.scala.xml.PCData($data)"
  }

  implicit def liftElemu(implicit outer: xml.NamespaceBinding = xml.TopScope) = Liftable[xml.Elem]{ elem =>
    def liftMetadata(meta: xml.MetaData): List[Tree] = meta match{
      case xml.Null =>
        q"var $$md: _root_.scala.xml.MetaData = _root_.scala.xml.Null" :: Nil
      case xml.UnprefixedAttribute(key, Seq(value), rest) =>
        q"$$md = new _root_.scala.xml.UnprefixedAttribute($key,$$md)" :: liftMetadata(rest)
      case xml.PrefixedAttribute(pre, key, Seq(value), rest) =>
        q"$$md = new _root_.scala.xml.PrefixedAttribute($pre, $key, $$md)" :: liftMetadata(rest)
    }

    val (metaPrevious, metaValue) = elem.attributes match {
      case xml.Null => (Nil, q"_root_.scala.xml.Null")
      case xml.UnprefixedAttribute(key, LiftHole(i), rest) => (Nil, q"""_root_.scala.xml.UnprefixedAttribute($key, ${argLift(i)},
        ${argLift(sid)})""")
      case xml.PrefixedAttribute(pre, key, LiftHole(i), rest) => (Nil, q"""_root_.scala.xml.PrefixedAttribute($pre, $key, ${argLift(i)},
        ${argLift(sid)})""")
      case _ => (liftMetadata(elem.attributes).reverse, q"")
    }
   
    val children = 
      if (elem.child.isEmpty) q""
      else{
        val outer = 'shadowed
        implicit val current: xml.NamespaceBinding = elem.scope
      
        def add(elem: xml.Node): Seq[Tree] = elem.child.map{ node => q"$node" }
        val additions = add(elem)

        q"..$additions"        
      }

    def scoped(tree: Tree) = {
      def distinct(ns: xml.NamespaceBinding): List[(String, String)] =
        if (ns == null || ns.eq(outer) || ns.eq(xml.TopScope)) Nil
        else{
          val xml.NamespaceBinding(pre, uri, innerns) = ns
          (pre, uri) :: distinct(innerns)
        }

        val bindings = distinct(elem.scope)
        if(bindings.isEmpty) pq"null"
        else{
          val q"..$stats" = tree
          val scopes = bindings.reverse.map{ case (pre, uri) =>
            q"$$tmpscope = new _root_.scala.xml.NamespaceBinding($pre, $uri, $$tmpscope)"
          }
        }
        
      pq"""_root_.scala.xml.Elem(_, ${elem.label}, ${metaValue}, _, ..$children)"""        
    }

    scoped(q"""        
         new _root_.scala.xml.Elem(${elem.prefix}, ${elem.label}, ${metaValue}, $$scope, ${elem.minimizeEmpty}, ..$children)
        """)
  }

    implicit val liftAtomu = Liftable[xml.Atom[String]]{
      case pcdata: xml.PCData => liftPCDatau(pcdata)
      case text: xml.Text => liftTextu(text)
      case unparsed: xml.Unparsed => liftUnparsedu(unparsed)
    }

    implicit val liftSpecialNodeu = Liftable[xml.SpecialNode]{
      case atom: xml.Atom[String] => liftAtomu(atom)
      case comment: xml.Comment => liftCommentu(comment)
      case procinstr: xml.ProcInstr => liftProcInstru(procinstr)
      case entityref: xml.EntityRef => liftEntityRefu(entityref)
      case unquote: Unquote => liftUnquoteu(unquote)
    }

    implicit def liftNodeu(implicit outer: xml.NamespaceBinding = xml.TopScope) = Liftable[xml.Node]{
      case elem: xml.Elem => liftElemu(outer)(elem)
      case snode: xml.SpecialNode => liftSpecialNodeu(snode)
    }
}









