import scala.Predef.{any2stringadd => _, _}
import scala.language.experimental.macros
import scala.reflect.macros._
import org.scalamacros.xml.MacroLiftables
import MyPackage.MacroPLiftables
import java.util.UUID.randomUUID

package object xmlquote {
  implicit class XMLQuote(val ctx: StringContext) {
    object xml {
      def apply[T](args: T*): scala.xml.Node = macro XMLQuoteImpl.apply
      def unapply(scrutinee: Any): Any = macro PMatch.unapply     
    }
  }  
}

package xmlquote {
  object ` :+ ` {
    def apply[T](elems: List[T], elem: T): List[T] = elems :+ elem
    def unapply[T](elems: List[T]): Option[(List[T], T)] = :+.unapply(elems)
  }

  private[xmlquote] class XMLQuoteImpl(val c: blackbox.Context) extends MacroLiftables { import c.universe._    
    
    lazy val sessionSuffix = randomUUID().toString.replace("-", "").substring(0, 8)
    lazy val q"$_($_(..${parts: List[String]})).xml.apply[..$_](..$args)" = c.macroApplication

    lazy val IterableClass: TypeSymbol = 
      typeOf[Iterable[_]].typeSymbol.asType

    lazy val IterableTParam: Type = 
      IterableClass.typeParams(0).asType.toType
  
    def iterableT(tpe: Type): Type = 
      IterableTParam.asSeenFrom(tpe, IterableClass)

    type Lifted = (List[Tree], Tree)    

    def arg(i: Int, dotted: Boolean = false): Tree = {     
      val arg = args(i)
      val tpe = if (!dotted) arg.tpe else iterableT(arg.tpe)
      val subst: Tree => Tree =
        if (tpe <:< typeOf[xml.Node]) identity
        else {
          val LiftT = appliedType(typeOf[xml.Node], tpe)
          val lift = c.inferImplicitValue(LiftT, silent = true)
          if (lift.nonEmpty) t => q"$lift($arg)"
          else c.abort(arg.pos, s"couldn't find implicit value of type Lift[$tpe]")
        }
      if (!dotted) subst(arg)
      else{
        val x = TermName(c.freshName())
        q"$arg.map { ($x: $tpe) => ${subst(q"$x")} }.toList"
      }
    }
/*
    implicit def liftNodes: Liftable[List[xml.Node]] = Liftable{ nodes =>
      def prepend(nodes: List[xml.Node], t: Tree) =
        nodes.foldRight(t) { case(n, acc) => q"$n :: $acc" }
        
      def append(t: Tree, nodes: List[xml.Node]) = 
        nodes.foldLeft(t) { case (acc, n) => q"$acc :+ $n" }

      val (pre, middle) = nodes.span(_.text != "..")
      
      middle match{
        case Nil =>
          prepend(pre, q"$Nil")
        case xml.Text("..") :: xml.Text(Hole(i)) :: rest =>
          append(prepend(pre, arg(i, dotted = true)), rest)
        case _ =>
          c.abort(c.enclosingPosition, "wrong usage of ..")
      }      
    }  
*/
    def liftApply(node: xml.Node): Lifted = (Nil, q"val $$scope = _root_.scala.xml.TopScope; $node")

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

    def wrap(node: xml.Node): Tree = {
      val (preamble, lifted) = liftApply(node)
      q"..$preamble; $lifted"
    }

    val parsed = xml.XML.loadString(xmlString)
    val transformed = transformNode(parsed).head
    def apply(args: Tree*) = wrap(transformed)
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
    lazy val q"$_($_(..${parts: List[String]})).xml.unapply(..$args)" = c.macroApplication
   
    lazy val IterableClass: TypeSymbol =
      typeOf[Iterable[_]].typeSymbol.asType

    lazy val IterableTParam: Type = 
      IterableClass.typeParams(0).asType.toType

    def code() = 
      parts.init.zipWithIndex.map { case (part, i) =>
        s"$part${(Hole(i))}"
      }.mkString("", "", parts.last)

    def iterableT(tpe: Type): Type = 
      IterableTParam.asSeenFrom(tpe, IterableClass)

    type Lifted = (List[Tree], Tree)

    def arg(i: Int, dotted: Boolean = false): Tree = {
      val x = TermName(s"x$i")
      val subpattern = c.internal.subpatterns(args.head).get.apply(i)
      subpattern match{
        case pq"_: $tpt" =>
          val typed = c.typecheck(tpt, c.TYPEmode)
          val tpe = if (!dotted) typed.tpe else iterableT(typed.tpe)
          val unlift = c.inferImplicitValue(appliedType(typeOf[xml.Node], tpe), silent = true)

          if (unlift.isEmpty)
            c.abort(c.enclosingPosition,
              s"couldn't find implicit value of type Unlift[$tpe]")
          else if (!dotted)  pq"$unlift($x @ _)"
          else {
            val name = TermName(c.freshName("unlift"))
            pq"$name($x @ _)"
          }
        case _ => pq"$x @ _"
      }
    }

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
        def unapply(input: xml.Node) = {
          input match{
            case $node => $thenp
            case _ => $elsep
          }
        }
      }.unapply(..$args)
      """
    }
    
    def expand = wrap(xml.XML.loadString(code()))    
    def unapply(scrutinee: Tree) = expand
  }
}


