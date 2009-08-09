import net.htmlparser.jericho._
import scala.collection.mutable._
import scala.collection.jcl.Conversions._
import scala.util.matching._
import java.io._
 
class Dir(pathname: String) {
 
  val path = new File(pathname)
 
  def glob(pattern: Regex): Array[File] = {
    val result = new ArrayBuffer[File]
    for(file <- path.listFiles){
      if( file.isDirectory ){
        result ++= new Dir(file.getPath).glob(pattern)
      }
      else{
        pattern.findFirstIn(file.getName) match {
          case Some(s) => result += file
          case _ =>
        }
      }
    }
    result.toArray
  }
}

class MethodInfo( val name:String, val url:String ){
  override def toString:String = "method_name: " + name  + "; " + "method_url: " + url
}

case class ClazzType
case class ClassType extends ClazzType
case class ObjectType extends ClazzType 
case class TraitType extends ClazzType
case class UnknownType extends ClazzType

case class PkgInfo( val name:String, url:String ) {

  override def toString:String = {
    return name + "; url = " + url
  }
}

class ClassInfo( val name:String, val url:String, val pkg:PkgInfo, val methods:List[MethodInfo], val class_type:ClazzType ){
  override def toString:String = {
       "name: " + name + "\n"  +
       "url: " + url + "\n"  +
       "pkg: " + pkg.toString + "\n" +
       "methods: " + methods.mkString("\n") + "\n" +
       "class_type: " + class_type 
  }
}

def parsePkgName( src:Source ):Option[String] = {
  val elements = src.getAllElements("class", "entity", false)
  for( elem <- elements ){
    val a = elem.getFirstElement(HTMLElementName.A)
    if( a != null ){
      return Some( a.getTextExtractor.toString.replaceAll("\\.[A-Z].*", "") )
    }
  }

  return None
}

def parseClassName( src:Source ):Option[(String, ClazzType)] = {
  val elements = src.getAllElements("class", "entity", false)
  for( elem <- elements ){
    val a = elem.getFirstElement(HTMLElementName.A)
    if( a == null ){
      "(.*)\\s+(.*)".r.findFirstMatchIn(elem.getTextExtractor.toString) match {
        case Some(m) => 
            val class_type:ClazzType = m.group(1) match {
              case "object" => ObjectType()
              case "class" => ClassType()
              case "trait" =>  TraitType()
              case _ => UnknownType()
            }
            return Some((m.group(2), class_type))
        case _ => return None
      }
    }
  }

  return None
}

def parseMehodName( src:Source, base_dir:String ):Option[List[MethodInfo]] = {
  val elements = src.getAllElements("class", "member", false)
  var results = List[MethodInfo]()
  for( elem <- elements ){
    val tr = elem.getFirstElement(HTMLElementName.TR)

    ".*Method Summary.*".r.findFirstMatchIn( tr.getTextExtractor.toString ) match {
        case Some(m) =>
          val signatures = elem.getAllElements("class", "signature", false)
          for( sig <- signatures ){
            val em = sig.getFirstElement(HTMLElementName.EM)
            val a = em.getFirstElement(HTMLElementName.A)
            val url =  base_dir + a.getAttributeValue("href")
            val name = em.getTextExtractor.toString

            results += new MethodInfo(name, url.replaceAll("\\.\\./", ""))
          }
       case _ =>
    }
  }

  if( results.isEmpty )
    return None
  else
    return Some(results)
}

val map = new HashMap[PkgInfo, List[ClassInfo]]();
val base_dir = "scala-2.7.5.final-devel-docs/api/"
for( file <- new Dir(base_dir).glob(".*\\.html".r) ){
  val src = new Source(new FileInputStream(file))

  val pkg = parsePkgName( src ) match {
    case Some(pkg_str) => pkg_str
    case _ => ""
  }

  val classtuple = parseClassName( src ) match {
    case Some(classtuple) => classtuple
    case _ => ("", UnknownType())
  }

  val methods = parseMehodName(src, base_dir) match {
    case Some(mis) => mis
    case _ => List()
  }

  val ci =  new ClassInfo( classtuple._1, file.toString, PkgInfo(pkg, base_dir + pkg.replaceAll("\\.", "/") + "$content.html"), methods, classtuple._2)
  if( ci.name != "" ){
    map.get( ci.pkg ) match {
      case Some( l ) =>
          map += ( ci.pkg -> ( l ++ List(ci) ) )
      case _ =>
          map += ( ci.pkg -> List(ci) )
    }
  }
}

def makeIndexFile( map:Map[PkgInfo, List[ClassInfo]], file:String ){
  val f = new FileWriter( file )

  f.write("""|<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML//EN\">
             |<HTML>
             |<HEAD>
             |<!-- Sitemap 1.0 -->
             |</HEAD><BODY>
             |<UL>
             |""".stripMargin )

  for( kv <- map ){
    for( c <- kv._2 ){
      f.write("<LI><OBJECT type=\"text/sitemap\">\n")
      f.write("<param name=\"Name\" value=\"" + c.name + "\">\n")
      f.write("<param name=\"Local\" value=\"" + c.url + "\">\n")
        f.write("</OBJECT></LI>\n")
      for( m <- c.methods ){
        val op = c.class_type match {
          case ClassType() => "#"
          case TraitType() => "#"
          case ObjectType() => "."
        }
        
        f.write("<LI><OBJECT type=\"text/sitemap\">\n")
        f.write("<param name=\"Name\" value=\"" + m.name + "\">\n")
        f.write("<param name=\"Name\" value=\"" + c.name + op + m.name +"\">\n")
        f.write("<param name=\"Local\" value=\"" + m.url + "\">\n")
        f.write("</OBJECT></LI>\n")

        f.write("<LI><OBJECT type=\"text/sitemap\">\n")
        f.write("<param name=\"Name\" value=\"" + c.name + op + m.name +"\">\n")
        f.write("<param name=\"Local\" value=\"" + m.url + "\">\n")
        f.write("</OBJECT></LI>\n")
      }
    }
  }

  f.write("</UL>\n</BODY></HTML>")
  f.close
}

def makeTableOfContentsFile( map:Map[PkgInfo, List[ClassInfo]], base_dir:String, file:String ){
  val f = new FileWriter( file )
    f.write("""|<!DOCTYPE HTML PUBLIC "-//IETF//DTD HTML//EN">
               |<HTML>
               |<HEAD>
               |<!-- Sitemap 1.0 -->
               |</HEAD><BODY>
               |""".stripMargin )

    f.write("<UL>\n<LI><OBJECT type=\"text/sitemap\">\n")
    f.write("<param name=\"Name\" value=\"scala-api\">\n")
    f.write("<param name=\"Local\" value=\"" + base_dir  + "root-content.html\">\n")
    f.write("</OBJECT>\n")

    for( p <- map.keys.toList.sort( (a,b) => a.name.compareTo(b.name) < 0 ) ){
    //for( kv <- map ){
      f.write("<UL>\n<LI><OBJECT type=\"text/sitemap\">\n")
      f.write("<param name=\"Name\" value=\"" + p.name + "\">\n")
      f.write("<param name=\"Local\" value=\"" + p.url + "\">\n")
      f.write("</OBJECT>\n")
      f.write("<UL>\n")
      val cs = map.getOrElse(p, List())
      for( c <- cs.sort( (a,b) => a.name.compareTo(b.name) < 0 ) ){
        f.write("<LI><OBJECT type=\"text/sitemap\">\n")
        f.write("<param name=\"Name\" value=\"" + c.name + "\">\n")
        f.write("<param name=\"Local\" value=\"" + c.url + "\">\n")
        f.write("</OBJECT>\n</LI>\n")
      }
      f.write("</UL>\n")
      f.write("</LI>\n</UL>\n")
    }

    f.write("</LI>\n</UL>\n</BODY></HTML>")
    f.close
}

def makeProjectFile( base_dir:String, file:String ) {
  val f = new FileWriter( file )
  f.write("""|[OPTIONS]
             |Compatibility=1.1 or later
             |Compiled file=scala_api.chm
             |Contents file=toc.hhc
             |Default Window=titlewindow
             |Default topic=scala-2.7.5.final-devel-docs/api/root-content.html
             |Display compile progress=Yes
             |Full-text search=Yes
             |Index file=index.hhk
             |Language=0x409
             |Title=Scala-API
             |
             |[WINDOWS]
             |titlewindow="Scala-API","toc.hhc","index.hhk",,,,,,,0x20420,,0x307e,,,,,,,,0
             |""".stripMargin)
  f.write("[FILES]\n")
  for( content <- new Dir(base_dir).glob(".*".r) ){
    f.write( content.toString + "\n" ) 
  }
  f.close
}

makeTableOfContentsFile( map, base_dir, "toc.hhc" )
makeIndexFile( map, "index.hhk" )
makeProjectFile( base_dir, "scala_api.hhp")
