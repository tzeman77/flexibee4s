/*
 * REPL:
 *
 * JAVA_OPTS="-XX:+CMSClassUnloadingEnabled -XX:MetaspaceSize=2G -Xmx2G -Xss32M" ./mill --repl -w
 *
 * Generate Idea project:
 *
 * ./mill mill.scalalib.GenIdea/idea
 *
 */ 

import mill._
import mill.api.Loose
import mill.define.{Command, Sources}
import mill.scalajslib.ScalaJSModule
import mill.scalalib._
import mill.scalalib.publish._
import upickle.default._

// http://podpora.flexibee.eu/cs/articles/3638757-jak-zacit-s-api-flexibee-6-6
val baseUrl = "https://demo.flexibee.eu/c/demo"

object V {
  val app = "0.2.3-SNAPSHOT"
  val scala213 = "2.13.6"
  val scalaJs = "1.5.1"
}

object D {
  val upickle = ivy"com.lihaoyi::upickle::1.4.0"
}

val compilerOptions = Seq(
    "-deprecation",                      // Emit warning and location for usages of deprecated APIs.
    "-encoding", "utf-8",                // Specify character encoding used by source files.
    "-explaintypes",                     // Explain type errors in more detail.
    "-feature",                          // Emit warning and location for usages of features that should be imported explicitly.
    "-language:higherKinds",             // Allow higher-kinded types
    "-language:implicitConversions",     // Allow definition of implicit functions called views
    "-language:reflectiveCalls",
    "-language:postfixOps",
    "-unchecked",                        // Enable additional warnings where generated code depends on assumptions.
    "-Xcheckinit",                       // Wrap field accessors to throw an exception on uninitialized access.
    "-target:jvm-1.8"
  )

trait Common extends ScalaModule with PublishModule {

  def pomSettings: T[PomSettings] = PomSettings(
    description = "Data model of FlexiBee client API",
    organization = "cz.functionals",
    url = "...",
    licenses = Seq(License.`Apache-2.0`),
    versionControl = VersionControl(),
    developers = Seq(
      Developer("tzeman", "Tomas Zeman", "https://functionals.cz")
    )
  )

  override def scalaVersion: T[String] = V.scala213

  override def artifactName: T[String] =
  s"flexibee4s-${super.artifactName()}".stripSuffix("-jvm").stripSuffix("-js")

  override def publishVersion: T[String] = V.app

  override def scalacOptions = T{compilerOptions}

  override def sources: Sources = T.sources {
    super.sources() :+ PathRef(millSourcePath / 'shared)
  }

}

case class FieldDescriptor(propertyName: String, name: String, title: String,
  `type`: String, mandatory: String,
  values: FieldDescriptor.Values = FieldDescriptor.Values(Seq.empty))

object FieldDescriptor {

  import upickle.implicits.key

  implicit val rw: ReadWriter[FieldDescriptor] = macroRW[FieldDescriptor]

  case class Values(value: Seq[Value])

  object Values {
    implicit val rw: ReadWriter[Values] = macroRW
  }

  case class Value(@key("@key") key: String, @key("$") v: String)

  object Value {
    implicit val rw: ReadWriter[Value] = macroRW
  }

}

case class EvidenceDescriptor(properties: EvidenceDescriptor.Inner)

object EvidenceDescriptor {
  implicit val rw: ReadWriter[EvidenceDescriptor] = macroRW[EvidenceDescriptor]

  case class Inner(evidenceName: String, tagName: String,
    dbName: String, property: Seq[FieldDescriptor])

  object Inner {
    implicit val rw: ReadWriter[Inner] = macroRW[Inner]
  }
}

trait Model extends Common {

  def entities = T{Seq(
    "adresar",
    "faktura-vydana",
    "konst-symbol",
    "pohledavka",
    "skupina-stitku",
    "smlouva",
    "smlouva-polozka",
    "stav-smlouvy",
    "stitek",
    "stredisko",
    "typ-smlouvy",
    "zakazka"
  )}

  private val evidenceList = "evidence-list"
  private val evidenceDir = millSourcePath / os.up / 'evidence
  val classBreakpoint = 200

  private def downloadJson(path: String): ujson.Value =
    ujson.read(requests.get(s"$baseUrl/$path.json").text)

  private def downloadEvidenceProperties(entity: String): ujson.Value = {
    downloadJson(s"$entity/properties")
  }

  def pascalCase(s: String): String = s.split("-").map(
    _.toList match {
      case first :: rest => (first.toUpper :: rest).mkString
    }).mkString

  def escapeIdent(s: String): String =
    if (s.contains("-")) s"`$s`" else s

  def camelCase(s: String): String = pascalCase(s).toList match {
    case first :: rest => (first.toLower :: rest).mkString
  }

  def refreshEvidences(): Command[Unit] = T.command{
    os.write.over(evidenceDir / s"$evidenceList.json", ujson.write(
      downloadJson(evidenceList) , 2))
    entities() foreach { e =>
      os.write.over(evidenceDir / s"$e.json",
        ujson.write(downloadEvidenceProperties(e), 2))
    }
  }

  def readEvidence(ident: String): EvidenceDescriptor =
    read[EvidenceDescriptor](os.read(evidenceDir / s"$ident.json"))

  def generateEvidenceModel(ed: EvidenceDescriptor): String = {

    def fldType(fd: FieldDescriptor, mkDefault: Boolean = false): String = {
      val t = fd.`type` match {
        case "string" | "date" | "datetime" => "String"
        case "integer" => "Int"
        case "numeric" => "Double"
        case _ => "String"
      }
      fd.mandatory match {
        case "true" => t
        case _ if mkDefault => s"Option[$t] = None"
        case _ => s"Option[$t]"
      }
    }

    def fld2src(fd: FieldDescriptor): String =
      s"""
         |  /** Name: ${fd.name}.
         |   *  Title: ${fd.title}.
         |   *  Type: ${fd.`type`}, mandatory: ${fd.mandatory}.*/
         |  val ${fd.propertyName}: ${fldType(fd, mkDefault = true)}""".stripMargin

    def fld2values(fd: FieldDescriptor): String = fd.values.value map(v =>
      s"""Value("${v.key}", "${v.v.replaceAll("\"", "'")}")"""
      ) mkString("Seq(", ", ", ")")

    def fld2desc(fd: FieldDescriptor): String =
      s"""  val ${fd.propertyName}: FieldDescriptor[${fldType(fd)}] =
         |    FieldDescriptor[${fldType(fd)}](
         |      propertyName = "${fd.propertyName}",
         |      name = "${fd.name}",
         |      title = "${fd.title}",
         |      `type` = "${fd.`type`}",
         |      mandatory = ${"true" == fd.mandatory},
         |      values = ${fld2values(fd)}
         |    )""".stripMargin

    val extendsEvidenceDescriptor =
      s"""EvidenceDescriptor(
         |  evidenceName = "${ed.properties.evidenceName}",
         |  tagName = "${ed.properties.tagName}",
         |  dbName = "${ed.properties.dbName}"
         |)""".stripMargin

    val d = ed.properties
    val cls = pascalCase(d.tagName)
    val tooBig = d.property.length > classBreakpoint

    val groups = d.property.grouped(classBreakpoint).toList.zipWithIndex

    val parts = if (tooBig) {
      groups map { case (grp, i) =>
        s"""
           |case class $cls$i(
           |${grp map fld2src mkString ",\n"}
           |)
           |
           |object $cls$i {
           |  implicit val rw: ReadWriter[$cls$i] = macroRW
           |}
           |""".stripMargin
      }
    } else Seq()

    val companion = if (tooBig) {
      s"""object $cls extends $extendsEvidenceDescriptor {
         |  def apply(${groups map(_._2) map(i => s"p$i: $cls$i") mkString ", "}): $cls = new $cls(
         |${groups flatMap { case (grp, i) => grp map { fld =>
        s"    ${fld.propertyName} = p$i.${fld.propertyName}"}} mkString ",\n" }
         |  )
         |
         |  def apply(js: ujson.Obj): $cls = $cls(
         |${groups map { case (_, i) =>
        s"    Pickler.read[$cls$i](js)"} mkString ",\n"}
         |  )
         |
         |${d.property map fld2desc mkString "\n\n"}
         |}
         |""".stripMargin
    } else
      s"""
         |object $cls extends $extendsEvidenceDescriptor {
         |  implicit val rw: ReadWriter[$cls] = macroRW
         |
         |${d.property map fld2desc mkString "\n\n"}
         |}
         |""".stripMargin

    s"""
       |// This file is autogenerated. Do not modify.
       |
       |package fxb
       |
       |import FieldDescriptor.Value
       |import Pickler._
       |
       |/**
       | * Evidence: ${d.evidenceName}.
       | * dbName: ${d.dbName}
       | */
       |${if (tooBig) "" else "case "}class $cls(
       |${d.property map fld2src mkString ",\n"}
       |)
       |
       |${parts mkString "\n"}
       |
       |$companion
       |
       |case class ${cls}W(winstrom: ${cls}W.Inner)
       |object ${cls}W {
       |
       |  implicit val rw: ReadWriter[${cls}W] = macroRW
       |
       |  case class Inner(@upickle.implicits.key("${d.tagName}") ${camelCase(d.tagName)}: Seq[${if (tooBig) "ujson.Obj" else cls}])
       |
       |  object Inner {
       |    implicit val rw: ReadWriter[${cls}W.Inner] = macroRW
       |  }
       |}
       |""".stripMargin
  }

  override def generatedSources: Sources = T.sources{
    val d = T.ctx.dest
    val l = entities()
    l.foreach { e =>
      os.write(d / s"${pascalCase(e)}.scala",
        generateEvidenceModel(readEvidence(e)))
    }
    Seq(PathRef(d))
  }

  override def ivyDeps: T[Loose.Agg[Dep]] = Agg(D.upickle)
}

object model extends Module {

  object jvm extends Model

  object js extends Model with ScalaJSModule {
    override def scalaJSVersion: T[String] = V.scalaJs
  }

}

def publishLocal(): Command[Unit] = T.command{
  model.jvm.publishLocal()()
  model.js.publishLocal()()
}

def publishM2Local(p: os.Path): Command[Unit] = T.command{
  model.jvm.publishM2Local(p.toString)()
  model.js.publishM2Local(p.toString)()
  ()
}

// vim: et ts=2 sw=2 syn=scala
