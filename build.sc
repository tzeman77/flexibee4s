/*
 * REPL:
 *
 * ./mill --repl -w
 *
 * Generate Idea project:
 *
 * ./mill mill.scalalib.GenIdea/idea
 *
 */ 

import mill._
import mill.api.Loose
import mill.define.{Command, Sources}
import mill.scalalib._
import mill.scalalib.publish._
import upickle.default._

// http://podpora.flexibee.eu/cs/articles/3638757-jak-zacit-s-api-flexibee-6-6
val baseUrl = "https://demo.flexibee.eu/c/demo"

object V {
  val app = "0.1-SNAPSHOT"
  val scala213 = "2.13.4"
}

object D {
  //val ujson = ivy"com.lihaoyi::ujson::1.2.2"
  val upickle = ivy"com.lihaoyi::upickle::1.2.2"
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

  override def artifactName = s"flexibee4s-${super.artifactName()}"

  override def publishVersion: T[String] = V.app

  override def scalacOptions = T{compilerOptions}

}

case class FieldDescriptor(propertyName: String, name: String, title: String,
  `type`: String, mandatory: String)

object FieldDescriptor {
  implicit val rw: ReadWriter[FieldDescriptor] = macroRW[FieldDescriptor]
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

object model extends Common {

  def entities = T{Seq("adresar", "smlouva", "faktura-vydana", "pohledavka",
    "stitek", "skupina-stitku")}

  private val evidenceList = "evidence-list"
  private val evidenceDir = millSourcePath / 'evidence

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

    def fldType(fd: FieldDescriptor): String = {
      val t = fd.`type` match {
        case "string" | "date" | "datetime" => "String"
        case "integer" => "Int"
        case "numeric" => "Double"
        case _ => "String"
      }
      fd.mandatory match {
        case "true" => t
        case _ => s"Option[$t] = None"
      }
    }

    def fld2src(fd: FieldDescriptor): String =
      s"""
         |  /** Name: ${fd.name}.
         |   *  Title: ${fd.title}.
         |   *  Type: ${fd.`type`}, mandatory: ${fd.mandatory}.*/
         |  ${fd.propertyName}: ${fldType(fd)}""".stripMargin

    val d = ed.properties
    val cls = pascalCase(d.tagName)
    val tooBig = d.property.length > 64

    val groups = d.property.grouped(64).toList.zipWithIndex

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
      s"""object $cls {
         |  def apply(${groups map(_._2) map(i => s"p$i: $cls$i") mkString ", "}): $cls = $cls(
         |${groups flatMap { case (grp, i) => grp map { fld =>
        s"    ${fld.propertyName} = p$i.${fld.propertyName}"}} mkString ",\n" }
         |  )
         |
         |  def apply(js: ujson.Obj): $cls = $cls(
         |${groups map { case (_, i) =>
        s"    Pickler.read[$cls$i](js)"} mkString ",\n"}
         |  )
         |}
         |""".stripMargin
    } else
      s"""
         |object $cls {
         |  implicit val rw: ReadWriter[$cls] = macroRW
         |}
         |""".stripMargin

    s"""
       |// This file is autogenerated. Do not modify.
       |
       |package fxb
       |
       |import Pickler._
       |
       |/**
       | * Evidence: ${d.evidenceName}.
       | * dbName: ${d.dbName}
       | */
       |case class $cls(
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

// vim: et ts=2 sw=2 syn=scala
