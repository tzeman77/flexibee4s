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
import mill.define.Command
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

  override def artifactName = "flexibee4s" 

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

  def entities = T{Seq("adresar")}

  private val evidenceList = "evidence-list"
  private val evidenceDir = millSourcePath / 'evidence

  private def downloadJson(path: String): ujson.Value =
    ujson.read(requests.get(s"$baseUrl/$path.json").text)

  private def downloadEvidenceProperties(entity: String): ujson.Value = {
    downloadJson(s"$entity/properties")
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
}

// vim: et ts=2 sw=2 syn=scala
