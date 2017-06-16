package definiti.scalamodel.plugin

import java.nio.charset.StandardCharsets
import java.nio.file.{Path, Paths}

import definiti.core._
import definiti.scalamodel.model.PackageFile
import definiti.scalamodel.utils.StringUtils
import definiti.scalamodel.{Configuration, ScalaAST, ScalaASTBuilder}
import org.apache.commons.io.FileUtils

import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class ScalaModelGeneratorPlugin extends GeneratorPlugin {
  val config = new Configuration()

  override def name: String = "scala-model-generator"

  override def generate(root: Root, context: Context): Map[Path, String] = {
    nativeSources ++ generatedSources(root, context)
  }

  def nativeSources: Map[Path, String] = {
    val source = Paths.get("src", "main", "resources", "native")
    val destinationDirectory = config.destination.resolve("definiti").resolve("native")
    FileUtils.listFiles(source.toFile, Seq("scala").toArray, false)
      .asScala
      .map(file => destinationDirectory.resolve(file.getName) -> FileUtils.readFileToString(file, StandardCharsets.UTF_8))
      .toMap
  }

  def generatedSources(root: Root, context: Context): Map[Path, String] = {
    createScalaSourceFiles(root, context)
      .map(scalaFile => scalaFile.path -> scalaFile.content)
      .toMap
  }

  def createScalaSourceFiles(root: Root, context: Context): Seq[ScalaAST.ScalaFile] = {
    implicit val c = context
    // packageName -> root file
    val rootFilesByPackage = mutable.Map[String, ListBuffer[RootFile]]()

    root.files.foreach { rootFile =>
      val listBufferForPackage = rootFilesByPackage.getOrElseUpdate(rootFile.packageName, ListBuffer())
      listBufferForPackage.append(rootFile)
    }

    rootFilesByPackage.toSeq.flatMap { case (packageName, contentFiles) =>
      val packageFile = buildPackageFile(root, packageName).map { packageFile =>
        buildScalaFile(
          packageName,
          "package.scala",
          ScalaASTBuilder.buildPackageFile(packageFile)
        )
      }

      val otherFiles = contentFiles
        .filter(_.classDefinitions.nonEmpty)
        .zipWithIndex
        .map { case (contentFile, index) =>
          buildScalaFile(
            packageName,
            s"${StringUtils.lastPart(packageName, '.')}_$index.scala",
            ScalaASTBuilder.build(contentFile)
          )
        }

      packageFile ++ otherFiles
    }
  }

  def buildScalaFile(packageName: String, filename: String, content: String): ScalaAST.ScalaFile = {
    val dirname = s"${packageName.replaceAllLiterally(".", "/")}"
    val path = config.destination.resolve(dirname).resolve(filename)
    ScalaAST.ScalaFile(path, content)
  }

  def buildPackageFile(root: Root, packageName: String)(implicit context: Context): Option[PackageFile] = {
    val acceptedFiles = root.files
      .filter(_.packageName == packageName)
    val packageFiles = acceptedFiles.map { rootFile =>
      val nativeAliasTypes = rootFile.classDefinitions
        .collect { case aliasType: AliasType => aliasType }
        .filter { aliasType =>
          context.findType(aliasType.alias.typeName) match {
            case Some(_: NativeClassDefinition) => true
            case _ => false
          }
        }
      PackageFile(packageName, rootFile.verifications, rootFile.namedFunctions, nativeAliasTypes)
    }
    Some(PackageFile.squash(packageFiles))
      .filter(_.nonEmpty)
  }
}
