package definiti.scalamodel

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths, StandardOpenOption}

import definiti.core._
import definiti.scalamodel.model.PackageFile
import definiti.scalamodel.utils.StringUtils
import org.apache.commons.io.FileUtils

import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

case class ScalaModelProjectResult(
  files: Seq[ScalaAST.ScalaFile],
  context: Context
)

class ScalaModelProject(configuration: ScalaModelConfiguration) {
  private val coreProject = new Project(configuration.core)

  def load(): Either[Seq[String], ScalaModelProjectResult] = {
    coreProject.load() match {
      case Left(errors) =>
        Left(errors)
      case Right(projectResult) =>
        Right(ScalaModelProjectResult(
          files = createScalaSourceFiles(projectResult.root, projectResult.context),
          context = projectResult.context
        ))

    }
  }

  def write(projectResult: ScalaModelProjectResult): Unit = {
    copyNativeSources()
    projectResult.files.foreach { file =>
      Files.createDirectories(file.path.getParent)
      Files.write(file.path, Seq(file.content).asJava, StandardCharsets.UTF_8, StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING)
    }
  }

  def loadAndWrite(): Either[Seq[String], ScalaModelProjectResult] = {
    load() match {
      case Left(errors) =>
        Left(errors)
      case Right(result) =>
        write(result)
        Right(result)
    }
  }

  private def copyNativeSources(): Unit = {
    val source = Paths.get("src", "main", "resources", "native")
    val destination = configuration.destination.resolve("definiti").resolve("native")
    Files.createDirectories(destination)
    FileUtils.copyDirectory(source.toFile, destination.toFile)
  }

  private def createScalaSourceFiles(root: Root, context: Context): Seq[ScalaAST.ScalaFile] = {
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

  private def buildScalaFile(packageName: String, filename: String, content: String): ScalaAST.ScalaFile = {
    val dirname = s"${packageName.replaceAllLiterally(".", "/")}"
    val path = configuration.destination.resolve(dirname).resolve(filename)
    ScalaAST.ScalaFile(path, content)
  }

  private def buildPackageFile(root: Root, packageName: String)(implicit context: Context): Option[PackageFile] = {
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
