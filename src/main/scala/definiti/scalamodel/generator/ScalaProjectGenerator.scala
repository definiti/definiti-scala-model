package definiti.scalamodel.generator

import definiti.scalamodel.utils.StringUtils
import definiti.scalamodel.{Configuration, ScalaAST}

class ScalaProjectGenerator(config: Configuration) {
  private val importLines: ScalaAST.StatementsGroup = {
    ScalaAST.StatementsGroup(
      ScalaAST.Import("definiti.native._"),
      ScalaAST.Import("java.time.LocalDateTime")
    )
  }

  def generateProject(root: ScalaAST.Root): Seq[ScalaAST.ScalaFile] = {
    generatePackageChildren(root.elements) ++ generatePackageFile("root", root.elements)
  }

  private def generatePackage(aPackage: ScalaAST.Package): Seq[ScalaAST.ScalaFile] = {
    generatePackageChildren(aPackage.elements) ++ generatePackageFile(aPackage.name, aPackage.elements)
  }

  private def generatePackageFile(packageName: String, packageElements: Seq[ScalaAST.PackageElement]): Option[ScalaAST.ScalaFile] = {
    if (packageElements.exists(_.isInstanceOf[ScalaAST.Statement])) {
      val finalPackageName = StringUtils.excludeLastPart(packageName, '.')
      val packageStatement = Some(finalPackageName).filter(_.nonEmpty).map(ScalaAST.PackageDeclaration)
      Some(buildScalaFile(
        packageName,
        ScalaCodeGenerator(
          ScalaAST.StatementsGroup(packageStatement)
            .plus(importLines)
            .plus(ScalaAST.PackageDef(
              name = StringUtils.lastPart(packageName, '.'),
              body = packageElements.collect { case statement: ScalaAST.Statement => statement }
            ))
        )
      ))
    } else {
      None
    }
  }

  private def generatePackageChildren(packageElements: Seq[ScalaAST.PackageElement]): Seq[ScalaAST.ScalaFile] = {
    packageElements.collect {
      case aPackage: ScalaAST.Package => generatePackage(aPackage)
    }.flatten
  }

  private def buildScalaFile(packageName: String, content: String): ScalaAST.ScalaFile = {
    val dirname = packageName.replaceAllLiterally(".", "/")
    val filename = StringUtils.lastPart(packageName, '.')
    val path = config.destination.resolve(dirname).resolve(s"$filename.scala")
    ScalaAST.ScalaFile(path, content)
  }
}
