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
    generatePackageChildren(root.elements) ++ generatePackageFile(ScalaAST.Package("root", Seq.empty, root.elements))
  }

  private def generatePackage(aPackage: ScalaAST.Package): Seq[ScalaAST.ScalaFile] = {
    generatePackageChildren(aPackage.elements) ++ generatePackageFile(aPackage)
  }

  private def generatePackageFile(aPackage: ScalaAST.Package): Option[ScalaAST.ScalaFile] = {
    if (aPackage.elements.exists(_.isInstanceOf[ScalaAST.Statement])) {
      val finalPackageName = StringUtils.excludeLastPart(aPackage.name, '.')
      val packageStatement = Some(finalPackageName).filter(_.nonEmpty).map(ScalaAST.PackageDeclaration)
      Some(buildScalaFile(
        aPackage.name,
        ScalaCodeGenerator(
          ScalaAST.StatementsGroup(packageStatement)
            .plus(ScalaAST.Blank)
            .plus(importLines)
            .plus(aPackage.imports)
            .plus(ScalaAST.Blank)
            .plus(ScalaAST.PackageDef(
              name = StringUtils.lastPart(aPackage.name, '.'),
              body = aPackage.elements.collect { case statement: ScalaAST.Statement => statement }
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
