package definiti.scalamodel.model

import definiti.core.ast.{AliasType, NamedFunction, Verification}

case class PackageFile(
  packageName: String,
  verifications: Seq[Verification],
  namedFunctions: Seq[NamedFunction],
  nativeAliasType: Seq[AliasType]
) {
  def isEmpty: Boolean = {
    verifications.isEmpty && namedFunctions.isEmpty && nativeAliasType.isEmpty
  }

  def nonEmpty: Boolean = !isEmpty
}

object PackageFile {
  def squash(packageFiles: Seq[PackageFile]): PackageFile = {
    PackageFile(
      packageName = packageFiles.headOption.map(_.packageName).getOrElse(""),
      verifications = packageFiles.flatMap(_.verifications),
      namedFunctions = packageFiles.flatMap(_.namedFunctions),
      nativeAliasType = packageFiles.flatMap(_.nativeAliasType)
    )
  }

  def squash(packageFiles: PackageFile*)(implicit dummyImplicit: DummyImplicit): PackageFile = {
    squash(packageFiles)
  }
}