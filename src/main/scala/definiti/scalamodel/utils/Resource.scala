package definiti.scalamodel.utils

import java.io.{BufferedReader, InputStreamReader}
import java.nio.charset.StandardCharsets
import java.util.jar.JarFile

import better.files.File

import scala.collection.JavaConverters._

/**
 * This utility is to manage resources when they come from file system or jar file.
 */
trait Resource {
  def children: Seq[Resource]

  def content: String

  def name: String
}

object Resource {
  private val classLoader: ClassLoader = getClass.getClassLoader

  def apply(path: String): Resource = {
    val resource = classLoader.getResource(path)
    if (resource.getProtocol == "jar") {
      val filePath = resource.getPath
      val jarPath = new java.net.URL(filePath.substring(0, filePath.indexOf("!"))).getPath
      val jarFile = new JarFile(jarPath)
      JarResource(jarFile, path)
    } else {
      FileResource(File(new java.io.File(resource.getPath).toPath))
    }
  }
}

case class FileResource(file: File) extends Resource {
  override def children: Seq[Resource] = {
    file.children.map(FileResource).toSeq
  }

  override def content: String = {
    val stringBuffer = new StringBuffer()
    file.newBufferedReader(StandardCharsets.UTF_8)
      .lines()
      .forEach(line => stringBuffer.append(line).append("\n"))
    stringBuffer.toString
  }

  override def name: String = file.name
}

case class JarResource(jar: JarFile, path: String) extends Resource {
  override def children: Seq[Resource] = {
    jar
      .entries()
      .asScala
      .filter { entry =>
        val name = entry.getName
        if (name.startsWith(path) && name != path && name != path + "/") {
          !name.substring(path.length + 1).contains("/")
        } else {
          false
        }
      }
      .map(entry => JarResource(jar, entry.getName))
      .toSeq
  }

  override def content: String = {
    val inputStream = jar.getInputStream(jar.getJarEntry(path))
    val stringBuffer = new StringBuffer()
    new BufferedReader(new InputStreamReader(inputStream, StandardCharsets.UTF_8))
      .lines()
      .forEach(line => stringBuffer.append(line).append("\n"))
    stringBuffer.toString
  }

  override def name: String = {
    val pathWithoutTrailingSlash = if (path.endsWith("/")) path.substring(0, path.length - 1) else path
    if (pathWithoutTrailingSlash.contains("/")) {
      pathWithoutTrailingSlash.substring(pathWithoutTrailingSlash.lastIndexOf("/") + 1)
    } else {
      pathWithoutTrailingSlash
    }
  }
}