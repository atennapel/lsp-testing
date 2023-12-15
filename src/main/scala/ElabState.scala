import scala.collection.mutable
import Core.{Name, Def, Ty}

object ElabState:
  private type DepMap = mutable.Map[String, Entry]
  private val urimap: DepMap = mutable.Map.empty
  private val revDeps: mutable.Map[String, mutable.Set[String]] =
    mutable.Map.empty

  def unsafeState: DepMap = urimap

  final case class Entry(
      uri: String,
      src: String,
      defs: mutable.ArrayBuffer[Def],
      dependsOn: Set[String]
  )

  def exists(name: Name): Boolean =
    urimap.exists((_, e) => e.defs.exists(d => d.name == name))

  def lookup(name: Name): Option[Ty] =
    urimap
      .find((_, e) => e.defs.exists(d => d.name == name))
      .flatMap((_, e) => e.defs.find(d => d.name == name).map(_.ty))

  def dependencies(uri: String): Set[String] =
    urimap.get(uri).map(_.dependsOn).getOrElse(Set.empty)
  def reverseDependencies(uri: String): Set[String] =
    revDeps.get(uri).map(_.toSet).getOrElse(Set.empty)
  def defs(uri: String): List[Def] = urimap(uri).defs.toList
  def cachedSource(uri: String): Option[String] = urimap.get(uri).map(_.src)

  def allDefs: List[Def] = urimap.flatMap((_, e) => e.defs).toList

  def setModule(uri: String, src: String, dependsOn: Set[String]): Unit =
    urimap += (uri -> Entry(uri, src, mutable.ArrayBuffer.empty, dependsOn))
    dependsOn.foreach(revUri => addRevDep(uri, revUri))

  def addDef(uri: String, d: Def): Unit =
    urimap(uri).defs += d

  private def addRevDep(uri: String, dependsOn: String): Unit =
    revDeps.get(dependsOn) match
      case None      => revDeps += (dependsOn -> mutable.Set(uri))
      case Some(set) => set += uri
