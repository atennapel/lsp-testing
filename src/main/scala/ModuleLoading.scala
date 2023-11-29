import Surface.*
import Core as C
import Parser.defsParser

import scala.collection.mutable
import scala.annotation.tailrec
import parsley.io.given
import java.net.URI
import java.io.File
import scala.io.Source
import Elaborate.elaborate

object ModuleLoading:
  class UriError(msg: String, val pos: Option[PosInfo]) extends Exception(msg):
    override def toString: String = s"UriError: $msg"

  private type DepMap = mutable.Map[String, Entry]
  private val urimap: DepMap = mutable.Map.empty

  private case class Entry(
      uri: String,
      defs: List[Def],
      uris: Set[String]
  ):
    def hasNoDeps: Boolean = uris.isEmpty
    def removeDep(x: String): Entry = Entry(uri, defs, uris - x)

  def load(uri: String, text: Option[String] = None): List[C.Def] =
    urimap.clear()
    Elaborate.globals.clear()
    loadUris(uri, text, None)
    toposort(urimap) match
      case Left(cycle) =>
        throw Exception(s"cycle in modules: ${cycle.mkString(", ")}")
      case Right(order) => order.flatMap(loadUri)

  def invalidate(uri: String): Unit = urimap.remove(uri)

  private def loadUris(
      uri: String,
      textIn: Option[String],
      pos: Option[PosInfo]
  ): Unit =
    if !urimap.contains(uri) then
      def loadFromFile =
        try Source.fromURL(uri).mkString
        catch _ => throw UriError(s"failed to load URI: $uri", pos)
      val text = textIn.getOrElse(loadFromFile)
      val defs = defsParser.parse(text).toTry.get
      val uris = defs.flatMap(_.imports)
      urimap.put(uri, Entry(uri, defs, uris.map(_.uri).toSet))
      uris
        .filter(u => !urimap.contains(u.uri))
        .foreach(u => loadUris(u.uri, None, Some(u.pos)))

  private def loadUri(uri: String): List[C.Def] =
    val entry = urimap(uri)
    elaborate(entry.defs)

  private def toposort(map: DepMap): Either[List[String], List[String]] =
    toposort(map.clone(), Nil, map.values.filter(_.hasNoDeps).toList)
      .map(_.reverse)

  @tailrec
  private def toposort(
      map: DepMap,
      l: List[String],
      es: List[Entry]
  ): Either[List[String], List[String]] = es match
    case Nil if !map.values.forall(_.hasNoDeps) =>
      Left(map.filter((_, v) => !v.hasNoDeps).keys.toList)
    case Nil => Right(l)
    case Entry(x, _, deps) :: s =>
      val dependents =
        map.values.filter(_.uris.contains(x)).map(_.removeDep(x))
      dependents.foreach(e => map += (e.uri -> e))
      toposort(map, x :: l, s ++ dependents.filter(_.hasNoDeps))
