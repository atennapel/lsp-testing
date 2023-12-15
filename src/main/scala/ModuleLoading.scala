import Surface.*
import Core as C
import Parser.defsParser
import ElabState as ES

import scala.collection.mutable
import scala.annotation.tailrec
import parsley.io.given
import java.net.URI
import java.io.File
import scala.io.Source
import Elaborate.elaborate
import java.nio.file.Path

object ModuleLoading:
  private def logger = Logger.instance

  class UriError(msg: String, val pos: Option[PosInfo]) extends Exception(msg):
    override def toString: String = s"UriError: $msg"

  def load(
      uriIn: String,
      textIn: Option[String] = None,
      pos: Option[PosInfo] = None,
      forced: Boolean = false
  )(implicit chain: mutable.Set[String]): Unit =
    val uri = normalizeURI(uriIn)
    if chain.contains(uri) then
      logger.log(s"load, in chain: $uri")
      ()
    else
      logger.log(s"load: $uri")
      inline def loadFromFile =
        try Source.fromURL(s"file:${uri.drop(7)}").mkString
        catch _ => throw UriError(s"failed to load URI: $uri", pos)
      val text = textIn.getOrElse(loadFromFile)
      ES.cachedSource(uri) match
        case Some(text2) if !forced && text == text2 =>
          logger.log(s"load, use cached: $uri")
          ()
        case _ =>
          ES.setModule(uri, text, Set.empty)
          val defs = defsParser.parse(text).toTry.get
          val uris = defs.flatMap(_.imports)
          val deps = uris.map(i => normalizeURI(i.uri)).toSet
          logger.log(s"load, add module: $uri")
          ES.setModule(uri, text, deps)
          chain += uri
          logger.log(
            s"load, load deps: $uri, ${uris.map(_.uri).mkString("[", ",", "]")}"
          )
          uris.foreach(i => load(i.uri, None, Some(i.pos)))
          logger.log(s"load, elaborate: $uri")
          elaborate(uri, defs)
          val revDeps = ES.reverseDependencies(uri)
          logger.log(
            s"load, rev deps: $uri, ${revDeps.mkString("[", ",", "]")}"
          )
          revDeps.foreach(load(_, forced = true))

  def defs(uri: String): List[C.Def] = ES.defs(normalizeURI(uri))
  def allDefs: List[C.Def] = ES.allDefs

  private def normalizeURI(uriIn: String): String =
    val uriNorm = uriIn.replace("%3A", ":") match
      case u if u.startsWith("file:///") => u.drop(8)
      case u if u.startsWith("file://")  => u.drop(7)
      case u if u.startsWith("file:")    => u.drop(5)
      case u                             => u
    s"file://${Path.of(uriNorm).toFile().getCanonicalPath()}"
