package datomlite

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, StandardOpenOption}

/** Append-only file backend for `Storage`. One datom per line, tab-separated, UTF-8. Writes are
  * flushed at the end of each `append` (one tx = one batch). Open/close happens per call so the
  * Storage holds no live OS handle between txs.
  *
  * Format: `<eid>\t<attr>\t<typeTag>\t<value>\t<tx>\t<timeMillis>\t<op>` Tags: `s` String, `l`
  * Long, `i` Int, `d` Double, `b` Boolean. Op: `+` Assert, `-` Retract.
  */
object FileStorage {
  def apply(path: Path): Storage = {
    Option(path.getParent).foreach(Files.createDirectories(_))
    if !Files.exists(path) then Files.createFile(path)
    new Impl(path)
  }

  final private class Impl(path: Path) extends Storage {
    def load(): Vector[Datom] = {
      val reader = Files.newBufferedReader(path, StandardCharsets.UTF_8)
      try
        Iterator
          .continually(reader.readLine())
          .takeWhile(_ != null)
          .filter(_.nonEmpty)
          .map(parse)
          .toVector
      finally reader.close()
    }

    def append(datoms: Vector[Datom]): Unit =
      if datoms.nonEmpty then {
        val writer = Files.newBufferedWriter(
          path,
          StandardCharsets.UTF_8,
          StandardOpenOption.CREATE,
          StandardOpenOption.APPEND
        )
        try {
          datoms.foreach { d =>
            writer.write(format(d))
            writer.newLine()
          }
          writer.flush()
        } finally writer.close()
      }
  }

  private def format(d: Datom): String = {
    val (tag, vstr) = tagAndValue(d.v)
    val op = d.op match {
      case Op.Assert  => "+"
      case Op.Retract => "-"
    }
    Vector(
      Eid.value(d.e).toString,
      d.a,
      tag,
      vstr,
      TxId.value(d.t).toString,
      Time.epochMillis(d.time).toString,
      op
    ).mkString("\t")
  }

  private def parse(line: String): Datom = {
    val parts = line.split("\t", 7)
    if parts.length != 7 then
      throw new RuntimeException(s"FileStorage: malformed line: $line")
    val eid = Eid(parts(0).toLong)
    val attr = parts(1)
    val v = parseValue(parts(2), parts(3))
    val tx = TxId(parts(4).toLong)
    val time = Time.fromEpochMillis(parts(5).toLong)
    val op = parts(6) match {
      case "+" => Op.Assert
      case "-" => Op.Retract
      case other =>
        throw new RuntimeException(s"FileStorage: unknown op '$other' in line: $line")
    }
    Datom(eid, attr, v, tx, time, op)
  }

  private def tagAndValue(v: Any): (String, String) = v match {
    case s: String  => ("s", encodeStr(s))
    case l: Long    => ("l", l.toString)
    case i: Int     => ("i", i.toString)
    case d: Double  => ("d", java.lang.Double.toString(d))
    case b: Boolean => ("b", b.toString)
    case other =>
      throw new RuntimeException(
        s"FileStorage: unsupported value type ${other.getClass.getName} (value: $other)"
      )
  }

  private def parseValue(tag: String, raw: String): Any = tag match {
    case "s" => decodeStr(raw)
    case "l" => raw.toLong
    case "i" => raw.toInt
    case "d" => raw.toDouble
    case "b" => raw.toBoolean
    case other =>
      throw new RuntimeException(s"FileStorage: unknown value tag '$other'")
  }

  private def encodeStr(s: String): String =
    s.flatMap {
      case '\\' => "\\\\"
      case '\t' => "\\t"
      case '\n' => "\\n"
      case '\r' => "\\r"
      case c    => c.toString
    }

  private def decodeStr(s: String): String = {
    @scala.annotation.tailrec
    def loop(i: Int, acc: StringBuilder): String =
      if i >= s.length then acc.toString
      else {
        val c = s.charAt(i)
        if c == '\\' && i + 1 < s.length then
          s.charAt(i + 1) match {
            case '\\'  => loop(i + 2, acc.append('\\'))
            case 't'   => loop(i + 2, acc.append('\t'))
            case 'n'   => loop(i + 2, acc.append('\n'))
            case 'r'   => loop(i + 2, acc.append('\r'))
            case other => loop(i + 2, acc.append(c).append(other))
          }
        else loop(i + 1, acc.append(c))
      }
    loop(0, new StringBuilder(s.length))
  }
}
