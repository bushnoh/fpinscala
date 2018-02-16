package fpinscala.parsing

trait JSON

object JSON {

  case object JNull extends JSON

  case class JNumber(get: Double) extends JSON

  case class JString(get: String) extends JSON

  case class JBool(get: Boolean) extends JSON

  case class JArray(get: IndexedSeq[JSON]) extends JSON

  case class JObject(get: Map[String, JSON]) extends JSON


  def jsonParser[Parser[+ _]](P: Parsers[Parser]): Parser[JSON] = {
    import P._

    def kv = (quotedstring ** (rhs(wswrap(":"), jsonobjs)))

    def jsonobjs: Parser[JSON] = primitives | array | dict

    def array = wrap(wswrap("["), wswrap("]"))(separated(jsonobjs, wswrap(","))).map(l => JArray(l.toIndexedSeq)) scope("array")

    def dict = wrap(wswrap("{"), wswrap("}"))(separated(kv, wswrap(","))).map(lkv => JObject(lkv.toMap)) scope("object")

    def primitives: Parser[JSON] = {
      string("true").map(_ => JBool(true)) |
        string("false").map(_ => JBool(false)) |
        string("null").map(_ => JNull) |
        double.map(s => JNumber(s.toDouble)) |
        quotedstring.map(JString(_))
    } scope("primitive")

    lhs((dict | array), eof)

  }
}
