/*
GUSTAVO ENCARNACION OCAMPO 16211993
Ing. Sistemas Computacionales
Lenguajes y Automatas I Semestre Junio 2019 - Diciembre 2019
Analizador Lexico y Analizador Sintactico (Compilador)
*/
//Algunas librerias que vamos a utilizar
import scala.io.Source
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Stack 
import scala.util.parsing.combinator._
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.{Position, Positional}
import scala.collection.immutable.HashSet
import scala.io.StdIn
import scala.util.matching.Regex
import scala.util.Failure
import org.parboiled2._  // Link: https://github.com/sirthias/parboiled2#id43

object Compilador 
{
    var sTokens = ""
    // Metodo Main, hara todo, supongo... 
    def main(args:Array[String]) 
    {
      val codeVar = Source.fromFile("CodigoEjem.txt").mkString 
      try 
      {
        if (codeVar != null) 
        {
          val tokens = scan(codeVar)
          for(t<-tokens)
          {
            var temp = t.toString()
            temp = """\(([^\)]+)\)""".r.replaceAllIn(temp,"")
            println(t) //Desbloquear para comprobar que los tokens contengan las palabras correctas
            sTokens = sTokens + temp + " "
          }
        }
      }
      catch 
      {
        case e: SyntaxError =>
          println(s"SYNTAX FATAL ERROR: ${e.msg} LINE ${e.pos.line}, COLUMN ${e.pos.column}")
          if (e.pos.line > 0) 
          {
            println(codeVar.lines.toList(e.pos.line - 1))
            println("-" * (e.pos.column - 1) + "^")
          }
      }
      
      val p = new Sint(sTokens)
      if (p.instruccionPrograma.run().toString() == "Success(())"){
        println("--------COMPILATION COMPLETED---------")
      }
      else{
        val Failure(err: ParseError) = p.instruccionPrograma.run()
        println(p.formatError(err))
      }
    }
    
}
// scan alamacena todas las palabras con sus Tokens
object scan extends RegexParsers
{
    //Agrupacion de 250 palabras por sus Tokens
    override def skipWhitespace = false
    def char: Parser[Char] = ("""[^"\\]""".r | '\\' ~> ".".r) ^^ { _.head }
    def cadena: Parser[Token] = "\"" ~> rep(char) <~ "\"" ^^ (chars => Cadena(chars.mkString))
    def asignacion: Parser[Token] = """=""".r ^^ Asignacion
    def digito: Parser[Token] = """\d+(\.\d+)?""".r ^^ (s => Digito(s.toDouble))
    def tipoDato: Parser[Token] = """dtInt|dtChar|dtStrng|dtByte|dtDcmal|dtDouble|dtEnum|dtFloat|dtLng|dtSbyte|dtShort|dtStrct|dtUint|dtUlong|dtUshort""".r ^^ TipoDato
    def programaprincipal: Parser[Token] = """main|prgrm""".r ^^ ProgramaPrincipal
    def clase: Parser[Token] = """clss""".r ^^ Clase
    def sistema: Parser[Token] = """systm""".r ^^ Sistema
    def consola: Parser[Token] = """console""".r ^^ Consola
    def puntocoma: Parser[Token] = """\;""".r ^^ PuntoComa
    def coma: Parser[Token] = """\,""".r ^^ Coma
    def punto: Parser[Token] = """\.""".r ^^ Punto
    def dospuntos: Parser[Token] = """\:""".r ^^ DosPuntos 
    def llaveizq: Parser[Token] = """\{""".r ^^ LlaveIzq
    def llaveder: Parser[Token] = """\}""".r ^^ LlaveDer
    def parenizq: Parser[Token] = """\(""".r ^^ ParenIzq
    def parender: Parser[Token] = """\)""".r ^^ ParenDer
    def corchizq: Parser[Token] = """\[""".r ^^ CorchIzq
    def corchder: Parser[Token] = """\]""".r ^^ CorchDer
    def decision: Parser[Token] = """If|Swtch""".r ^^ Decision
    def comdecision: Parser[Token] = """Else|Elif|Case""".r ^^ ComDecision
    def iteracion: Parser[Token] = """Foreach|For|While""".r ^^ Iteraciones
    def comiteraciones: Parser[Token] = """In|Do""".r ^^ ComIteraciones
    def salto: Parser[Token] = """Break|Conti|Goto|Rturn|Thrw""".r ^^ Salto
    def excepcion: Parser[Token] = """Exep|exTry|exCatch|exFin""".r ^^ Excepcion
    def operaritmetico: Parser[Token] = """\+|\-|\*|\/|\%""".r ^^ OperAritmetico
    def operrelacional: Parser[Token] = (">=".r|"=<".r|"==".r|"=!".r|">".r|"<".r) ^^ OperRelacional
    def operlogicos: Parser[Token] = """\&|\^|\!|\|""".r ^^ OperLogicos
    def operlamb: Parser[Token] = ("<=".r |"=>".r) ^^ OperLamb
    def literal: Parser[Token] = """True|False|Null""".r ^^ Literal
    def reseroper: Parser[Token] = """As|Await|Is|New|Nameof|Sizeof|Typeof|Stacal""".r ^^ ReserOper
    def acceso: Parser[Token] = """acThis|acBase""".r ^^ Acceso
    def conversiones: Parser[Token] = """cmOptr|cmImpl|cmExpl""".r ^^ Conversiones
    def modificador: Parser[Token] = """modAbstr|modAsync|modConst|modEvent|modExte|modIn|modOut|modOverrd|modReadonly|modSeal|modSafe|modUsafe|modVirtual|modVolatil""".r ^^ Modificador
    def usando: Parser[Token] = """Using""".r ^^ Usando
    def namespace: Parser[Token] = """Namespace""".r ^^ Namespace
    def seleccionar: Parser[Token] = """SELECT""".r ^^ Seleccionar
    def de: Parser[Token] = """FROM""".r ^^ De
    def datosen: Parser[Token] = """IN""".r ^^ DatosEn  
    def on: Parser[Token] = """ON""".r ^^ On
    def igual: Parser[Token] = """EQUAL""".r ^^ Igual
    def agrupar: Parser[Token] = """GROUPBY""".r ^^ Agrupar
    def ascendente: Parser[Token] = """ASC""".r ^^ Ascendente
    def descendente: Parser[Token] = """DESC""".r ^^ Descendente
    def unir: Parser[Token] = """JOIN""".r ^^ Unir
    def crear: Parser[Token] = """CREATE""".r ^^ Crear
    def tabla: Parser[Token] = """TABLE""".r ^^ Tabla
    def quitar: Parser[Token] = """DROP""".r ^^ Quitar
    def insertar: Parser[Token] = """INSERT""".r ^^ Insertar  
    def contexto: Parser[Token] = """Add|Dyna|Get|Set|Glob|Parti|Remov|Value|When|Yiel""".r ^^ Contexto
    def entrasali: Parser[Token] = """Rdl|Rd|Wrl|Wr|Clean|Wait""".r ^^ EntraSali
    def exponente: Parser[Token] = """matExp""".r ^^ Exponente
    def alcuadrado: Parser[Token] = """matSqrt""".r ^^ AlCuadrado
    def logaritmo: Parser[Token] = """matLog""".r ^^ Logaritmo
    def logaritmo10: Parser[Token] = """matLog10""".r ^^ Logaritmo10
    def bmul: Parser[Token] = """matBmul""".r ^^ Bmul
    def potencia: Parser[Token] = """matPow""".r ^^ Potencia
    def raiz: Parser[Token] = """matCeil""".r ^^ Raiz
    def escala: Parser[Token] = """matScale""".r ^^ Escala
    def ieee: Parser[Token] = """matIEEE""".r ^^ IEEE
    def signo: Parser[Token] = """matSign""".r ^^ Signo
    def dividir: Parser[Token] = """matDivr""".r ^^ Dividir
    def dibujo: Parser[Token] = """DrawCurve|DrawArc|DrawBezier|DrawClosedCurve|DrawEllipse|DrawIcon|DrawIconUnstreched|DrawImage|DrawLine|DrawPie|DrawPolygon|DrawRectangle|DrawString""".r ^^ Dibujo
    def rellenar: Parser[Token] = """FillClosedCurve|FillEllipse|FillPath|FillPie|FillPolygon|FillRectangle|FillRegion""".r ^^ Rellenar 
    def matriz: Parser[Token] = """Matriz""".r ^^ Matriz
    def vector: Parser[Token] = """Vector""".r ^^ Vector
    def tipolapiz: Parser[Token] = """Pen|Brush""".r ^^ TipoLapiz
    def objeto: Parser[Token] = """obj""".r ^^ Objeto
    def color: Parser[Token] = """Red|White|Green|Black|Yellow""".r ^^ Color
    def figura: Parser[Token] = """Rectangle|Square|Triangle|Circle""".r ^^ Figura
    def hilo: Parser[Token] = """Thread""".r ^^ Hilo
    def estado: Parser[Token] = """Start|Stop|Sleep|Restore|Flush|Finalize""".r ^^ Estado
    def tipoacceso: Parser[Token] = """Priv|Public""".r ^^ TipoAcceso 
    def metodo: Parser[Token] = """void""".r ^^ Metodo
    def resto: Parser[Token] = """BeginContainer|GraphicsUnit|Clear|CopyFromScreen|CopyPixel|CopyFromObj|Size|Pnt|CreateObjRef|Dispose|Single|PntF|RectangleF|Icon|Font|EndContainer|EnumerateMetafile|ExcludeClip|IsVisible|MeasureCharacterRange|MeasureString|ScaleTransform|SetClip|TranslateClip|ArrayOf|downTo|Step|args|extends|constructor|Interface|asDynamic|Index|Iterator|Pairs|absValue|unitLastPlace|nextDown|nextTowards|roundToLong|Annotation|leftList|rightList|common|leftOnly|rightOnly|commonDirs|commonFiles|subDirs|bufferInfo|Count|FromFile|FromList|Pop|Reverse|DateTime|Date|Time|Min|Pack|PackInto|Unpack|Form|MethodType|ModuleType|FrameType|Lock|Timer|Run""".r ^^ Resto
    
    //El hash surve para registrar las palabras y no se tomen como un ID
    val notanId = HashSet("dtInt","dtChar", "dtStrng", "dtByte", "dtDcmal", "dtDouble", "dtEnum", "dtFloat", "dtLng", 
    "dtSbyte", "dtShort", "dtStrct", "dtUint", "dtUlong", "dtUshort", "main", "prgrm", "clss", "systm", "console", "If", 
    "Swtch", "Else", "Elif", "Case", "For", "Foreach", "While", "DrawCurve","In", "Do", "Break", "Conti", "Goto", "Rturn", 
    "Thrw", "Exep", "exTry", "exCatch", "exFin", "True", "False", "Null","As", "Await", "Is", "New", "Nameof", "Sizeof", 
    "Typeof", "Stacal", "acThis", "acBase", "cmOptr", "cmImpl", "cmExpl","modAbstr", "modAsync", "modConst", "modEvent", "modExte",
    "modIn", "modOut", "modOverrd", "modReadonly", "modSeal", "modSafe", "modUsafe", "modVirtual", "modVolatil", "Using", "Namespace",
    "SELECT", "FROM","IN", "ON", "GROUPBY", "ASC", "DESC", "JOIN", "CREATE", "TABLE", "DROP", "INSERT", "Add", "Dyna", "Get", "Set", 
    "Glob", "Parti", "Remov", "Value", "When", "Yiel", "Rd", "Rdl", "Wr", "Wrl", "Clean", "Wait","matExp", "matSqrt", "matLog", "matLog10", 
    "matBmul", "matPow", "matCeil", "matScale", "matIEEE", "matSign", "matDivr","DrawArc", "DrawBezier", "DrawClosedCurve", "DrawEllipse",
    "DrawIcon", "DrawIconUnstreched", "DrawImage", "DrawLine", "DrawPie", "DrawPolygon", "DrawRectangle", "DrawString", "FillClosedCurve", 
    "FillEllipse", "FillPath", "FillPie", "FillPolygon", "FillRectangle", "FillRegion", "Matriz", "Vector", "Pen", "Brush", "obj", "Red", 
    "White", "Green", "Black", "Yellow", "Rectangle", "Square", "Triangle", "Circle", "Thread", "Start", "Stop", "Sleep", "Restore", "Flush", 
    "Finalize", "Public", "Priv", "void","BeginContainer","GraphicsUnit","Clear","CopyFromScreen","CopyPixel","CopyFromObj","Size","Pnt",
    "CreateObjRef","Dispose","Single","PntF", "RectangleF","Icon","Font","EndContainer","EnumerateMetafile","ExcludeClip","IsVisible",
    "MeasureCharacterRange","MeasureString","ScaleTransform","SetClip","TranslateClip","ArrayOf","downTo","Step","args","extends","constructor",
    "Interface","asDynamic","Index","Iterator","Pairs","absValue","unitLastPlace","nextDown","nextTowards","roundToLong","Annotation","leftList",
    "rightList","common","leftOnly","rightOnly","commonDirs","commonFiles","subDirs","bufferInfo","Count","FromFile","FromList","Pop","Reverse",
    "DateTime","Date","Time","Min","Pack","PackInto","Unpack","Form","MethodType","ModuleType","FrameType","Lock","Timer","Run")


  def id: Parser[Token] = """[a-zA-Z][a-zA-Z0-9_]*""".r ^^ 
  {
    case s if notanId.contains(s) => TipoDato(s)
    case s => ID(s)
  }

  def salta: Parser[Unit] = rep(whiteSpace | comentarios) ^^^ ()
  def comentarios: Parser[Unit] = comentario | multicomentarios

  def token: Parser[Token] = positioned(entrasali | programaprincipal | dibujo | tipoacceso | cadena | digito | clase | sistema | tipolapiz |
  llaveizq | llaveder | parenizq | parender | corchizq | corchder | consola |  puntocoma | operlamb | coma | operlogicos | operrelacional | 
  operaritmetico | asignacion | punto | dospuntos | decision | comdecision | iteracion | comiteraciones | salto | excepcion | literal | reseroper | 
  acceso | conversiones | modificador | usando | namespace | seleccionar | insertar | de | datosen | on | igual | agrupar | ascendente | descendente | 
  unir | crear | tabla | quitar | contexto | exponente | alcuadrado | logaritmo | logaritmo10 | bmul | potencia | raiz | escala | ieee | signo | dividir | 
  rellenar | matriz | vector | objeto | color | figura | hilo | estado | metodo | id | tipoDato | resto ) 
  
  def program: Parser[List[Token]] = salta ~> rep(token <~ salta) <~ endLine
  def endLine: Parser[String] = "\\z".r | failure("MISTAKE ON CHARACTER")

  def comentario: Parser[Unit] = "//" ~ rep(not("\n") ~ ".".r) ^^^ ()
  def multicomentarios: Parser[Unit] = "/*" ~ rep(not("*/") ~ "(?s).".r) ~ "*/" ^^^ ()
 
  def apply(codeVar: String): List[Token] = parseAll(program, codeVar) match {
    case Success(result, _) => result
    case NoSuccess(msg, next) => throw SyntaxError(msg, next.pos)
  }

  
}
class Sint(val input: ParserInput) extends Parser
{
  //Reglas
  //Bloque de Instrucciones
  def BloqueInst = rule {oneOrMore(instruccionRead | instruccionLimpiar | unirTablas | instruccionSeleccionar | crearTabla | quitarTabla | instruccionHilos | 
  crearGrafico | instruccionDibujo | instruccionRellenar | cicloForeach | matrizBidi | declararVector | crearPincel | declararVar |instanciarObj | doWhile | 
  cicloFor | iterWhile | decisionIf ).separatedBy(WS) }
  def BloqueInstIt = rule {oneOrMore(instruccionRead | instruccionLimpiar | unirTablas | instruccionSeleccionar | crearTabla | quitarTabla | instruccionHilos | 
  crearGrafico | instruccionDibujo | instruccionRellenar | cicloForeach | matrizBidi | declararVector | crearPincel | declararVar |instanciarObj).separatedBy(WS)}

  //1 
  def instruccionPrograma = rule {atomic("Usando") ~ WS ~ atomic("ID") ~ WS ~ atomic("PuntoComa") ~ WS ~ atomic("Namespace") ~ WS ~ atomic("ID") ~ WS ~ atomic("LlaveIzq") ~ WS ~ 
  Clases.named("instruccionClase") ~ WS ~ atomic("LlaveDer") ~ WS ~ EOI.named("Fin de programa")}
  
  //2 En Clases pueden ir 1 o mas Clases,Metodos,Funciones,Bloque de instrucciones o Try&Catch
  def instruccionClase = rule {atomic("TipoAcceso") ~ WS ~ atomic("Clase") ~ WS ~ atomic("ID") ~ WS ~ atomic("LlaveIzq") ~ WS ~ oneOrMore(funcion.named("Funcion") | metodos.named("Metodo") | 
  BloqueInst.named("BloqueInstrucciones") | excepcionTry.named("Excepcion")).separatedBy(WS) ~ WS ~ atomic("LlaveDer")}
  def Clases = rule {oneOrMore(instruccionClase).separatedBy(WS)}
  
  //3
  def instruccionRead = rule {atomic("EntraSali") ~ WS ~ atomic("DosPuntos") ~ WS ~ atomic("Cadena") ~ WS ~ atomic("PuntoComa") }
  
  //4
  def instruccionLimpiar = rule {atomic("EntraSali") ~ WS ~ atomic("PuntoComa")}
  
  //5
  def instruccionSeleccionar = rule {atomic("Seleccionar") ~ WS ~ atomic("ID") ~ WS ~ atomic("De") ~ WS ~ atomic("ID")}
  
  //6
  def crearTabla = rule {atomic{"Crear"} ~ WS ~ atomic{"Tabla"} ~ WS ~ atomic{"ID"}}
  
  //7 
  def quitarTabla = rule {atomic("Quitar") ~ WS ~ atomic("Tabla") ~ WS ~ atomic{"ID"}}
  
  //8
  def instruccionHilos = rule { atomic("ReserOper") ~ WS ~ atomic("Hilo") ~ WS ~ atomic("ID") ~ WS ~ atomic("CorchIzq") ~ WS ~ atomic("Estado") ~ WS ~ atomic("CorchDer") ~ WS ~ 
  atomic("PuntoComa")}
  
  //9
  def crearGrafico = rule { atomic("Figura") ~ WS ~ atomic("ID") ~ WS ~ atomic("Asignacion") ~ WS ~ atomic("ReserOper") ~ WS ~ atomic("Figura") ~ WS ~ atomic("CorchIzq") ~ WS ~ 
  atomic("TipoLapiz") ~ WS ~ atomic("CorchDer") ~ WS ~ atomic("PuntoComa")}
  
  //10
  def instruccionDibujo = rule {atomic("Dibujo") ~ WS ~ atomic("CorchIzq") ~ WS ~ atomic("TipoLapiz") ~ WS ~ atomic("Coma") ~ WS ~ atomic("Digito") ~ WS ~ atomic("Coma") ~ WS ~ 
  atomic("Digito") ~ WS ~ atomic("CorchDer") ~ WS ~ atomic("PuntoComa") }
  
  //11
  def instruccionRellenar = rule {atomic("Rellenar") ~ WS ~ atomic("CorchIzq") ~ WS ~ atomic("TipoLapiz") ~ WS ~ atomic("Coma") ~ WS ~ atomic("Digito") ~ WS ~ 
  atomic("Coma") ~ WS ~ atomic("Digito") ~ WS ~ atomic("CorchDer") ~ WS ~ atomic("PuntoComa")} 
  
  //12
  def cicloForeach = rule {atomic("Iteraciones") ~ WS ~ atomic("CorchIzq") ~ WS ~ atomic("ID") ~ WS ~ atomic("OperLamb") ~ WS ~ atomic("ID") ~ WS ~ atomic("CorchDer") ~ WS ~ 
  atomic("PuntoComa")}
  
  //13
  def matrizBidi = rule {atomic("Matriz") ~ WS ~ atomic("Punto") ~ WS ~ atomic("TipoDato") ~ WS ~ atomic("CorchIzq") ~ WS ~ atomic("ID") ~ WS ~ atomic("CorchDer") ~ WS ~ 
  atomic("DosPuntos") ~ WS ~ atomic("CorchIzq") ~ WS ~ (atomic("ID")|atomic("Digito")) ~ WS ~ atomic("CorchDer") ~ WS ~ atomic("CorchIzq") ~ WS ~ (atomic("ID")|atomic("Digito")) ~ WS ~ atomic("CorchDer") ~ WS ~ atomic("PuntoComa")}
  
  //14
  def declararVector = rule {atomic("Vector") ~ WS ~ atomic("Punto") ~ WS ~ atomic("TipoDato") ~ WS ~ atomic("CorchIzq") ~ WS ~ atomic("ID") ~ WS ~ atomic("CorchDer") ~ WS ~ 
  atomic("DosPuntos") ~ WS ~ atomic("CorchIzq") ~ WS ~ (atomic("ID")|atomic("Digito")) ~ WS ~ atomic("CorchDer") ~ WS ~ atomic("PuntoComa")}
  
  //15
  def crearPincel = rule {atomic("TipoLapiz") ~ WS ~ atomic("ID") ~ WS ~ atomic("Asignacion") ~ WS ~ atomic("ReserOper") ~ WS ~ atomic("Punto") ~ WS ~ atomic("Objeto") ~ WS ~ 
  atomic("TipoLapiz") ~ WS ~ atomic("CorchIzq") ~ WS ~ atomic("Color") ~ WS ~ atomic("Coma") ~ WS ~ atomic("Digito") ~ WS ~ atomic("CorchDer") ~ WS ~ atomic("PuntoComa")}
  
  //16
  def unirTablas = rule {instruccionSeleccionar.named("instruccionSeleccionar") ~ WS ~ atomic("Unir") ~ WS ~ instruccionSeleccionar.named("instruccionSeleccionar")}
  
  //17 Dentro de una funcion no puede ir otra funcion, metodo o Try&Catch
  def funcion = rule {atomic("TipoAcceso") ~ WS ~ atomic("TipoDato") ~ WS ~ atomic("ID") ~ WS ~ parametros.named("Parametros") ~ WS ~ atomic("LlaveIzq") ~ WS ~ 
  BloqueInst.named("BloqueInstrucciones") ~ WS ~ atomic("LlaveDer")}
  
  //18 Dentro de un metodo no puede ir otro metodo, funcion o try&catch
  def metodos = rule {atomic("TipoAcceso") ~ WS ~ atomic("Metodo") ~ WS ~ atomic("ID") ~ WS ~ parametros.named("Parametros") ~ WS ~ atomic("LlaveIzq") ~ WS ~ 
  BloqueInst.named("BloqueInstrucciones") ~ WS ~ atomic("LlaveDer")}
  
  //19
  def excepcionTry = rule {atomic("Excepcion") ~ WS ~ atomic("LlaveIzq") ~ WS ~ BloqueInst.named("BloqueInstrucciones") ~ WS ~ atomic("LlaveDer") ~ WS ~ 
  atomic("Excepcion") ~ WS ~ atomic("LlaveIzq") ~ WS ~ BloqueInst.named("BloqueInstrucciones") ~ WS ~ atomic("LlaveDer")}
  
  //20 Dentro de DoWhile no puede ir un DoWhile,For,While o If.
  def doWhile = rule {atomic("ComIteraciones") ~ WS ~ atomic("LlaveIzq") ~ WS ~ BloqueInstIt.named("BloqueInstruccionesIt") ~ WS ~ atomic("LlaveDer") ~ WS ~ 
  atomic("Iteraciones") ~ WS ~ condiciones.named("Condiciones") ~ WS ~ atomic("PuntoComa") }
  
  //21
  def declararVar = rule {atomic("TipoAcceso") ~ WS ~ atomic("TipoDato") ~ WS ~ atomic("ID") ~ WS ~ atomic("Asignacion") ~ WS ~ 
  (atomic("Digito") | atomic("Cadena") | atomic("ID")) ~ WS ~ atomic("PuntoComa")}
  
  //22
  def instanciarObj = rule {atomic("Objeto") ~ WS ~ atomic("ID") ~ WS ~ atomic("Asignacion") ~ WS ~ atomic("ReserOper") ~ WS ~ atomic("Objeto") ~ WS ~ atomic("PuntoComa")}
  
  //23 Dentro de For no puede ir un DoWhile,For,While o If
  def cicloFor = rule {atomic("Iteraciones") ~ WS ~ atomic("ID") ~ WS ~ atomic("ComIteraciones") ~ WS ~ atomic("CorchIzq") ~ WS ~ (atomic("ID")|atomic("Digito")) ~ WS ~ 
  atomic("Coma") ~ WS ~ (atomic("ID")|atomic("Digito")) ~ WS ~ atomic("Coma") ~ WS ~ (atomic("ID")|atomic("Digito")) ~ WS ~ atomic("CorchDer") ~ WS ~ atomic("LlaveIzq") ~ WS ~ BloqueInstIt.named("BloqueInstruccionesIt") ~ WS ~ atomic("LlaveDer")}
  
  //24 Dentro de While no puede ir un DoWhile,For,While o If
  def iterWhile = rule {atomic("Iteraciones") ~ WS ~ condiciones.named("Condiciones") ~ WS ~ atomic("LlaveIzq") ~ WS ~ BloqueInstIt.named("BloqueInstruccionesIt") ~ WS ~ 
  atomic("LlaveDer") }
  
  //25 Dentro de If no puede ir un DoWhile,For,While o If, Un If puede tener 0 o 1 Else, Dentro de Else tampoco puede ir un DoWhile,For,While o If
  def decisionIf = rule {atomic("Decision") ~ WS ~ condiciones.named("Condiciones") ~ WS ~ atomic("LlaveIzq") ~ WS ~ BloqueInstIt.named("BloqueInstruccionesIt") ~ WS ~ 
  atomic("LlaveDer") ~ WS ~ zeroOrMore(comElse).separatedBy(WS)}

  //Entre cada palabras puede haber ningun, uno o mas de un espacio
  def WS = rule { quiet(zeroOrMore(anyOf(" \t \n"))) }

  //Producciones
  def parametro = rule {atomic("ID") ~ WS ~ atomic("DosPuntos") ~ WS ~ atomic("TipoDato")}
  def masparametro = rule {atomic("Coma") ~ WS ~ parametro.named("Parametro")}
  def parametros = rule {atomic("CorchIzq") ~ WS ~ parametro.named("Parametro") ~ WS ~ zeroOrMore(masparametro).separatedBy(WS) ~ WS ~ atomic("CorchDer")}

  def condicion = rule {atomic("ID") ~ WS ~ atomic("OperRelacional") ~ WS ~ (atomic("ID")|atomic("Digito")|atomic("Cadena"))}
  //def mascondicion = rule {atomic("OperLogico") ~ WS ~ condicion.named("Condicion")}
  def condiciones = rule {condicion.named("Condicion") /*~ WS ~ zeroOrMore(mascondicion).separatedBy(WS)*/}

  //def comElif = rule { atomic("ComDecision") ~ WS ~ condiciones.named("Condiciones") ~ WS ~ atomic("LlaveIzq") ~ WS ~ BloqueInstIt.named("BloqueInstruccionesIy") ~ WS ~ atomic("LlaveDer") ~ WS ~ oneOrMore(comElif|comElse).separatedBy(WS)}
  def comElse = rule { atomic("ComDecision") ~ WS ~ atomic("LlaveIzq") ~ WS ~ BloqueInstIt.named("BloqueInstruccionesIt") ~ WS ~ atomic("LlaveDer")}

}
//Clases Tokens
abstract sealed class Token extends Positional
case class Cadena(value: String) extends Token
case class Asignacion(value: String) extends Token
case class Digito(value: Double) extends Token
case class ID(id: String) extends Token
case class TipoDato (tipoDato: String) extends Token
case class Comentario(comentario: String) extends Token
case class SyntaxError(msg: String, pos: Position) extends Exception(msg)
case class ProgramaPrincipal (programaprincipal: String) extends Token
case class Clase (clase: String) extends Token
case class Sistema (sistema: String) extends Token
case class Consola (consola: String) extends Token
case class PuntoComa (puntocoma: String)extends Token
case class Coma (coma: String)extends Token
case class Punto (punto: String)extends Token
case class DosPuntos (dospuntos: String) extends Token
case class Decision (decision: String)extends Token
case class ComDecision (comdecision: String)extends Token
case class Iteraciones (iteracion: String)extends Token
case class ComIteraciones (comiteraciones: String)extends Token
case class Salto (salto: String)extends Token
case class Excepcion (excepcion: String)extends Token
case class OperAritmetico (operaritmetico: String)extends Token
case class OperRelacional (operrelacional: String)extends Token
case class OperLogicos (operlogicos: String)extends Token
case class OperLamb (operlamb: String)extends Token
case class LlaveIzq (llaveizq: String)extends Token
case class LlaveDer (llaveder: String)extends Token
case class ParenIzq (parenizq: String)extends Token
case class ParenDer (parender: String)extends Token
case class CorchIzq (corchizq: String)extends Token
case class CorchDer (corchder: String)extends Token
case class Literal (literal: String)extends Token
case class ReserOper (reseroper: String)extends Token
case class Acceso (acceso: String)extends Token
case class Conversiones (conversiones: String)extends Token
case class Modificador (modificador: String)extends Token
case class Usando (usando: String)extends Token
case class Namespace (namespace: String)extends Token
case class Seleccionar (seleccionar: String)extends Token
case class De (de: String)extends Token
case class DatosEn (datosen: String)extends Token
case class On (on: String)extends Token
case class Igual (igual: String)extends Token
case class Agrupar (agrupar: String)extends Token
case class Ascendente (ascendente: String)extends Token
case class Descendente (descendente: String)extends Token
case class Unir (unir: String)extends Token
case class Crear (crear: String)extends Token
case class Tabla (tabla: String)extends Token
case class Quitar (quitar: String)extends Token
case class Insertar (insertar: String)extends Token
case class Contexto (contexto: String)extends Token
case class EntraSali (entrasali: String)extends Token
case class Exponente (exponente: String)extends Token
case class AlCuadrado (alcuadrado: String)extends Token
case class Logaritmo (logaritmo: String)extends Token
case class Logaritmo10 (logaritmo10: String)extends Token
case class Bmul (bmul: String)extends Token
case class Potencia (potencia: String)extends Token
case class Raiz (raiz: String)extends Token
case class Escala (escala: String)extends Token
case class IEEE (ieee: String)extends Token
case class Signo (signo: String)extends Token
case class Dividir (dividir: String)extends Token
case class Dibujo (dibujo: String)extends Token
case class Rellenar (rellenar: String)extends Token
case class Matriz (matriz: String)extends Token
case class Vector (vector: String)extends Token
case class TipoLapiz (tipolapiz: String)extends Token
case class Objeto (objeto: String)extends Token
case class Color (color: String)extends Token
case class Figura (figura: String)extends Token
case class Hilo (hilo: String)extends Token
case class Estado (estado: String)extends Token
case class TipoAcceso (tipoacceso: String) extends Token
case class Metodo (metodo: String) extends Token
case class Resto (resto: String) extends Token
 



