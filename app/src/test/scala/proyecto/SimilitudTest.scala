package proyecto

import org.scalatest.funsuite.AnyFunSuite
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner
import Similitud._

/**
 * Pruebas de contrato de la versión secuencial: una por función, con los
 * valores de los dos ejemplos del enunciado. No se modifican.
 *
 * La rúbrica pide además cinco casos propios por función, distintos de estos y
 * de los ejemplos del enunciado. Van al final de este archivo o en un archivo
 * nuevo de esta misma carpeta; las dos formas valen.
 */
@RunWith(classOf[JUnitRunner])
class SimilitudTest extends AnyFunSuite {

  val eps = 1e-9

  // Ejemplo 1 del enunciado, con las palabras ya normalizadas para que estas
  // pruebas no dependan unas de otras.
  val d0: Documento = ("d0", Vector("el", "gato", "duerme", "en", "el", "sofa"))
  val d1: Documento = ("d1", Vector("el", "gato", "duerme", "en", "la", "alfombra"))
  val d2: Documento = ("d2", Vector("la", "lluvia", "moja", "el", "parque"))
  val c1: Corpus = Vector(d0, d1, d2)

  // Ejemplo 2 del enunciado.
  val e0: Documento = ("e0", Vector("la", "funcion", "recursiva", "termina",
    "cuando", "llega", "al", "caso", "base"))
  val e1: Documento = ("e1", Vector("toda", "funcion", "recursiva", "termina",
    "cuando", "llega", "al", "caso", "base"))
  val e2: Documento = ("e2", Vector("el", "paralelismo", "divide", "el",
    "trabajo", "en", "tareas", "independientes"))
  val e3: Documento = ("e3", Vector("el", "paralelismo", "divide", "el",
    "trabajo", "en", "hilos", "independientes"))
  val c2: Corpus = Vector(e0, e1, e2, e3)

  test("normalizar: baja a minúsculas y parte en palabras") {
    assert(normalizar("El Gato Duerme") == Vector("el", "gato", "duerme"))
  }

  test("fragmentos: son las ventanas consecutivas, en orden") {
    val ts = Vector("a", "b", "c", "d")
    assert(fragmentos(ts, 2) ==
      Vector(Vector("a", "b"), Vector("b", "c"), Vector("c", "d")))
  }

  test("huella: el ejemplo 1 tiene 4, 4 y 3 fragmentos") {
    assert(huella(d0, 3).size == 4)
    assert(huella(d1, 3).size == 4)
    assert(huella(d2, 3).size == 3)
  }

  test("huellas: hay una huella por documento") {
    assert(huellas(c1, 3).length == 3)
  }

  test("jaccard: ejemplo 2, las dos parejas dan 0.75 y 0.5") {
    assert(math.abs(jaccard(huella(e0, 3), huella(e1, 3)) - 0.75) < eps)
    assert(math.abs(jaccard(huella(e2, 3), huella(e3, 3)) - 0.5) < eps)
  }

  test("parejas: hay n (n - 1) / 2") {
    assert(parejas(5).length == 10)
  }

  test("similitudes: el ejemplo 1 da 1/3, 0 y 0") {
    val hs = huellas(c1, 3)
    val ms = similitudes(hs, parejas(3)).map(_._3)
    assert(math.abs(ms(0) - 2.0 / 6.0) < eps && ms(1) == 0.0 && ms(2) == 0.0)
  }

  test("indiceInvertido: el fragmento inicial de d0 está en d0 y en d1") {
    val idx = indiceInvertido(huellas(c1, 3))
    assert(idx(Vector("el", "gato", "duerme")) == Set(0, 1))
  }

  test("parejasCandidatas: en el ejemplo 2 quedan dos de seis") {
    val idx = indiceInvertido(huellas(c2, 3))
    assert(parejasCandidatas(idx) == Vector((0, 1), (2, 3)))
  }

  test("ranking: ordena de mayor a menor y trunca a p") {
    val ms: Vector[Medicion] = Vector((0, 1, 0.4), (0, 2, 0.1), (1, 2, 0.9))
    assert(ranking(ms, 0.0, 10).map(_._3) == Vector(0.9, 0.4, 0.1))
    assert(ranking(ms, 0.0, 2).length == 2)
  }

  test("detectar: el ejemplo 2 reporta (0, 1) con 0.75 y (2, 3) con 0.5") {
    val r = detectarDirecto(c2, 3, 0.3, 5)
    assert(r.map(m => (m._1, m._2)) == Vector((0, 1), (2, 3)))
    assert(math.abs(r(0)._3 - 0.75) < eps && math.abs(r(1)._3 - 0.5) < eps)
  }

  test("detectar: las dos soluciones coinciden en los ejemplos") {
    assert(detectarDirecto(c1, 3, 0.2, 5) == detectarConIndice(c1, 3, 0.2, 5))
    assert(detectarDirecto(c2, 3, 0.3, 5) == detectarConIndice(c2, 3, 0.3, 5))
  }



  // _________________________ TEST ADICIONALES ___________________________

  // normalizar: 5 casos propios

  test("normalizar propio 1: la puntuación se vuelve separador") {
    assert(normalizar("Hola, mundo!") == Vector("hola", "mundo"))
  }

  test("normalizar propio 2: espacios repetidos, tabs y saltos no dejan vacías") {
    assert(normalizar("  uno   dos\tTres\ncuatro ") ==
      Vector("uno", "dos", "tres", "cuatro"))
  }

  test("normalizar propio 3: cadena vacía o solo símbolos da vector vacío") {
    assert(normalizar("") == Vector())
    assert(normalizar("¡¿...?! --- ;;") == Vector())
  }

  test("normalizar propio 4: los dígitos se conservan como parte de la palabra") {
    assert(normalizar("Ley 100 de 1993") == Vector("ley", "100", "de", "1993"))
  }

  test("normalizar propio 5: acentos y ñ se conservan; guion y apóstrofe separan") {
    assert(normalizar("Canción, MAÑANA") == Vector("canción", "mañana"))
    assert(normalizar("bien-estar don't") == Vector("bien", "estar", "don", "t"))
  }

  // fragmentos: 5 casos

  test("fragmentos propio 1: con k = 1 cada palabra es un fragmento") {
    assert(fragmentos(Vector("a", "b", "c"), 1) ==
      Vector(Vector("a"), Vector("b"), Vector("c")))
  }

  test("fragmentos propio 2: con k = t hay un único fragmento, el documento") {
    assert(fragmentos(Vector("a", "b", "c"), 3) == Vector(Vector("a", "b", "c")))
  }

  test("fragmentos propio 3: con k mayor que t no hay fragmentos") {
    assert(fragmentos(Vector("a", "b"), 3) == Vector())
  }

  test("fragmentos propio 4: un documento vacío no tiene fragmentos") {
    assert(fragmentos(Vector(), 2) == Vector())
  }

  test("fragmentos propio 5: los repetidos se conservan y hay t - k + 1") {
    val fs = fragmentos(Vector("x", "x", "x", "x"), 2)
    assert(fs.length == 3)
    assert(fs.forall(_ == Vector("x", "x")))
  }

  // huella: 5 casos

  test("huella propio 1: un fragmento repetido cuenta una sola vez") {
    val h = huella(("r", Vector("a", "a", "a", "a")), 2)
    assert(h.size == 1)
    assert(h == Set(Vector("a", "a")))
  }

  test("huella propio 2: con menos de k palabras la huella es vacía") {
    assert(huella(("corto", Vector("a", "b")), 3) == Set())
  }

  test("huella propio 3: el contenido exacto de un documento de tres palabras") {
    assert(huella(("y", Vector("a", "b", "c")), 2) ==
      Set(Vector("a", "b"), Vector("b", "c")))
  }

  test("huella propio 4: con k = 1 es el conjunto de palabras distintas") {
    assert(huella(("z", Vector("a", "b", "a", "c")), 1) ==
      Set(Vector("a"), Vector("b"), Vector("c")))
  }

  test("huella propio 5: ignora el id pero el orden de las palabras sí cuenta") {
    val w = Vector("a", "b", "c", "d")
    assert(huella(("uno", w), 2) == huella(("otro", w), 2))
    assert(huella(("uno", w), 2) != huella(("uno", w.reverse), 2))
  }

  // huellas: 5 casos

  test("huellas propio 1: un corpus vacío da un vector vacío") {
    assert(huellas(Vector(), 3) == Vector())
  }

  test("huellas propio 2: la huella i corresponde al documento i") {
    val hs = huellas(c1, 3)
    assert(c1.indices.forall(i => hs(i) == huella(c1(i), 3)))
  }

  test("huellas propio 3: un documento corto da huella vacía sin afectar a los demás") {
    val c: Corpus = Vector(("corto", Vector("x")), d0)
    val hs = huellas(c, 3)
    assert(hs(0).isEmpty)
    assert(hs(1).size == 4)
  }

  test("huellas propio 4: el ejemplo 2 tiene 7, 7, 6 y 6 fragmentos") {
    assert(huellas(c2, 3).map(_.size) == Vector(7, 7, 6, 6))
  }

  test("huellas propio 5: documentos con las mismas palabras tienen huellas iguales") {
    val hs = huellas(Vector(d0, ("copia", d0._2)), 3)
    assert(hs(0) == hs(1))
  }

  // jaccard: 5 casos

  test("jaccard propio 1: huellas vacías dan cero, sin dividir por cero") {
    val vacia: Huella = Set()
    assert(jaccard(vacia, vacia) == 0.0)
    assert(jaccard(vacia, huella(d0, 3)) == 0.0)
  }

  test("jaccard propio 2: huellas idénticas dan uno") {
    assert(math.abs(jaccard(huella(d0, 3), huella(d0, 3)) - 1.0) < eps)
  }

  test("jaccard propio 3: huellas disjuntas dan cero") {
    assert(jaccard(huella(d0, 3), huella(d2, 3)) == 0.0)
  }

  test("jaccard propio 4: es simétrica y d0 con d1 da 1/3") {
    val a = huella(d0, 3)
    val b = huella(d1, 3)
    assert(math.abs(jaccard(a, b) - jaccard(b, a)) < eps)
    assert(math.abs(jaccard(a, b) - 1.0 / 3.0) < eps)
  }

  test("jaccard propio 5: una huella contenida en otra da el cociente de tamaños") {
    val pequena: Huella = Set(Vector("a"), Vector("b"))
    val grande: Huella = Set(Vector("a"), Vector("b"), Vector("c"), Vector("d"))
    assert(math.abs(jaccard(pequena, grande) - 0.5) < eps)
  }

  // parejas: 5 casos

  test("parejas propio 1: con 0 o 1 documentos no hay parejas") {
    assert(parejas(0) == Vector())
    assert(parejas(1) == Vector())
  }

  test("parejas propio 2: con 2 documentos solo está (0, 1)") {
    assert(parejas(2) == Vector((0, 1)))
  }

  test("parejas propio 3: el contenido exacto para n = 4") {
    assert(parejas(4) ==
      Vector((0, 1), (0, 2), (0, 3), (1, 2), (1, 3), (2, 3)))
  }

  test("parejas propio 4: todas cumplen i < j, están en rango y no se repiten") {
    val ps = parejas(6)
    assert(ps.forall(p => 0 <= p._1 && p._1 < p._2 && p._2 < 6))
    assert(ps.distinct.length == ps.length)
  }

  test("parejas propio 5: la cantidad es n (n - 1) / 2 y salen ordenadas") {
    assert((0 to 10).forall(n => parejas(n).length == n * (n - 1) / 2))
    assert(parejas(7) == parejas(7).sorted)
    assert(parejas(5).head == (0, 1) && parejas(5).last == (3, 4))
  }
}
