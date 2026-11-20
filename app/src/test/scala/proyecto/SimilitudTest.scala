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

  //Similitudes 5 casos

  test("similitudes propio 1: mide solo las parejas pedidas y en el orden pedido") {
    val hs = huellas(c1, 3)
    val r = similitudes(hs, Vector((1, 2), (0, 1)))
    assert(r.length == 2)
    assert(r(0)._1 == 1 && r(0)._2 == 2 && r(0)._3 == 0.0)
    assert(r(1)._1 == 0 && r(1)._2 == 1 && math.abs(r(1)._3 - (1.0 / 3.0)) < eps)
  }
 
  test("similitudes propio 2: ejemplo 2 completo, 0.75 y 0.5 y el resto en cero") {
    val r = similitudes(huellas(c2, 3), parejas(4))
    assert(r.map(m => (m._1, m._2)) == parejas(4))
    val esperado = Vector(0.75, 0.0, 0.0, 0.0, 0.0, 0.5)
    assert(r.map(_._3).zip(esperado).forall { case (a, b) => math.abs(a - b) < eps })
  }
 
  test("similitudes propio 3: usa la huella de la posición i y de la j") {
    // Huellas armadas a mano, con tamaños distintos para que un índice
    // equivocado cambie el resultado.
    val fa = Vector("a")
    val fb = Vector("b")
    val fc = Vector("c")
    val fd = Vector("d")
    val hs: Vector[Huella] = Vector(Set(fa, fb), Set(fa), Set(fc, fd, fa))
    val r = similitudes(hs, parejas(3))
    assert(r.map(m => (m._1, m._2)) == Vector((0, 1), (0, 2), (1, 2)))
    assert(math.abs(r(0)._3 - (1.0 / 2.0)) < eps)  // 1 / (2 + 1 - 1)
    assert(math.abs(r(1)._3 - (1.0 / 4.0)) < eps)  // 1 / (2 + 3 - 1)
    assert(math.abs(r(2)._3 - (1.0 / 3.0)) < eps)  // 1 / (1 + 3 - 1)
  }
 
  test("similitudes propio 4: las huellas vacías dan 0.0 y nunca NaN") {
    val hs: Vector[Huella] = Vector(Set(), Set(), Set(Vector("x")))
    val r = similitudes(hs, parejas(3))
    assert(r.length == 3)
    assert(r.forall(m => m._3 == 0.0 && !m._3.isNaN))
  }
 
  test("similitudes propio 5: una copia exacta da 1.0 y se mantiene el resto") {
    val c: Corpus = Vector(d0, ("copia", d0._2), d1)
    val r = similitudes(huellas(c, 3), parejas(3))
    assert(r.map(m => (m._1, m._2)) == Vector((0, 1), (0, 2), (1, 2)))
    assert(math.abs(r(0)._3 - 1.0) < eps)
    assert(math.abs(r(1)._3 - (1.0 / 3.0)) < eps)
    assert(math.abs(r(2)._3 - (1.0 / 3.0)) < eps)
  }

  //Indice Invertido

  test("indiceInvertido propio 1: ejemplo 1 tiene 9 fragmentos distintos") {
    val idx = indiceInvertido(huellas(c1, 3))
    assert(idx.size == 9)  // 4 + 4 + 3 fragmentos, 2 compartidos
    assert(idx(Vector("el", "gato", "duerme")) == Set(0, 1))
    assert(idx(Vector("gato", "duerme", "en")) == Set(0, 1))
    assert(idx(Vector("en", "el", "sofa")) == Set(0))
    assert(idx(Vector("duerme", "en", "la")) == Set(1))
    assert(idx(Vector("moja", "el", "parque")) == Set(2))
  }
 
  test("indiceInvertido propio 2: guarda la posición en el corpus, no otra") {
    val f1 = Vector("a")
    val f2 = Vector("b")
    val f3 = Vector("c")
    val f4 = Vector("d")
    val hs: Vector[Huella] = Vector(Set(f1, f2), Set(f3), Set(f1, f4))
    val idx = indiceInvertido(hs)
    assert(idx(f1) == Set(0, 2))
    assert(idx(f2) == Set(0))
    assert(idx(f3) == Set(1))
    assert(idx(f4) == Set(2))
  }
 
  test("indiceInvertido propio 3: un fragmento repetido dentro de un documento no duplica nada") {
    val c: Corpus = Vector(("r", Vector("a", "a", "a", "a")),
      ("s", Vector("a", "a", "a")))
    val idx = indiceInvertido(huellas(c, 2))
    assert(idx == Map(Vector("a", "a") -> Set(0, 1)))
  }
 
  test("indiceInvertido propio 4: sin huellas o con huellas vacías el índice es vacío") {
    assert(indiceInvertido(Vector[Huella]()).isEmpty)
    assert(indiceInvertido(Vector[Huella](Set(), Set())).isEmpty)
    // Documentos más cortos que k no aportan fragmentos
    assert(indiceInvertido(huellas(Vector(("c", Vector("a", "b"))), 3)).isEmpty)
  }
 
  test("indiceInvertido propio 5: g está en la huella de i si y solo si i está en idx(g)") {
    val hs = huellas(c2, 3)
    val idx = indiceInvertido(hs)
    // Las llaves son exactamente los fragmentos que aparecen en alguna huella
    assert(idx.keySet == hs.flatMap(h => h).toSet)
    // Ida: cada fragmento de cada huella apunta de vuelta a su documento
    assert(hs.indices.forall(i => hs(i).forall(g => idx(g).contains(i))))
    // Vuelta: todo documento del conjunto realmente contiene el fragmento
    assert(idx.forall { case (g, docs) => docs.forall(i => hs(i).contains(g)) })
  }

  //ParejasCandidatas

  test("parejasCandidatas propio 1: una pareja que comparte varios fragmentos sale una vez") {
    val idx: Indice = Map(
      Vector("a") -> Set(0, 1),
      Vector("b") -> Set(0, 1),
      Vector("c") -> Set(0, 1))
    assert(parejasCandidatas(idx) == Vector((0, 1)))
  }
 
  test("parejasCandidatas propio 2: tres documentos en un fragmento dan tres parejas ordenadas con i < j") {
    // El conjunto se arma desordenado a propósito
    val idx: Indice = Map(Vector("a") -> Set(2, 0, 1))
    assert(parejasCandidatas(idx) == Vector((0, 1), (0, 2), (1, 2)))
  }
 
  test("parejasCandidatas propio 3: los fragmentos de un solo documento no generan parejas") {
    val idx: Indice = Map(
      Vector("a") -> Set(0),
      Vector("b") -> Set(1, 3),
      Vector("c") -> Set(2))
    assert(parejasCandidatas(idx) == Vector((1, 3)))
    assert(parejasCandidatas(Map[Fragmento, Set[Int]]()) == Vector())
  }
 
  test("parejasCandidatas propio 4: no se asume transitividad, (0, 2) no aparece") {
    val idx: Indice = Map(
      Vector("a") -> Set(0, 1),
      Vector("b") -> Set(1, 2))
    assert(parejasCandidatas(idx) == Vector((0, 1), (1, 2)))
  }
 
  test("parejasCandidatas propio 5: toda pareja que no es candidata tiene Jaccard 0") {
    // Es la propiedad que sostiene el diseño: el índice solo descarta ceros
    val casos: Vector[(Corpus, Int)] = Vector((c1, 3), (c2, 3), (c2, 1))
    assert(casos.forall { case (c, k) =>
      val hs = huellas(c, k)
      val cands = parejasCandidatas(indiceInvertido(hs))
      val descartadas = parejas(hs.length).filterNot(p => cands.contains(p))
      descartadas.forall(p => jaccard(hs(p._1), hs(p._2)) == 0.0) &&
        cands.forall(p => jaccard(hs(p._1), hs(p._2)) > 0.0)
    })
  }

  //Ranking

  test("ranking propio 1: el umbral es inclusivo, lo que vale justo el umbral se queda") {
    val ss: Vector[Medicion] = Vector((0, 1, 0.5), (0, 2, 0.49), (1, 2, 0.51))
    assert(ranking(ss, 0.5, 10) == Vector((1, 2, 0.51), (0, 1, 0.5)))
  }
 
  test("ranking propio 2: los empates se rompen por (i, j), sin depender del orden de entrada") {
    val ss: Vector[Medicion] =
      Vector((2, 3, 0.5), (0, 2, 0.5), (0, 1, 0.5), (1, 2, 0.5))
    val esperado: Vector[Medicion] =
      Vector((0, 1, 0.5), (0, 2, 0.5), (1, 2, 0.5), (2, 3, 0.5))
    assert(ranking(ss, 0.0, 10) == esperado)
    assert(ranking(ss.reverse, 0.0, 10) == esperado)
  }
 
  test("ranking propio 3: el tope p se aplica después de filtrar y ordenar") {
    // Si se truncara antes de filtrar, con p = 1 quedaría (0, 1, 0.1) y luego
    // se filtraría hasta dejar el vector vacío.
    val ss: Vector[Medicion] =
      Vector((0, 1, 0.1), (0, 2, 0.9), (1, 2, 0.8), (1, 3, 0.05))
    assert(ranking(ss, 0.5, 1) == Vector((0, 2, 0.9)))
    // Con p mayor que los que pasan el umbral no rellena
    assert(ranking(ss, 0.5, 5) == Vector((0, 2, 0.9), (1, 2, 0.8)))
  }
 
  test("ranking propio 4: umbrales extremos, tope cero y entrada vacía") {
    val ss: Vector[Medicion] =
      Vector((0, 1, 1.0), (0, 2, 0.0), (1, 2, 0.6), (1, 3, 0.0))
    // Con umbral 0.0 entran también las de similitud cero
    assert(ranking(ss, 0.0, 10) ==
      Vector((0, 1, 1.0), (1, 2, 0.6), (0, 2, 0.0), (1, 3, 0.0)))
    // Con umbral 1.0 solo las copias exactas
    assert(ranking(ss, 1.0, 10) == Vector((0, 1, 1.0)))
    assert(ranking(ss, 0.0, 0) == Vector())
    assert(ranking(Vector[Medicion](), 0.0, 3) == Vector())
  }
 
  test("ranking propio 5: con 20 mediciones deja las 10 que pasan y las 6 primeras en orden") {
    // Similitudes 0.0, 0.7, 0.4, 0.1, 0.8, 0.5, 0.2, 0.9, 0.6, 0.3 repetidas
    val ss: Vector[Medicion] =
      Vector.tabulate(20)(i => (i, i + 1, ((i * 7) % 10) / 10.0))
    val todas = ranking(ss, 0.5, 100)
    assert(todas.length == 10)
    assert(todas.forall(_._3 >= 0.5))
    assert(todas.map(_._3) == todas.map(_._3).sorted.reverse)
    assert(ranking(ss, 0.5, 6) == Vector(
      (7, 8, 0.9), (17, 18, 0.9),
      (4, 5, 0.8), (14, 15, 0.8),
      (1, 2, 0.7), (11, 12, 0.7)))
  }

  //detectarDirecto

  test("detectarDirecto propio 1: las copias parciales quedan como las únicas parejas sospechosas") {
    // Los 3 primeros documentos tienen copia en las posiciones 8, 9 y 10.
    // Con v = 5000 y k = 3 los documentos al azar no comparten fragmentos.
    val c = corpusConCopias(8, 100, 5000, 3, 0.75)
    val r = detectarDirecto(c, 3, 0.2, 10)
    assert(r.length == 3)
    assert(r.map(m => (m._1, m._2)).toSet == Set((0, 8), (1, 9), (2, 10)))
    // Se copia el 75 % inicial: al menos 73 de 98 fragmentos son comunes
    assert(r.forall(_._3 >= 0.5))
  }
 
  test("detectarDirecto propio 2: umbral y tope sobre el ejemplo 2") {
    val alto = detectarDirecto(c2, 3, 0.6, 5)
    assert(alto.map(m => (m._1, m._2)) == Vector((0, 1)) && math.abs(alto.head._3 - 0.75) < eps)
    val tope = detectarDirecto(c2, 3, 0.3, 1)
    assert(tope.map(m => (m._1, m._2)) == Vector((0, 1)) && math.abs(tope.head._3 - 0.75) < eps)
    assert(detectarDirecto(c2, 3, 0.8, 5) == Vector())
  }
 
  test("detectarDirecto propio 3: documentos más cortos que k no entran, salvo con umbral 0.0") {
    val c: Corpus = Vector(("corto", Vector("a")), d0, d1, ("vacio", Vector()))
    val r = detectarDirecto(c, 3, 0.2, 5)
    assert(r.map(m => (m._1, m._2)) == Vector((1, 2)) && math.abs(r.head._3 - (1.0 / 3.0)) < eps)
    // Con umbral 0.0 se miden las 6 parejas, 5 de ellas con similitud cero
    val todas = detectarDirecto(c, 3, 0.0, 100)
    assert(todas.length == 6)
    assert(todas.map(m => (m._1, m._2)).head == ((1, 2)))
    assert(todas.tail.forall(_._3 == 0.0))
  }
 
  test("detectarDirecto propio 4: copias con fracciones 0.9, 0.6 y 0.3 salen en ese orden") {
    // Similitudes esperadas: cerca de 0.82, 0.42 y 0.17
    val base = corpusAlAzar(6, 100, 5000)
    val c: Corpus = base ++ Vector(
      copiaParcial(base(0), "C0", 0.9, 5000),
      copiaParcial(base(1), "C1", 0.6, 5000),
      copiaParcial(base(2), "C2", 0.3, 5000))
    val r = detectarDirecto(c, 3, 0.1, 10)
    assert(r.map(m => (m._1, m._2)) == Vector((0, 6), (1, 7), (2, 8)))
    assert(r(0)._3 > r(1)._3 && r(1)._3 > r(2)._3)
    assert(detectarDirecto(c, 3, 0.1, 2).map(m => (m._1, m._2)) == Vector((0, 6), (1, 7)))
  }
 
  test("detectarDirecto propio 5: el tamaño de fragmento k cambia el ranking del ejemplo 2") {
    // k = 1: palabras distintas, 8 / 10 y 6 / 8
    val k1 = detectarDirecto(c2, 1, 0.3, 5)
    assert(k1.map(m => (m._1, m._2)) == Vector((0, 1), (2, 3)))
    assert(math.abs(k1(0)._3 - 0.8) < eps && math.abs(k1(1)._3 - 0.75) < eps)
    // k = 5: 4 / 6 y 2 / 6
    val k5 = detectarDirecto(c2, 5, 0.3, 5)
    assert(k5.map(m => (m._1, m._2)) == Vector((0, 1), (2, 3)))
    assert(math.abs(k5(0)._3 - (4.0 / 6.0)) < eps && math.abs(k5(1)._3 - (2.0 / 6.0)) < eps)
    // k = 7: e2 y e3 ya no comparten ningún fragmento, 2 / 4 para el primero
    val k7 = detectarDirecto(c2, 7, 0.3, 5)
    assert(k7.map(m => (m._1, m._2)) == Vector((0, 1)) && math.abs(k7.head._3 - 0.5) < eps)
  }

  //detectarConIndice

  test("detectarConIndice propio 1: valores exactos en los ejemplos 1 y 2") {
    val r1 = detectarConIndice(c1, 3, 0.2, 5)
    assert(r1.map(m => (m._1, m._2)) == Vector((0, 1)) && math.abs(r1.head._3 - (1.0 / 3.0)) < eps)
    val r2 = detectarConIndice(c2, 3, 0.6, 5)
    assert(r2.map(m => (m._1, m._2)) == Vector((0, 1)) && math.abs(r2.head._3 - 0.75) < eps)
    val r3 = detectarConIndice(c2, 5, 0.3, 5)
    assert(r3.map(m => (m._1, m._2)) == Vector((0, 1), (2, 3)))
    assert(math.abs(r3(0)._3 - (4.0 / 6.0)) < eps && math.abs(r3(1)._3 - (2.0 / 6.0)) < eps)
  }
 
  test("detectarConIndice propio 2: encuentra las copias parciales y respeta el tope p") {
    // Las 4 primeras tienen copia en las posiciones 10, 11, 12 y 13
    val c = corpusConCopias(10, 100, 5000, 4, 0.75)
    val esperado = Set((0, 10), (1, 11), (2, 12), (3, 13))
    val r = detectarConIndice(c, 3, 0.2, 10)
    assert(r.length == 4)
    assert(r.map(m => (m._1, m._2)).toSet == esperado)
    assert(r.forall(_._3 >= 0.5))
    val top2 = detectarConIndice(c, 3, 0.2, 2)
    assert(top2.length == 2 && top2.map(m => (m._1, m._2)).forall(p => esperado.contains(p)))
  }
 
  test("detectarConIndice propio 3: coincide con detectarDirecto para varios umbrales positivos") {
    // v = 10 y k = 4: el índice descarta una parte de las parejas, no todas
    val c = corpusAlAzar(20, 80, 10)
    val hs = huellas(c, 4)
    val cands = parejasCandidatas(indiceInvertido(hs))
    assert(cands.nonEmpty && cands.length < parejas(20).length)
    assert(detectarConIndice(c, 4, 0.005, 1000).nonEmpty)
    assert(Vector(0.005, 0.01, 0.02, 0.1).forall(u =>
      detectarDirecto(c, 4, u, 1000) == detectarConIndice(c, 4, u, 1000)))
  }
 
  test("detectarConIndice propio 4: con umbral 0.0 deja de coincidir con el directo") {
    // La equivalencia vale para umbral > 0: el directo mide las parejas con
    // similitud cero y las reporta, el índice nunca las llega a ver.
    val directo = detectarDirecto(c1, 3, 0.0, 10)
    assert(directo.map(m => (m._1, m._2)) == Vector((0, 1), (0, 2), (1, 2)))
    val conIndice = detectarConIndice(c1, 3, 0.0, 10)
    assert(conIndice.map(m => (m._1, m._2)) == Vector((0, 1)))
    assert(math.abs(conIndice.head._3 - (1.0 / 3.0)) < eps)
  }
 
  test("detectarConIndice propio 5: sin fragmentos compartidos no hay nada y con uno solo sale esa pareja") {
    val disjuntos: Corpus = Vector(
      ("a", Vector("a1", "a2", "a3", "a4")),
      ("b", Vector("b1", "b2", "b3", "b4")),
      ("c", Vector("c1", "c2", "c3", "c4")))
    assert(detectarConIndice(disjuntos, 2, 0.01, 5) == Vector())
 
    // x y z comparten (a, b, c): 1 / (2 + 2 - 1). y no comparte nada y w es
    // más corto que k.
    val c: Corpus = Vector(
      ("x", Vector("a", "b", "c", "d")),
      ("y", Vector("e", "f", "g", "h")),
      ("z", Vector("a", "b", "c", "x")),
      ("w", Vector("q")))
    val r = detectarConIndice(c, 3, 0.1, 5)
    assert(r.map(m => (m._1, m._2)) == Vector((0, 2)) && math.abs(r.head._3 - (1.0 / 3.0)) < eps)
  }

}
