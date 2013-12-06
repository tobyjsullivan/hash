package net.tobysullivan.hash

object Salsa20Core {
  Int.MaxValue.toBinaryString

  def r(a: Int, b: Int): Int = {
    (((a) << (b)) | ((a) >> (32 - (b))))
  }

  /*
   * This class is only public for testability. Otherwise it would be buried in the salsa20Core method
   */
  implicit class twistableSeq(a: Seq[Int]) {
    def grate(i0: Int, i1: Int, i2: Int, i3: Int): Seq[Int] =
      a.updated(i0, a(i0) ^ r(a(i1) + a(i2), i3))

    def twist: Seq[Int] = {
      val sequence = Seq(
        (4, 0, 12, 7), (8, 4, 0, 9),
        (12, 8, 4, 13), (0, 12, 8, 18),
        (9, 5, 1, 7), (13, 9, 5, 9),
        (1, 13, 9, 13), (5, 1, 13, 18),
        (14, 10, 6, 7), (2, 14, 10, 9),
        (6, 2, 14, 13), (10, 6, 2, 18),
        (3, 15, 11, 7), (7, 3, 15, 9),
        (11, 7, 3, 13), (15, 11, 7, 18),
        (1, 0, 3, 7), (2, 1, 0, 9),
        (3, 2, 1, 13), (0, 3, 2, 18),
        (6, 5, 4, 7), (7, 6, 5, 9),
        (4, 7, 6, 13), (5, 4, 7, 18),
        (11, 10, 9, 7), (8, 11, 10, 9),
        (9, 8, 11, 13), (10, 9, 8, 18),
        (12, 15, 14, 7), (13, 12, 15, 9),
        (14, 13, 12, 13), (15, 14, 13, 18))

      sequence.foldRight(a)((t, a) => a.grate(t._1, t._2, t._3, t._4))

    }
  }
  
  def salsa20Core(in: Seq[Byte], n: Int): Seq[Byte] = {
    require(in.size == 64, "salsa20Core only works on seq of 64 bytes")
    
    val ints: Seq[Int] = for {
      i <- (0 until 16).toSeq
      a = in(i * 4).toInt
      b = in(i * 4 + 1).toInt
      c = in(i * 4 + 2).toInt
      d = in(i * 4 + 3).toInt
    } yield (a | (b << 8) | (c << 16) | (d << 24))
    

    val twisted: Seq[Int] = (n until 0 by -2).foldLeft(ints)((a, _) => a.twist)

    val outInts = (twisted zip in) map (pair => pair._1 + pair._2)
    
    val out: Seq[Byte] = for {
      i <- outInts
      j <- 0 until 4
    } yield (i >> (j* 8) & 0xff).toByte
    
    out
  }
}
