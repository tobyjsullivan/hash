package net.tobysullivan.hash

object Salsa20Core {
  Int.MaxValue.toBinaryString

  def r(a: Int, b: Int): Int = {
    (((a) << (b)) | ((a) >> (32 - (b))))
  }

  def salsa20Core(in: Seq[Int]): Seq[Int] = {
    require(in.size == 16, "salsa20Core only works on seq of 16 ints")

    implicit class x(a: Seq[Int]) {
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

    (0 until 10).foldLeft(in)((a, _) => a.twist)
  }
}