package net.tobysullivan.hash

import org.scalatest._

class Salsa20CoreSpec extends WordSpec with Matchers {
  "Calling r" should {
    "Produce the correct value for two positive inputs" in {
      val a: Int = 395
      val b: Int = 12346
      
      val res = Salsa20Core.r(a, b)
      
      res shouldEqual 738197510
    }
    "Produce the correct value for two small positive inputs" in {
      val res = Salsa20Core.r(76, 8)
      val expected = 19456
      
      res shouldEqual expected
    }
  }
  "Calling salsa20Core" should {
    "Produce the correct resulting bytes according to the internet draft" in {
      /* 
-      * These input and output values are defined at http://tools.ietf.org/html/draft-josefsson-scrypt-kdf-01#page-10
-      */
      val input = "7e 87 9a 21 4f 3e c9 86 7c a9 40 e6 41 71 8f 26 ba ee 55 5b 8c 61 c1 b5 0d f8 46 11 6d cd 3b 1d ee 24 f3 19 df 9b 3d 85 14 12 1e 4b 5a c5 aa 32 76 02 1d 29 09 c7 48 29 ed eb c6 8d b8 b8 c2 5e"
      
      val output= "a4 1f 85 9c 66 08 cc 99 3b 81 ca cb 02 0c ef 05 04 4b 21 81 a2 fd 33 7d fd 7b 1c 63 96 68 2f 29 b4 39 31 68 e3 c9 e6 bc fe 6b c5 b7 a0 6d 96 ba e4 24 cc 10 2c 91 74 5c 24 ad 67 3d c7 61 8f 81"
      
      implicit def string2bytes(in: String): Seq[Byte] = in.split(" ").map(s => java.lang.Integer.parseInt(s, 16).toByte)
        
      val inBytes: Seq[Byte] = input
      
      val outBytes: Seq[Byte] = output
      
      val res = Salsa20Core.salsa20Core(inBytes, 8)
      
      res shouldEqual outBytes
    }
  }
  "Calling grate on a twistableSeq" should {
    "properly transform a sequence of one int" in {
      val inSeq = Seq(12, 44, 32, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
      
      val res = Salsa20Core.grate(inSeq, 0, 1, 2, 8)

      res shouldEqual Seq(19468, 44, 32, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    }
  }
}
