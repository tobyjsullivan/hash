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
  }
  "Calling salsa20Core" should {
    "Produce the right output according to the Internet Draft" in {
      /* 
       * These inputand output values are defined at http://tools.ietf.org/html/draft-josefsson-scrypt-kdf-01#page-10
       */
      
      implicit def hex2int(hex: String): Int = java.lang.Long.parseLong(hex, 16).toInt
      
      val input: Seq[Int] = Seq(
          "7e879a21", 
          "4f3ec986",
          "7ca940e6",
          "41718f26",
          "baee555b",
          "8c61c1b5",
          "0df84611",
          "6dcd3b1d",

          "ee24f319",
          "df9b3d85",
          "14121e4b",
          "5ac5aa32",

          "76021d29",
          "09c74829",
          "edebc68d",
          "b8b8c25e"
        )
        
      
      val expected: Seq[Int] = Seq(
        "a41f859c", 
        "6608cc99", 
        "3b81cacb", 
        "020cef05",
        
        "044b2181", 
        "a2fd337d", 
        "fd7b1c63", 
        "96682f29",

        "b4393168", 
        "e3c9e6bc", 
        "fe6bc5b7", 
        "a06d96ba",

        "e424cc10", 
        "2c91745c", 
        "24ad673d", 
        "c7618f81"    
      )
      
      val result = Salsa20Core.salsa20Core(input)
      
      result shouldEqual expected
    }
  }
}