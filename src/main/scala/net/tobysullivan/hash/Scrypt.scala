package net.tobysullivan.hash

object Scrypt {
  case class Uint(value: Int)
  
  Int.MaxValue.toBinaryString
  
  def R(a: Int, b: Int): Int = {
    (((a) << (b)) | ((a) >> (32 - (b))))
  }
}