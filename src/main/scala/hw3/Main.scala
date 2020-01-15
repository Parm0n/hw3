package hw3

import scala.collection.mutable
//package scala.math.sqrt

object Main {
  def standardDeviation(vector: List[Double]): Double = {
    var sum : Double = 0
    for(it <- vector){
      sum += it
    }
    val avg: Double = sum / vector.size
    var avgL: List[Double] = List()

    for(it <- vector){
      avgL = avgL :+ (it - avg) * (it - avg)
    }

    var sumA : Double = 0
    for(it <- avgL){
      sumA += it
    }

    val avgA: Double = sumA / vector.size
    val stddev: Double = scala.math.sqrt(avgA)
    stddev
  }

  def findMax(inpt: List[(Char, Int)]): (Char, List[(Char, Int)]) = {
    var max: Int = 0
    var res: (Char, Int) = (' ', 0)
    var l: List[(Char, Int)] = inpt
    for(it <- inpt){
      if(it._2 > max)
        max = it._2
    }
    for(it <- l){
      if(it._2 == max){
        res = it
        max = -1
      }
    }
    l = l.filter(_ != (res))

    (res._1, l)
  }

  def letterFrequencyRanking(corpus: String): String = {
    var lets: List[Char] = List()

    for(it <- corpus){
      if(it.isLetter)
      lets = lets :+ it.toLower
    }

    lets = lets.sorted


    var letters: List[(Char, Int)] = List()

    for(it <- lets){
      var flag: Boolean = false
      for(i <- 0 to letters.size - 1){
        if(it == letters(i)._1){
          flag = true
          letters = letters.updated(i, (letters(i)._1, letters(i)._2 + 1))
        }
      }
      if(!flag){
        letters = letters :+ (it, 1)
      }

    }

    var res: String = ""

    while(letters.size > 0){
      var localRes: (Char, List[(Char, Int)])  = findMax(letters)

      res += localRes._1
      letters = localRes._2
    }

    res
  }

  def romanji(katakana: String): String = ???


  def gray(bits: Int): List[String] = {
    var grays : Map[Int,List[String]] = Map()
    grays.get(bits) match {
         case Some(res) => res
           case _ =>
             val res = bits match {
             case 0 => Nil
                 case 1 => List("0","1")
                 case _ =>
                   val others = gray(bits-1)
                   val reflected = others.reverse
                   others.map("0" + _) ++ reflected.map("1" + _)
           }
             grays = grays + (bits -> res)
             res
           }
  }
}

object Katakana {
  val symbols = Map(
    'ア' -> List('a'), 'イ' -> List('i'),  'ウ' -> List('u'), 'エ' -> List('e'), 'オ' -> List('o'),
    'ン' -> List('n'),
    'カ' -> List('k','a'), 'キ' -> List('k','i'), 'ク' -> List('k','u'), 'ケ' -> List('k','e'), 'コ' -> List('k','o'),
    'ガ' -> List('g','a'), 'ギ' -> List('g','i'), 'グ' -> List('g','u'), 'ゲ' -> List('g','e'), 'ゴ' -> List('g','o'),
    'サ' -> List('s','a'), 'シ' -> List('s','i'), 'ス' -> List('s','u'), 'セ' -> List('s','e'), 'ソ' -> List('s','o'),
    'ザ' -> List('z','a'), 'ジ' -> List('z','i'), 'ズ' -> List('z','u'), 'ゼ' -> List('z','e'), 'ゾ' -> List('z','o'),
    'タ' -> List('t','a'), 'チ' -> List('t','i'), 'ツ' -> List('t','u'), 'テ' -> List('t','e'), 'ト' -> List('t','o'),
    'ダ' -> List('d','a'), 'ヂ' -> List('d','i'), 'ヅ' -> List('d','u'), 'デ' -> List('d','e'), 'ド' -> List('d','o'),
    'ナ' -> List('n','a'), 'ニ' -> List('n','i'), 'ヌ' -> List('n','u'), 'ネ' -> List('n','e'), 'ノ' -> List('n','o'),
    'ハ' -> List('h','a'), 'ヒ' -> List('h','i'), 'フ' -> List('h','u'), 'ヘ' -> List('h','e'), 'ホ' -> List('h','o'),
    'バ' -> List('b','a'), 'ビ' -> List('b','i'), 'ブ' -> List('b','u'), 'ベ' -> List('b','e'), 'ボ' -> List('b','o'),
    'パ' -> List('p','a'), 'ピ' -> List('p','i'), 'プ' -> List('p','u'), 'ペ' -> List('p','e'), 'ポ' -> List('p','o'),
    'マ' -> List('m','a'), 'ミ' -> List('m','i'), 'ム' -> List('m','u'), 'メ' -> List('m','e'), 'モ' -> List('m','o'),
    'ヤ' -> List('y','a'),                        'ユ' -> List('y','u'),                        'ヨ' -> List('y','o'),
    'ラ' -> List('r','a'), 'リ' -> List('r','i'), 'ル' -> List('r','u'), 'レ' -> List('r','e'), 'ロ' -> List('r','o'),
    'ワ' -> List('w','a'), 'ヰ' -> List('w','i'),                        'ヱ' -> List('w','e'), 'ヲ' -> List('w','o'),
  )
  val longVowels = Map(
    'a' -> 'ā',
    'i' -> 'ī',
    'e' -> 'ē',
    'u' -> 'ū',
    'o' -> 'ō'
  )
}