package inclinometer

/**
 * Created by two on 2016-03-01.
 */
import scala.math._
object Inclinometer {

  def mmToMeters(scale: Int)(mm:Double): Double = scale * mm / 1000
  
  def metersToMm(scale: Int)(metrs:Double): Double = metrs / scale * 1000

  def mmToInclination(scale: Int)(high: Double)(dist:Double) =
    atan(high / mmToMeters(scale)(dist)).toDegrees

  def degreeToMm(scale: Int)(h: Double)(alpha: Double) =
    metersToMm(scale)(h / tan(alpha.toRadians))

  def s25000h100Inclination = mmToInclination(25000)(100)_
  def s25000h50Inclination = mmToInclination(25000)(50)_
  def s25000h100Distance = degreeToMm(25000)(100)_
  def s25000h50Distance = degreeToMm(25000)(50)_

  def compute(f: Double => Double)(l: List[Double]): List[(Double, Double)] =
    l.map(x => (x, f(x)))

  
}

import Inclinometer._
import List._

object InclinometerMain{

  val mm =  range(1, 21, 1).map(_.toDouble)
  val degrees =  range(1, 46, 1).map(_.toDouble)

  def print(unit1: String, unit2:String)(a: (Double, Double)):String = a match {
    case (x, y) => f"$x%.0f$unit1%s -> $y%.2f$unit2%s"
  }

  def printInclinations = print("mm", "\u00b0")_
  def printDegrees = print("\u00b0", "mm")_

  def main(args: Array[String]) {
    println("h = 50m")
    println(compute(s25000h50Inclination)(mm).map(printInclinations))
    println("h = 100m")
    println(compute(s25000h100Inclination)(mm).map(printInclinations))
    println("h = 50m")
    println(compute(s25000h50Distance)(degrees).map(printDegrees))
    println("h = 100m")
    println(compute(s25000h100Distance)(degrees).map(printDegrees))
  }
}

