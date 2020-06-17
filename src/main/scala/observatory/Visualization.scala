package observatory

import scala.math._

import com.sksamuel.scrimage.{Image, Pixel}

/**
  * 2nd milestone: basic visualization
  */
object Visualization extends VisualizationInterface {

  def calculateDistanceBetweenLocations(loc1: Location, loc2: Location): Double = {
    if (loc1 == loc2) 0
    else {
      val loc1Latitude = toRadians(loc1.lat)
      val loc2Latitude = toRadians(loc2.lat)
      val loc1Longitude = toRadians(loc1.lon)
      val loc2Longitude = toRadians(loc2.lon)
      val deltaLongitudes = abs(loc1Longitude - loc2Longitude)
      val earthRadiusInKm = 6371
      earthRadiusInKm * acos(sin(loc1Latitude) * sin(loc2Latitude) + cos(loc1Latitude) * cos(loc2Latitude) * cos(deltaLongitudes))
    }
  }

  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Temperature)], location: Location): Temperature = {

    def calculateIDW(distance: Double): Double = { // Inverse distance weighting
      1 / pow (distance, 6)
    }
    val distanceTemperatures = temperatures.map(tuple =>
      (calculateDistanceBetweenLocations(tuple._1, location), tuple._2))

    val minDistance = distanceTemperatures.minBy(_._1)

    if (minDistance._1 <= 1) {
      minDistance._2
    } else {
      val (dist, temp) = distanceTemperatures
        .map {
          case (distance, temperature) =>
            val weight = calculateIDW(distance)
            (weight * temperature, weight)
        }
        .reduce((elem1, elem2) => (elem1._1 + elem2._1, elem1._2 + elem2._2))
      dist / temp
      }
  }

  /**
    * @param points Pairs containing a value and its associated color
    * @param value The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Temperature, Color)], value: Temperature): Color = {

    val equalTemperature = points.find(_._1 == value)

    equalTemperature match {

      case Some((_, color)) => color
      case _ =>

        val (colder, warmer) = points.partition(_._1 < value)

        if (colder.isEmpty) {
          warmer.minBy(_._1)._2
        } else if (warmer.isEmpty) {
          colder.maxBy(_._1)._2
        } else {
          val nearestColder = colder.maxBy(_._1)
          val nearestWarmer = warmer.minBy(_._1)

          val warmerDifference = nearestWarmer._1 - value
          val colderDifference = value - nearestColder._1
          val span = nearestWarmer._1 - nearestColder._1

          def interpolate(left: Int, right: Int): Int = ((left * colderDifference + right * warmerDifference) / span).round.toInt

          Color(
            interpolate(nearestWarmer._2.red, nearestColder._2.red),
            interpolate(nearestWarmer._2.green, nearestColder._2.green),
            interpolate(nearestWarmer._2.blue, nearestColder._2.blue)
          )
        }
    }
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)]): Image = {

    def coordinatesToLocation(x: Int, y: Int): Location = Location(90 - y, x - 180)
    val width = 120
    val height = 60

    val pixels = new Array[Pixel](width * height)

    val coordinates = for {
      x <- 0 until width
      y <- 0 until height
    } yield (x, y)

    coordinates.par.foreach(pair => {
      val (x, y) = pair
      val location = coordinatesToLocation(x, y)
      val temperature = predictTemperature(temperatures, location)
      val color = interpolateColor(colors, temperature)
      pixels(x + y * width) = Pixel(color.red, color.green, color.blue, 255)
    })

    Image(width, height, pixels)

  }
}

