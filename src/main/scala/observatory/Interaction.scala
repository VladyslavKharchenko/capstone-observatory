package observatory

import scala.math._
import com.sksamuel.scrimage.{Image, Pixel}
import observatory.Visualization.{predictTemperature, interpolateColor}


/**
  * 3rd milestone: interactive visualization
  */
object Interaction extends InteractionInterface {

  /**
    * @param tile Tile coordinates
    * @return The latitude and longitude of the top-left corner of the tile, as per http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
    */
  def tileLocation(tile: Tile): Location = {
    val num = pow(2, tile.zoom)
    val latitude = atan(sinh(Pi - (tile.y * 2 * Pi / num))) * 180 / Pi
    val longitude = (tile.x * 360.0 / num)  - 180
    Location(latitude, longitude)
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @param tile Tile coordinates
    * @return A 256×256 image showing the contents of the given tile
    */
  def tile(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)], tile: Tile): Image = {

    val (width, height, power) = (128, 128, 8)

    val (x0, y0) =
      (tile.x * pow(2, power).toInt,
      tile.y * pow(2, power).toInt)

    val subTiles = for (y <- 0 until height; x <- 0 until width) yield Tile(x + x0, y + y0, power + tile.zoom)

    val pixels: Array[Pixel] =
      subTiles
        .map(tile => tileLocation(tile))
        .map(location => predictTemperature(temperatures, location))
        .map(temperature => interpolateColor(colors, temperature))
        .map(color => Pixel(color.red, color.green, color.blue, 127))
        .toArray
    Image(width, height, pixels)
  }

  /**
    * Generates all the tiles for zoom levels 0 to 3 (included), for all the given years.
    * @param yearlyData Sequence of (year, data), where `data` is some data associated with
    *                   `year`. The type of `data` can be anything.
    * @param generateImage Function that generates an image given a year, a zoom level, the x and
    *                      y coordinates of the tile and the data to build the image from
    */
  def generateTiles[Data](
    yearlyData: Iterable[(Year, Data)],
    generateImage: (Year, Tile, Data) => Unit
  ): Unit = {

    for {
      (year, data) <- yearlyData
      zoom <- 0 until 4
      x <- 0 until pow(2, zoom).toInt
      y <- 0 until pow(2, zoom).toInt
    } generateImage(year, Tile(x, y, zoom), data)

  }

}
