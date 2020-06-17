package observatory

import com.sksamuel.scrimage.{Image, Pixel}
import Interaction.tileLocation
import Visualization.interpolateColor

/**
  * 5th milestone: value-added information visualization
  */
object Visualization2 extends Visualization2Interface {

  /**
    * @param point (x, y) coordinates of a point in the grid cell
    * @param d00 Top-left value
    * @param d01 Bottom-left value
    * @param d10 Top-right value
    * @param d11 Bottom-right value
    * @return A guess of the value at (x, y) based on the four known values, using bilinear interpolation
    *         See https://en.wikipedia.org/wiki/Bilinear_interpolation#Unit_Square
    */
  def bilinearInterpolation(
    point: CellPoint,
    d00: Temperature,
    d01: Temperature,
    d10: Temperature,
    d11: Temperature
  ): Temperature = {

    val x = point.x
    val y = point.y

    d00 * (1 - x) * (1 - y) + d10 * x * (1 - y) + d01 * (1 - x) * y + d11 * x * y
  }

  def interpolateTemps(grid: GridLocation => Temperature,
                       location: Location): Temperature = {

    val lat = location.lat.toInt
    val lon = location.lon.toInt

    val d00 = grid(GridLocation(lat, lon))
    val d01 = grid(GridLocation(lat + 1, lon))
    val d10 = grid(GridLocation(lat, lon + 1))
    val d11 = grid(GridLocation(lat + 1, lon + 1))

    val point = CellPoint(location.lon - lon, location.lat - lat)

    bilinearInterpolation(point, d00, d01, d10, d11)
  }

  /**
    * @param grid Grid to visualize
    * @param colors Color scale to use
    * @param tile Tile coordinates to visualize
    * @return The image of the tile at (x, y, zoom) showing the grid using the given color scale
    */
  def visualizeGrid(
    grid: GridLocation => Temperature,
    colors: Iterable[(Temperature, Color)],
    tile: Tile
  ): Image = {

    val (width, height, power) = (128, 128, 8)

    val (xOffset, yOffset) = (
      tile.x * math.pow(2, power).toInt,
      tile.y * math.pow(2, power).toInt)

    val subTiles = for (y <- 0 until height;
                        x <- 0 until width)
                          yield Tile(
                            x + xOffset,
                            y + yOffset,
                            power + tile.zoom)

    val pixels: Array[Pixel] =
      subTiles.par
        .map(tile => tileLocation(tile))
        .map(loc => interpolateTemps(grid, loc))
        .map(temp => interpolateColor(colors, temp))
        .map(color => Pixel(color.red, color.green, color.blue, 127))
        .toArray

    Image(width, height, pixels)

  }
}
