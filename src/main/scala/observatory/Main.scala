package observatory
import org.apache.log4j.{Level, Logger}
import java.io.File
import java.nio.file.{Paths, Files}
import observatory.Interaction2.temperaturesColors

object Main extends App {

  Logger.getLogger("org.apache.spark").setLevel(Level.WARN)
  val outDir = "target/temperatures"

  val temps = Extraction.locateTemperatures(2015, "/stations.csv", "/2015.csv")
  val tempsAvg = Extraction.locationYearlyAverageRecords(temps)

  def createSaveTile(year: Year, tile: Tile, data: Iterable[(Location, Temperature)]): Unit = {

    val zoom = tile.zoom
    val x = tile.x
    val y = tile.y

    val zoomDir = f"$outDir%s/$year%d/$zoom%d"
    val file = f"$zoomDir%s/$x%d-$y%d.png"

    Files.createDirectories(Paths.get(zoomDir))

    val img = Interaction.tile(data, temperaturesColors, tile)
    img.output(new File(file))
  }

  val data = Seq[(Year, Iterable[(Location, Temperature)])]((2015, tempsAvg))

  Interaction.generateTiles[Iterable[(Location, Temperature)]](data, createSaveTile)

}
