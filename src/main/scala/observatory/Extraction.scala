package observatory

import java.time.LocalDate


import scala.io.Source

/**
  * 1st milestone: data extraction
  */
object Extraction extends ExtractionInterface {

  private val resourceLookup = (fileName: String) => getClass.getResourceAsStream(fileName)
  case class Station(stn: String, wban: String)

  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperatures(year: Year, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Temperature)] = {
    val stations = getStations(stationsFile) // map
    val temperatures = getTemperatures(year, temperaturesFile)

    temperatures
      .filter(line => stations.contains(line._1))
      .map(line => {
        val (station, (date, temperature)) = line
        val location = stations(station)
        (date, location, temperature)
      })
  }

  def getStations(stationsFile: String) : Map[Station, Location] = {
    Source.fromInputStream(resourceLookup(stationsFile))
      .getLines()
      .map(_.split(","))
      .filter(_.length == 4)
      .map(cols => {
        val stn = cols(0)
        val wban = cols(1)
        val lat = cols(2).toDouble
        val lon = cols(3).toDouble
        Station(stn, wban) -> Location(lat, lon)
      })
      .toMap
  }

  def getTemperatures(year: Year, temperaturesFile: String): Iterable[(Station, (LocalDate, Temperature))] = {
    Source.fromInputStream(resourceLookup(temperaturesFile))
      .getLines
      .map(_.split(","))
      .filter(_.length == 5)
        .map(
          cols => {
            val stn = cols(0)
            val wban = cols(1)
            val month = cols(2).toInt
            val day = cols(3).toInt
            val tempInC: Temperature = (cols(4).toDouble - 32) / 1.8 // cols(4) is temperature in degrees Fahrenheit
            (Station(stn, wban), (LocalDate.of(year, month, day), tempInC))
          })
      .toList.view
  }

  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Temperature)]): Iterable[(Location, Temperature)] = {

    def avg(temperatures: Iterable[Temperature]): Temperature = temperatures.sum / temperatures.size

    records
      .map(elem => (elem._2, elem._3)) // (Location, Iterable[(Location, Temperature)])
      .groupBy(_._1)
      .map(tuple =>
      {
        val avgTemp = avg(tuple._2.map(_._2))
        (tuple._1, avgTemp)
      })
  }
}
