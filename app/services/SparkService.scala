package services

import org.apache.spark.sql.{DataFrame, Encoder, SaveMode, SparkSession}

import java.util.Properties

object SparkService   {

  lazy val spark = SparkSession.builder()
    .appName("Managing Nulls")
    .config("spark.master", "local")
    .getOrCreate()

 val props = new PropertiesResolver
 val properties = props.getProperties()

  def readDfFromJson(filename: String) = spark.read
    .option("inferSchema", "true")
    .json(s"conf/data/$filename")

  def getDataFromJson[A : Encoder](path: String) : List[A] = {
    val df = readDfFromJson(path)
    df.select("*").na.drop()
    val ds = df.as[A]
    ds.rdd.collect().toList
  }

  def writeToDb(df: DataFrame, name: String)  = {
    df.write.mode(SaveMode.Overwrite).jdbc(properties.getProperty("url"), name, properties)
  }

  def readEntityFromDb[A: Encoder](name: String): List[A] = {
    val df = spark.read.jdbc(properties.getProperty("url"), s"public.$name", properties)
    val ds = df.as[A]
    ds.rdd.collect().toList
  }
  def readDFFromDb(name: String): DataFrame = {
    spark.read.jdbc(properties.getProperty("url"), s"public.$name", properties)
  }

}


class PropertiesResolver {
  def getProperties() = {
    val properties         = new Properties
    val currentThread      = Thread.currentThread
    val contextClassLoader = currentThread.getContextClassLoader
    val propertiesStream   = contextClassLoader.getResourceAsStream("db.properties")
      properties.load(propertiesStream)
      propertiesStream.close()
    properties
  }
}
