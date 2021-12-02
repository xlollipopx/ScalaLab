package services

import conf.dbConf._
import org.apache.spark.sql.{DataFrame, Dataset, Encoder, SparkSession}

object SparkService   {

  val spark = SparkSession.builder()
    .appName("Managing Nulls")
    .config("spark.master", "local")
    .getOrCreate()

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
    df.write
      .format("jdbc")
      .option("driver", driver)
      .option("url", url)
      .option("user", user)
      .option("password", password)
      .option("dbtable", s"public.$name")
      .save()
  }


  def readFromDb[A: Encoder](name: String): List[A] = {
    val df = spark.read
      .format("jdbc")
      .option("driver", driver)
      .option("url", url)
      .option("user", user)
      .option("password", password)
      .option("dbtable", s"public.$name")
      .load()

    val ds = df.as[A]
    ds.rdd.collect().toList
  }





}
