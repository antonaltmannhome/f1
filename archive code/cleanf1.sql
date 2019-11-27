-- MySQL dump 10.13  Distrib 5.6.25, for Win64 (x86_64)
--
-- Host: localhost    Database: cleanf1
-- ------------------------------------------------------
-- Server version	5.6.25

/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8 */;
/*!40103 SET @OLD_TIME_ZONE=@@TIME_ZONE */;
/*!40103 SET TIME_ZONE='+00:00' */;
/*!40014 SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0 */;
/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;
/*!40101 SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='NO_AUTO_VALUE_ON_ZERO' */;
/*!40111 SET @OLD_SQL_NOTES=@@SQL_NOTES, SQL_NOTES=0 */;

--
-- Table structure for table `driver`
--

DROP TABLE IF EXISTS `driver`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `driver` (
  `forixid` int(11) DEFAULT NULL,
  `driver` varchar(50) NOT NULL,
  `adjustedsurname` varchar(50) DEFAULT NULL,
  `longname` varchar(50) DEFAULT NULL,
  PRIMARY KEY (`driver`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `driver`
--

LOCK TABLES `driver` WRITE;
/*!40000 ALTER TABLE `driver` DISABLE KEYS */;
/*!40000 ALTER TABLE `driver` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `qualifying`
--

DROP TABLE IF EXISTS `qualifying`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `qualifying` (
  `racename` varchar(50) NOT NULL,
  `driver` varchar(50) NOT NULL,
  `q1` decimal(10,3) NOT NULL,
  `q2` decimal(10,3) NOT NULL,
  `q3` decimal(10,3) NOT NULL,
  PRIMARY KEY (`racename`,`driver`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `qualifying`
--

LOCK TABLES `qualifying` WRITE;
/*!40000 ALTER TABLE `qualifying` DISABLE KEYS */;
/*!40000 ALTER TABLE `qualifying` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `race`
--

DROP TABLE IF EXISTS `race`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `race` (
  `racename` varchar(50) NOT NULL,
  `date` date DEFAULT NULL,
  `gotdata` tinyint(1) DEFAULT '0',
  `nlap` smallint(6) DEFAULT NULL,
  `perim` int(11) DEFAULT NULL,
  PRIMARY KEY (`racename`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `race`
--

LOCK TABLES `race` WRITE;
/*!40000 ALTER TABLE `race` DISABLE KEYS */;
/*!40000 ALTER TABLE `race` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `racedriver`
--

DROP TABLE IF EXISTS `racedriver`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `racedriver` (
  `racename` varchar(50) NOT NULL,
  `driver` varchar(50) NOT NULL,
  `startinggrid` smallint(6) DEFAULT NULL,
  PRIMARY KEY (`racename`,`driver`),
  KEY `driver_driver_fk` (`driver`),
  CONSTRAINT `driver_driver_fk` FOREIGN KEY (`driver`) REFERENCES `driver` (`driver`),
  CONSTRAINT `race_racename_fk` FOREIGN KEY (`racename`) REFERENCES `race` (`racename`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `racedriver`
--

LOCK TABLES `racedriver` WRITE;
/*!40000 ALTER TABLE `racedriver` DISABLE KEYS */;
/*!40000 ALTER TABLE `racedriver` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `racedriverlap`
--

DROP TABLE IF EXISTS `racedriverlap`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `racedriverlap` (
  `racename` varchar(50) NOT NULL,
  `driver` varchar(50) NOT NULL,
  `lap` smallint(6) NOT NULL,
  `sec` decimal(10,3) NOT NULL,
  PRIMARY KEY (`racename`,`driver`,`lap`),
  CONSTRAINT `FK` FOREIGN KEY (`racename`, `driver`) REFERENCES `racedriver` (`racename`, `driver`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `racedriverlap`
--

LOCK TABLES `racedriverlap` WRITE;
/*!40000 ALTER TABLE `racedriverlap` DISABLE KEYS */;
/*!40000 ALTER TABLE `racedriverlap` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `racetyre`
--

DROP TABLE IF EXISTS `racetyre`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `racetyre` (
  `racename` varchar(50) NOT NULL,
  `tyre` varchar(50) NOT NULL,
  `mod4int` decimal(10,3) NOT NULL,
  `mod4slo` decimal(10,3) NOT NULL,
  `mod30int` decimal(10,3) NOT NULL,
  `mod30slo` decimal(10,3) NOT NULL,
  PRIMARY KEY (`racename`,`tyre`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `racetyre`
--

LOCK TABLES `racetyre` WRITE;
/*!40000 ALTER TABLE `racetyre` DISABLE KEYS */;
/*!40000 ALTER TABLE `racetyre` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `stint`
--

DROP TABLE IF EXISTS `stint`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `stint` (
  `racename` varchar(50) NOT NULL,
  `driver` varchar(50) NOT NULL,
  `startlap` smallint(6) NOT NULL,
  `endlap` smallint(6) DEFAULT NULL,
  `tyre` varchar(50) NOT NULL,
  PRIMARY KEY (`racename`,`driver`,`startlap`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `stint`
--

LOCK TABLES `stint` WRITE;
/*!40000 ALTER TABLE `stint` DISABLE KEYS */;
/*!40000 ALTER TABLE `stint` ENABLE KEYS */;
UNLOCK TABLES;
/*!40103 SET TIME_ZONE=@OLD_TIME_ZONE */;

/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;
/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;
/*!40014 SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
/*!40111 SET SQL_NOTES=@OLD_SQL_NOTES */;

-- Dump completed on 2018-05-26 15:41:57
