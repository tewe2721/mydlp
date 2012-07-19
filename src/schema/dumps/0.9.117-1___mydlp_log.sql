
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
DROP TABLE IF EXISTS `EndpointStatus`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `EndpointStatus` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `computerName` varchar(255) DEFAULT NULL,
  `firstAppeared` datetime NOT NULL,
  `ipAddress` varchar(255) NOT NULL,
  `isUpToDate` bit(1) DEFAULT NULL,
  `lastUpdate` datetime NOT NULL,
  `username` varchar(255) DEFAULT NULL,
  PRIMARY KEY (`id`),
  KEY `ipAddressIndex` (`ipAddress`(191)),
  KEY `firstAppearedIndex` (`firstAppeared`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `IncidentLog`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `IncidentLog` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `action` varchar(255) NOT NULL,
  `channel` varchar(255) NOT NULL,
  `date` datetime NOT NULL,
  `destination` varchar(255) DEFAULT NULL,
  `informationTypeId` bigint(20) DEFAULT NULL,
  `matcherMessage` varchar(255) DEFAULT NULL,
  `ruleId` bigint(20) NOT NULL,
  `sourceIp` bigint(20) DEFAULT NULL,
  `sourceUser` varchar(255) DEFAULT NULL,
  `visible` bit(1) NOT NULL,
  PRIMARY KEY (`id`),
  KEY `visibleIndex` (`visible`),
  KEY `dateIndex` (`date`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `IncidentLogFile`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `IncidentLogFile` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `filename` varchar(255) NOT NULL,
  `content_id` int(11) DEFAULT NULL,
  `incidentLog_id` int(11) NOT NULL,
  PRIMARY KEY (`id`),
  KEY `FKDAB6258E5A33480B` (`incidentLog_id`),
  KEY `FKDAB6258EFE16C13B` (`content_id`),
  CONSTRAINT `FKDAB6258EFE16C13B` FOREIGN KEY (`content_id`) REFERENCES `IncidentLogFileContent` (`id`),
  CONSTRAINT `FKDAB6258E5A33480B` FOREIGN KEY (`incidentLog_id`) REFERENCES `IncidentLog` (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `IncidentLogFileContent`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `IncidentLogFileContent` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `localPath` varchar(255) DEFAULT NULL,
  `mimeType` varchar(255) DEFAULT NULL,
  `size` bigint(20) DEFAULT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `IncidentLogRequeueStatus`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `IncidentLogRequeueStatus` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `date` datetime DEFAULT NULL,
  `isRequeued` bit(1) NOT NULL,
  `incidentLog_id` int(11) DEFAULT NULL,
  PRIMARY KEY (`id`),
  KEY `FK987DEC7E5A33480B` (`incidentLog_id`),
  CONSTRAINT `FK987DEC7E5A33480B` FOREIGN KEY (`incidentLog_id`) REFERENCES `IncidentLog` (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;
/*!40101 SET character_set_client = @saved_cs_client */;
/*!40103 SET TIME_ZONE=@OLD_TIME_ZONE */;

/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;
/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;
/*!40014 SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
/*!40111 SET SQL_NOTES=@OLD_SQL_NOTES */;

