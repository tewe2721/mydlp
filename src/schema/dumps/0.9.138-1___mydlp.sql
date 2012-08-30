
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
DROP TABLE IF EXISTS `ADDomain`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `ADDomain` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `currentlyEnumerating` bit(1) DEFAULT NULL,
  `domainName` varchar(190) NOT NULL,
  `loginPassword` varchar(255) NOT NULL,
  `loginUsername` varchar(255) NOT NULL,
  `netbiosName` varchar(255) DEFAULT NULL,
  `serverIp` varchar(255) NOT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `domainName` (`domainName`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `ADDomainAlias`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `ADDomainAlias` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `domainAlias` varchar(255) NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `ADDomainGroup`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `ADDomainGroup` (
  `name` varchar(255) NOT NULL,
  `id` int(11) NOT NULL,
  PRIMARY KEY (`id`),
  KEY `FK2C2C7F83D5691E4` (`id`),
  CONSTRAINT `FK2C2C7F83D5691E4` FOREIGN KEY (`id`) REFERENCES `ADDomainItem` (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `ADDomainItem`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `ADDomainItem` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `distinguishedName` varchar(1024) NOT NULL,
  `distinguishedNameHash` int(11) NOT NULL,
  `parent_id` int(11) DEFAULT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `distinguishedNameHash` (`distinguishedNameHash`),
  KEY `FKAD83167ABE9EC806` (`parent_id`),
  CONSTRAINT `FKAD83167ABE9EC806` FOREIGN KEY (`parent_id`) REFERENCES `ADDomainItemGroup` (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `ADDomainItemGroup`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `ADDomainItemGroup` (
  `id` int(11) NOT NULL,
  PRIMARY KEY (`id`),
  KEY `FKAB47EC253D5691E4` (`id`),
  CONSTRAINT `FKAB47EC253D5691E4` FOREIGN KEY (`id`) REFERENCES `ADDomainItem` (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `ADDomainOU`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `ADDomainOU` (
  `name` varchar(255) NOT NULL,
  `id` int(11) NOT NULL,
  PRIMARY KEY (`id`),
  KEY `FKAFFF56ED43382451` (`id`),
  CONSTRAINT `FKAFFF56ED43382451` FOREIGN KEY (`id`) REFERENCES `ADDomainItemGroup` (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `ADDomainRoot`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `ADDomainRoot` (
  `id` int(11) NOT NULL,
  `domain_id` int(11) DEFAULT NULL,
  PRIMARY KEY (`id`),
  KEY `FKAD871C4943382451` (`id`),
  KEY `FKAD871C49BD40070C` (`domain_id`),
  CONSTRAINT `FKAD871C49BD40070C` FOREIGN KEY (`domain_id`) REFERENCES `ADDomain` (`id`),
  CONSTRAINT `FKAD871C4943382451` FOREIGN KEY (`id`) REFERENCES `ADDomainItemGroup` (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `ADDomainUser`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `ADDomainUser` (
  `displayName` varchar(255) NOT NULL,
  `sAMAccountName` varchar(255) NOT NULL,
  `id` int(11) NOT NULL,
  PRIMARY KEY (`id`),
  KEY `FKAD8887323D5691E4` (`id`),
  CONSTRAINT `FKAD8887323D5691E4` FOREIGN KEY (`id`) REFERENCES `ADDomainItem` (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `ADDomainUserAlias`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `ADDomainUserAlias` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `userAlias` varchar(255) NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `ADDomainUser_ADDomainGroup`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `ADDomainUser_ADDomainGroup` (
  `users_id` int(11) NOT NULL,
  `groups_id` int(11) NOT NULL,
  PRIMARY KEY (`users_id`,`groups_id`),
  KEY `FK757BA1AB15067AEF` (`groups_id`),
  KEY `FK757BA1AB2D87BC33` (`users_id`),
  CONSTRAINT `FK757BA1AB2D87BC33` FOREIGN KEY (`users_id`) REFERENCES `ADDomainUser` (`id`),
  CONSTRAINT `FK757BA1AB15067AEF` FOREIGN KEY (`groups_id`) REFERENCES `ADDomainGroup` (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `ADDomainUser_ADDomainUserAlias`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `ADDomainUser_ADDomainUserAlias` (
  `ADDomainUser_id` int(11) NOT NULL,
  `aliases_id` int(11) NOT NULL,
  UNIQUE KEY `aliases_id` (`aliases_id`),
  KEY `FK914B96716498EF09` (`ADDomainUser_id`),
  KEY `FK914B96716409D34B` (`aliases_id`),
  CONSTRAINT `FK914B96716409D34B` FOREIGN KEY (`aliases_id`) REFERENCES `ADDomainUserAlias` (`id`),
  CONSTRAINT `FK914B96716498EF09` FOREIGN KEY (`ADDomainUser_id`) REFERENCES `ADDomainUser` (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `ADDomain_ADDomainAlias`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `ADDomain_ADDomainAlias` (
  `ADDomain_id` int(11) NOT NULL,
  `aliases_id` int(11) NOT NULL,
  UNIQUE KEY `aliases_id` (`aliases_id`),
  KEY `FK6BC234519497F249` (`ADDomain_id`),
  KEY `FK6BC23451FA529D56` (`aliases_id`),
  CONSTRAINT `FK6BC23451FA529D56` FOREIGN KEY (`aliases_id`) REFERENCES `ADDomainAlias` (`id`),
  CONSTRAINT `FK6BC234519497F249` FOREIGN KEY (`ADDomain_id`) REFERENCES `ADDomain` (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `Argument`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `Argument` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `name` varchar(255) DEFAULT NULL,
  `nameKey` varchar(255) DEFAULT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB AUTO_INCREMENT=2 DEFAULT CHARSET=utf8mb4;
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `AuthSecurityRole`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `AuthSecurityRole` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `roleName` varchar(255) DEFAULT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB AUTO_INCREMENT=4 DEFAULT CHARSET=utf8mb4;
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `AuthUser`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `AuthUser` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `email` varchar(255) DEFAULT NULL,
  `hasAuthorityScope` bit(1) NOT NULL,
  `isActive` bit(1) DEFAULT NULL,
  `password` varchar(255) DEFAULT NULL,
  `username` varchar(127) DEFAULT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `username` (`username`)
) ENGINE=InnoDB AUTO_INCREMENT=3 DEFAULT CHARSET=utf8mb4;
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `AuthUser_ADDomainItem`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `AuthUser_ADDomainItem` (
  `AuthUser_id` int(11) NOT NULL,
  `authorityScopeADItems_id` int(11) NOT NULL,
  KEY `FK71AA0D868305BF97` (`authorityScopeADItems_id`),
  KEY `FK71AA0D8671D65EE9` (`AuthUser_id`),
  CONSTRAINT `FK71AA0D8671D65EE9` FOREIGN KEY (`AuthUser_id`) REFERENCES `AuthUser` (`id`),
  CONSTRAINT `FK71AA0D868305BF97` FOREIGN KEY (`authorityScopeADItems_id`) REFERENCES `ADDomainItem` (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `AuthUser_AuthSecurityRole`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `AuthUser_AuthSecurityRole` (
  `AuthUser_id` int(11) NOT NULL,
  `roles_id` int(11) NOT NULL,
  KEY `FKA97E96CA3FF1C26A` (`roles_id`),
  KEY `FKA97E96CA71D65EE9` (`AuthUser_id`),
  CONSTRAINT `FKA97E96CA71D65EE9` FOREIGN KEY (`AuthUser_id`) REFERENCES `AuthUser` (`id`),
  CONSTRAINT `FKA97E96CA3FF1C26A` FOREIGN KEY (`roles_id`) REFERENCES `AuthSecurityRole` (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `AuthUser_DocumentDatabase`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `AuthUser_DocumentDatabase` (
  `AuthUser_id` int(11) NOT NULL,
  `documentDatabases_id` int(11) NOT NULL,
  KEY `FKAFF7D0E2647183A2` (`documentDatabases_id`),
  KEY `FKAFF7D0E271D65EE9` (`AuthUser_id`),
  CONSTRAINT `FKAFF7D0E271D65EE9` FOREIGN KEY (`AuthUser_id`) REFERENCES `AuthUser` (`id`),
  CONSTRAINT `FKAFF7D0E2647183A2` FOREIGN KEY (`documentDatabases_id`) REFERENCES `DocumentDatabase` (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `Config`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `Config` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `name` varchar(255) DEFAULT NULL,
  `nameKey` varchar(255) DEFAULT NULL,
  `configKey` varchar(127) NOT NULL,
  `value` longtext,
  PRIMARY KEY (`id`),
  UNIQUE KEY `configKey` (`configKey`)
) ENGINE=InnoDB AUTO_INCREMENT=47 DEFAULT CHARSET=utf8mb4;
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `DashboardItem`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `DashboardItem` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `dasboardItemKey` varchar(255) NOT NULL,
  `userSettings_id` int(11) DEFAULT NULL,
  PRIMARY KEY (`id`),
  KEY `FK5ED9FA7C8D74989` (`userSettings_id`),
  CONSTRAINT `FK5ED9FA7C8D74989` FOREIGN KEY (`userSettings_id`) REFERENCES `UserSettings` (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `DataFormat`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `DataFormat` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `name` varchar(255) DEFAULT NULL,
  `nameKey` varchar(255) DEFAULT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB AUTO_INCREMENT=9 DEFAULT CHARSET=utf8mb4;
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `DataFormat_MIMEType`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `DataFormat_MIMEType` (
  `DataFormat_id` int(11) NOT NULL,
  `mimeTypes_id` int(11) NOT NULL,
  UNIQUE KEY `mimeTypes_id` (`mimeTypes_id`),
  KEY `FK1CFB2BACEC8FF069` (`DataFormat_id`),
  KEY `FK1CFB2BAC3CDB9E32` (`mimeTypes_id`),
  CONSTRAINT `FK1CFB2BAC3CDB9E32` FOREIGN KEY (`mimeTypes_id`) REFERENCES `MIMEType` (`id`),
  CONSTRAINT `FK1CFB2BACEC8FF069` FOREIGN KEY (`DataFormat_id`) REFERENCES `DataFormat` (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `Document`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `Document` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `DocumentDatabase`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `DocumentDatabase` (
  `id` int(11) NOT NULL,
  `rdbmsInformationTarget_id` int(11) DEFAULT NULL,
  PRIMARY KEY (`id`),
  KEY `FKD57B3FD653C6BDC7` (`id`),
  KEY `FKD57B3FD6B436FC09` (`rdbmsInformationTarget_id`),
  CONSTRAINT `FKD57B3FD6B436FC09` FOREIGN KEY (`rdbmsInformationTarget_id`) REFERENCES `RDBMSInformationTarget` (`id`),
  CONSTRAINT `FKD57B3FD653C6BDC7` FOREIGN KEY (`id`) REFERENCES `Argument` (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `DocumentDatabaseFileEntry`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `DocumentDatabaseFileEntry` (
  `createdDate` datetime DEFAULT NULL,
  `filename` varchar(255) DEFAULT NULL,
  `md5Hash` varchar(255) DEFAULT NULL,
  `id` int(11) NOT NULL,
  PRIMARY KEY (`id`),
  KEY `FK94AC8120E5E43025` (`id`),
  CONSTRAINT `FK94AC8120E5E43025` FOREIGN KEY (`id`) REFERENCES `Document` (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `DocumentDatabaseRDBMSEntry`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `DocumentDatabaseRDBMSEntry` (
  `originalId` varchar(255) DEFAULT NULL,
  `id` int(11) NOT NULL,
  PRIMARY KEY (`id`),
  KEY `FK1B895C72E5E43025` (`id`),
  KEY `originalIdIndex` (`originalId`(191)),
  CONSTRAINT `FK1B895C72E5E43025` FOREIGN KEY (`id`) REFERENCES `Document` (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `DocumentDatabase_DocumentDatabaseFileEntry`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `DocumentDatabase_DocumentDatabaseFileEntry` (
  `DocumentDatabase_id` int(11) NOT NULL,
  `fileEntries_id` int(11) NOT NULL,
  UNIQUE KEY `fileEntries_id` (`fileEntries_id`),
  KEY `FKC2F0D9F7D2F738D7` (`fileEntries_id`),
  KEY `FKC2F0D9F7B9CE9909` (`DocumentDatabase_id`),
  CONSTRAINT `FKC2F0D9F7B9CE9909` FOREIGN KEY (`DocumentDatabase_id`) REFERENCES `DocumentDatabase` (`id`),
  CONSTRAINT `FKC2F0D9F7D2F738D7` FOREIGN KEY (`fileEntries_id`) REFERENCES `DocumentDatabaseFileEntry` (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `DocumentDatabase_DocumentDatabaseRDBMSEntry`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `DocumentDatabase_DocumentDatabaseRDBMSEntry` (
  `DocumentDatabase_id` int(11) NOT NULL,
  `rdbmsEntries_id` int(11) NOT NULL,
  UNIQUE KEY `rdbmsEntries_id` (`rdbmsEntries_id`),
  KEY `FKB5D01E7BB9CE9909` (`DocumentDatabase_id`),
  KEY `FKB5D01E7B263C3E41` (`rdbmsEntries_id`),
  CONSTRAINT `FKB5D01E7B263C3E41` FOREIGN KEY (`rdbmsEntries_id`) REFERENCES `DocumentDatabaseRDBMSEntry` (`id`),
  CONSTRAINT `FKB5D01E7BB9CE9909` FOREIGN KEY (`DocumentDatabase_id`) REFERENCES `DocumentDatabase` (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `DocumentFingerprint`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `DocumentFingerprint` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `fingerprint` bigint(20) NOT NULL,
  `document_id` int(11) NOT NULL,
  PRIMARY KEY (`id`),
  KEY `FK8EFD58A941769C9` (`document_id`),
  CONSTRAINT `FK8EFD58A941769C9` FOREIGN KEY (`document_id`) REFERENCES `Document` (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `InformationDescription`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `InformationDescription` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `distance` int(11) NOT NULL,
  `distanceEnabled` bit(1) NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB AUTO_INCREMENT=5 DEFAULT CHARSET=utf8mb4;
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `InformationDescription_InformationFeature`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `InformationDescription_InformationFeature` (
  `InformationDescription_id` int(11) NOT NULL,
  `features_id` int(11) NOT NULL,
  UNIQUE KEY `features_id` (`features_id`),
  KEY `FKF468A5993E2B5B49` (`InformationDescription_id`),
  KEY `FKF468A599A8696FB6` (`features_id`),
  CONSTRAINT `FKF468A599A8696FB6` FOREIGN KEY (`features_id`) REFERENCES `InformationFeature` (`id`),
  CONSTRAINT `FKF468A5993E2B5B49` FOREIGN KEY (`InformationDescription_id`) REFERENCES `InformationDescription` (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `InformationFeature`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `InformationFeature` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `threshold` bigint(20) NOT NULL,
  `matcher_id` int(11) NOT NULL,
  PRIMARY KEY (`id`),
  KEY `FK58BE704A1AFEC0AB` (`matcher_id`),
  CONSTRAINT `FK58BE704A1AFEC0AB` FOREIGN KEY (`matcher_id`) REFERENCES `Matcher` (`id`)
) ENGINE=InnoDB AUTO_INCREMENT=5 DEFAULT CHARSET=utf8mb4;
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `InformationType`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `InformationType` (
  `id` int(11) NOT NULL,
  `informationDescription_id` int(11) NOT NULL,
  PRIMARY KEY (`id`),
  KEY `FK123ABA661955299D` (`id`),
  KEY `FK123ABA663E2B5B49` (`informationDescription_id`),
  CONSTRAINT `FK123ABA663E2B5B49` FOREIGN KEY (`informationDescription_id`) REFERENCES `InformationDescription` (`id`),
  CONSTRAINT `FK123ABA661955299D` FOREIGN KEY (`id`) REFERENCES `Item` (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `InformationType_DataFormat`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `InformationType_DataFormat` (
  `InformationType_id` int(11) NOT NULL,
  `dataFormats_id` int(11) NOT NULL,
  KEY `FK35CD07DACEF2F74B` (`InformationType_id`),
  KEY `FK35CD07DAF86C3E78` (`dataFormats_id`),
  CONSTRAINT `FK35CD07DAF86C3E78` FOREIGN KEY (`dataFormats_id`) REFERENCES `DataFormat` (`id`),
  CONSTRAINT `FK35CD07DACEF2F74B` FOREIGN KEY (`InformationType_id`) REFERENCES `InformationType` (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `InventoryBase`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `InventoryBase` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `name` varchar(255) DEFAULT NULL,
  `nameKey` varchar(255) DEFAULT NULL,
  `category_id` int(11) DEFAULT NULL,
  PRIMARY KEY (`id`),
  KEY `FKD4AAB62DD4393567` (`category_id`),
  CONSTRAINT `FKD4AAB62DD4393567` FOREIGN KEY (`category_id`) REFERENCES `InventoryCategory` (`id`)
) ENGINE=InnoDB AUTO_INCREMENT=14 DEFAULT CHARSET=utf8mb4;
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `InventoryCategory`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `InventoryCategory` (
  `editable` bit(1) NOT NULL,
  `id` int(11) NOT NULL,
  PRIMARY KEY (`id`),
  KEY `FKE0A05E5A3F451ED9` (`id`),
  CONSTRAINT `FKE0A05E5A3F451ED9` FOREIGN KEY (`id`) REFERENCES `InventoryBase` (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `InventoryItem`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `InventoryItem` (
  `id` int(11) NOT NULL,
  `item_id` int(11) NOT NULL,
  PRIMARY KEY (`id`),
  KEY `FKD4AE2A6F3F451ED9` (`id`),
  KEY `FKD4AE2A6F9777D649` (`item_id`),
  CONSTRAINT `FKD4AE2A6F9777D649` FOREIGN KEY (`item_id`) REFERENCES `Item` (`id`),
  CONSTRAINT `FKD4AE2A6F3F451ED9` FOREIGN KEY (`id`) REFERENCES `InventoryBase` (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `Item`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `Item` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB AUTO_INCREMENT=10 DEFAULT CHARSET=utf8mb4;
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `LicenseInformation`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `LicenseInformation` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `administrativeUserCount` bigint(20) NOT NULL,
  `expirationDate` bigint(20) NOT NULL,
  `licenseType` varchar(255) DEFAULT NULL,
  `userCount` bigint(20) NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `MIMEType`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `MIMEType` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `mimeType` varchar(255) NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB AUTO_INCREMENT=166 DEFAULT CHARSET=utf8mb4;
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `Matcher`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `Matcher` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `functionName` varchar(255) DEFAULT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB AUTO_INCREMENT=5 DEFAULT CHARSET=utf8mb4;
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `MatcherArgument`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `MatcherArgument` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `coupledArgument_id` int(11) NOT NULL,
  `coupledMatcher_id` int(11) NOT NULL,
  PRIMARY KEY (`id`),
  KEY `FK328FB1EFCEF1644F` (`coupledMatcher_id`),
  KEY `FK328FB1EFAB9BBF65` (`coupledArgument_id`),
  CONSTRAINT `FK328FB1EFAB9BBF65` FOREIGN KEY (`coupledArgument_id`) REFERENCES `Argument` (`id`),
  CONSTRAINT `FK328FB1EFCEF1644F` FOREIGN KEY (`coupledMatcher_id`) REFERENCES `Matcher` (`id`)
) ENGINE=InnoDB AUTO_INCREMENT=2 DEFAULT CHARSET=utf8mb4;
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `Network`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `Network` (
  `ipBase` bigint(20) NOT NULL,
  `ipMask` bigint(20) NOT NULL,
  `id` int(11) NOT NULL,
  PRIMARY KEY (`id`),
  KEY `FKD119F20E1955299D` (`id`),
  CONSTRAINT `FKD119F20E1955299D` FOREIGN KEY (`id`) REFERENCES `Item` (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `NonCascadingArgument`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `NonCascadingArgument` (
  `id` int(11) NOT NULL,
  `argument_id` int(11) NOT NULL,
  PRIMARY KEY (`id`),
  KEY `FK38F7252153C6BDC7` (`id`),
  KEY `FK38F72521E139EE89` (`argument_id`),
  CONSTRAINT `FK38F72521E139EE89` FOREIGN KEY (`argument_id`) REFERENCES `Argument` (`id`),
  CONSTRAINT `FK38F7252153C6BDC7` FOREIGN KEY (`id`) REFERENCES `Argument` (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `RDBMSConnection`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `RDBMSConnection` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `name` varchar(255) DEFAULT NULL,
  `nameKey` varchar(255) DEFAULT NULL,
  `dbType` varchar(255) NOT NULL,
  `jdbcUrl` varchar(255) NOT NULL,
  `loginPassword` varchar(255) NOT NULL,
  `loginUsername` varchar(255) NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `RDBMSEnumeratedValue`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `RDBMSEnumeratedValue` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `hashCode` int(11) NOT NULL,
  `originalId` varchar(255) DEFAULT NULL,
  `string` longtext,
  `informationTarget_id` int(11) NOT NULL,
  PRIMARY KEY (`id`),
  KEY `FKC95635FBF7578F3` (`informationTarget_id`),
  KEY `orginalIdIndex` (`originalId`(191)),
  KEY `hashCodeIndex` (`hashCode`),
  CONSTRAINT `FKC95635FBF7578F3` FOREIGN KEY (`informationTarget_id`) REFERENCES `RDBMSInformationTarget` (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `RDBMSInformationTarget`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `RDBMSInformationTarget` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `catalogName` varchar(255) DEFAULT NULL,
  `columnName` varchar(255) NOT NULL,
  `currentlyEnumerating` bit(1) DEFAULT NULL,
  `schemaName` varchar(255) DEFAULT NULL,
  `tableName` varchar(255) NOT NULL,
  `rdbmsConnection_id` int(11) NOT NULL,
  PRIMARY KEY (`id`),
  KEY `FK2BE07A875135548B` (`rdbmsConnection_id`),
  CONSTRAINT `FK2BE07A875135548B` FOREIGN KEY (`rdbmsConnection_id`) REFERENCES `RDBMSConnection` (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `RegularExpression`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `RegularExpression` (
  `regex` varchar(255) NOT NULL,
  `id` int(11) NOT NULL,
  PRIMARY KEY (`id`),
  KEY `FK19D5FB3453C6BDC7` (`id`),
  CONSTRAINT `FK19D5FB3453C6BDC7` FOREIGN KEY (`id`) REFERENCES `Argument` (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `RegularExpressionGroup`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `RegularExpressionGroup` (
  `id` int(11) NOT NULL,
  `rdbmsInformationTarget_id` int(11) DEFAULT NULL,
  PRIMARY KEY (`id`),
  KEY `FK7088EBAB53C6BDC7` (`id`),
  KEY `FK7088EBABB436FC09` (`rdbmsInformationTarget_id`),
  CONSTRAINT `FK7088EBABB436FC09` FOREIGN KEY (`rdbmsInformationTarget_id`) REFERENCES `RDBMSInformationTarget` (`id`),
  CONSTRAINT `FK7088EBAB53C6BDC7` FOREIGN KEY (`id`) REFERENCES `Argument` (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `RegularExpressionGroupEntry`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `RegularExpressionGroupEntry` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `regex` varchar(255) NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `RegularExpressionGroup_RegularExpressionGroupEntry`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `RegularExpressionGroup_RegularExpressionGroupEntry` (
  `RegularExpressionGroup_id` int(11) NOT NULL,
  `entries_id` int(11) NOT NULL,
  UNIQUE KEY `entries_id` (`entries_id`),
  KEY `FKF05888F3F826EC82` (`entries_id`),
  KEY `FKF05888F3AE03E549` (`RegularExpressionGroup_id`),
  CONSTRAINT `FKF05888F3AE03E549` FOREIGN KEY (`RegularExpressionGroup_id`) REFERENCES `RegularExpressionGroup` (`id`),
  CONSTRAINT `FKF05888F3F826EC82` FOREIGN KEY (`entries_id`) REFERENCES `RegularExpressionGroupEntry` (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `Revision`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `Revision` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `name` varchar(255) DEFAULT NULL,
  `nameKey` varchar(255) DEFAULT NULL,
  `date` datetime NOT NULL,
  `dumpPath` varchar(255) NOT NULL,
  `isInstalled` bit(1) NOT NULL,
  `restoreRevision_id` int(11) DEFAULT NULL,
  PRIMARY KEY (`id`),
  KEY `FKF44F1BFB58249A7B` (`restoreRevision_id`),
  CONSTRAINT `FKF44F1BFB58249A7B` FOREIGN KEY (`restoreRevision_id`) REFERENCES `Revision` (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `Rule`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `Rule` (
  `DTYPE` varchar(31) NOT NULL,
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `name` varchar(255) DEFAULT NULL,
  `nameKey` varchar(255) DEFAULT NULL,
  `action` varchar(255) NOT NULL,
  `description` varchar(255) DEFAULT NULL,
  `enabled` bit(1) NOT NULL,
  `priority` bigint(20) NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `RuleItem`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `RuleItem` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `item_id` int(11) NOT NULL,
  `rule_id` int(11) NOT NULL,
  PRIMARY KEY (`id`),
  KEY `FK31570C4F9777D649` (`item_id`),
  KEY `FK31570C4F75A7E1A9` (`rule_id`),
  CONSTRAINT `FK31570C4F75A7E1A9` FOREIGN KEY (`rule_id`) REFERENCES `Rule` (`id`),
  CONSTRAINT `FK31570C4F9777D649` FOREIGN KEY (`item_id`) REFERENCES `Item` (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `RuleUser`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `RuleUser` (
  `id` int(11) NOT NULL,
  PRIMARY KEY (`id`),
  KEY `FK315C7D071955299D` (`id`),
  CONSTRAINT `FK315C7D071955299D` FOREIGN KEY (`id`) REFERENCES `Item` (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `RuleUserAD`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `RuleUserAD` (
  `id` int(11) NOT NULL,
  `domainItem_id` int(11) NOT NULL,
  PRIMARY KEY (`id`),
  KEY `FK4C315F6AE00977F1` (`id`),
  KEY `FK4C315F6AA4B5454C` (`domainItem_id`),
  CONSTRAINT `FK4C315F6AA4B5454C` FOREIGN KEY (`domainItem_id`) REFERENCES `ADDomainItem` (`id`),
  CONSTRAINT `FK4C315F6AE00977F1` FOREIGN KEY (`id`) REFERENCES `RuleUser` (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `RuleUserStatic`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `RuleUserStatic` (
  `username` varchar(255) NOT NULL,
  `id` int(11) NOT NULL,
  PRIMARY KEY (`id`),
  KEY `FK2DC91655E00977F1` (`id`),
  CONSTRAINT `FK2DC91655E00977F1` FOREIGN KEY (`id`) REFERENCES `RuleUser` (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `USBDevice`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `USBDevice` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `action` varchar(255) NOT NULL,
  `comment` varchar(255) DEFAULT NULL,
  `deviceId` varchar(255) NOT NULL,
  `uniqId` varchar(255) NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `UserSettings`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `UserSettings` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `user_id` int(11) NOT NULL,
  PRIMARY KEY (`id`),
  KEY `FK3188652EFF216991` (`user_id`),
  CONSTRAINT `FK3188652EFF216991` FOREIGN KEY (`user_id`) REFERENCES `AuthUser` (`id`)
) ENGINE=InnoDB AUTO_INCREMENT=3 DEFAULT CHARSET=utf8mb4;
/*!40101 SET character_set_client = @saved_cs_client */;
/*!40103 SET TIME_ZONE=@OLD_TIME_ZONE */;

/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;
/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;
/*!40014 SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
/*!40111 SET SQL_NOTES=@OLD_SQL_NOTES */;

