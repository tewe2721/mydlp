
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
  `baseDistinguishedName` varchar(1024) NOT NULL,
  `baseDistinguishedNameHash` int(11) NOT NULL,
  `currentlyEnumerating` bit(1) DEFAULT NULL,
  `domainName` varchar(190) NOT NULL,
  `loginPassword` varchar(255) NOT NULL,
  `loginUsername` varchar(255) NOT NULL,
  `netbiosName` varchar(255) DEFAULT NULL,
  `serverIp` varchar(255) NOT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `baseDistinguishedNameHash` (`baseDistinguishedNameHash`)
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
  `domainId` int(11) NOT NULL,
  `parent_id` int(11) DEFAULT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `domainId` (`domainId`,`distinguishedNameHash`),
  KEY `FKAD83167ABE9EC806` (`parent_id`),
  KEY `distinguishedNameIndex` (`distinguishedNameHash`),
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
DROP TABLE IF EXISTS `ApplicationName`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `ApplicationName` (
  `destinationString` varchar(255) NOT NULL,
  `id` int(11) NOT NULL,
  PRIMARY KEY (`id`),
  KEY `FK7A3883DB1955299D` (`id`),
  CONSTRAINT `FK7A3883DB1955299D` FOREIGN KEY (`id`) REFERENCES `Item` (`id`)
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
) ENGINE=InnoDB AUTO_INCREMENT=103 DEFAULT CHARSET=utf8mb4;
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `AuthSecurityRole`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `AuthSecurityRole` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `roleName` varchar(255) DEFAULT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB AUTO_INCREMENT=6 DEFAULT CHARSET=utf8mb4;
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `AuthUser`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `AuthUser` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `email` varchar(255) DEFAULT NULL,
  `emailHashCode` int(11) NOT NULL,
  `hasAuthorityScope` bit(1) NOT NULL,
  `isActive` bit(1) DEFAULT NULL,
  `password` varchar(255) DEFAULT NULL,
  `username` varchar(127) DEFAULT NULL,
  `role_id` int(11) NOT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `emailHashCode` (`emailHashCode`),
  UNIQUE KEY `username` (`username`),
  KEY `FK59398DB3A1D94A11` (`role_id`),
  CONSTRAINT `FK59398DB3A1D94A11` FOREIGN KEY (`role_id`) REFERENCES `AuthSecurityRole` (`id`)
) ENGINE=InnoDB AUTO_INCREMENT=2 DEFAULT CHARSET=utf8mb4;
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
DROP TABLE IF EXISTS `BundledKeywordGroup`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `BundledKeywordGroup` (
  `descriptionKey` varchar(255) NOT NULL,
  `filename` varchar(255) NOT NULL,
  `id` int(11) NOT NULL,
  PRIMARY KEY (`id`),
  KEY `FK7C890B3853C6BDC7` (`id`),
  CONSTRAINT `FK7C890B3853C6BDC7` FOREIGN KEY (`id`) REFERENCES `Argument` (`id`)
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
) ENGINE=InnoDB AUTO_INCREMENT=66 DEFAULT CHARSET=utf8mb4;
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `CustomAction`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `CustomAction` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `name` varchar(255) DEFAULT NULL,
  `nameKey` varchar(255) DEFAULT NULL,
  `typeKey` varchar(255) NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `CustomActionDescription`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `CustomActionDescription` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `coupledCustomAction_id` int(11) NOT NULL,
  PRIMARY KEY (`id`),
  KEY `FK2AD48C55FBFE25C5` (`coupledCustomAction_id`),
  CONSTRAINT `FK2AD48C55FBFE25C5` FOREIGN KEY (`coupledCustomAction_id`) REFERENCES `CustomAction` (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `CustomActionDescriptionSeclore`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `CustomActionDescriptionSeclore` (
  `activityComment` varchar(255) NOT NULL,
  `hotFolderId` int(11) NOT NULL,
  `id` int(11) NOT NULL,
  PRIMARY KEY (`id`),
  KEY `FK399E64B23E0273C1` (`id`),
  CONSTRAINT `FK399E64B23E0273C1` FOREIGN KEY (`id`) REFERENCES `CustomActionDescription` (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `DailySchedule`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `DailySchedule` (
  `id` int(11) NOT NULL,
  PRIMARY KEY (`id`),
  KEY `FK547FDBB088EDF1A1` (`id`),
  CONSTRAINT `FK547FDBB088EDF1A1` FOREIGN KEY (`id`) REFERENCES `Schedule` (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;
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
) ENGINE=InnoDB AUTO_INCREMENT=5 DEFAULT CHARSET=utf8mb4;
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
  `currentlyFingerprinting` bit(1) DEFAULT NULL,
  `id` int(11) NOT NULL,
  `rdbmsInformationTarget_id` int(11) DEFAULT NULL,
  PRIMARY KEY (`id`),
  KEY `FKD57B3FD653C6BDC7` (`id`),
  KEY `FKD57B3FD6B436FC09` (`rdbmsInformationTarget_id`),
  CONSTRAINT `FKD57B3FD6B436FC09` FOREIGN KEY (`rdbmsInformationTarget_id`) REFERENCES `RDBMSInformationTarget` (`id`),
  CONSTRAINT `FKD57B3FD653C6BDC7` FOREIGN KEY (`id`) REFERENCES `Argument` (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `DocumentDatabaseExcludeFile`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `DocumentDatabaseExcludeFile` (
  `documentDatabaseRemoteStorage_id` int(11) NOT NULL,
  `excludeFilename` varchar(255) DEFAULT NULL,
  KEY `FK50ED10C09E8B7A6B` (`documentDatabaseRemoteStorage_id`),
  CONSTRAINT `FK50ED10C09E8B7A6B` FOREIGN KEY (`documentDatabaseRemoteStorage_id`) REFERENCES `DocumentDatabaseRemoteStorage` (`id`)
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
DROP TABLE IF EXISTS `DocumentDatabaseRemoteStorage`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `DocumentDatabaseRemoteStorage` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `remoteStorage_id` int(11) DEFAULT NULL,
  PRIMARY KEY (`id`),
  KEY `FK6C72031F30AA64EB` (`remoteStorage_id`),
  CONSTRAINT `FK6C72031F30AA64EB` FOREIGN KEY (`remoteStorage_id`) REFERENCES `RemoteStorage` (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `DocumentDatabase_DocumentDatabaseFileEntry`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `DocumentDatabase_DocumentDatabaseFileEntry` (
  `DocumentDatabase_id` int(11) NOT NULL,
  `fileEntries_id` int(11) NOT NULL,
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
DROP TABLE IF EXISTS `DocumentDatabase_DocumentDatabaseRemoteStorage`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `DocumentDatabase_DocumentDatabaseRemoteStorage` (
  `DocumentDatabase_id` int(11) NOT NULL,
  `documentDatabaseRemoteStorages_id` int(11) NOT NULL,
  KEY `FKBCA01876B9CE9909` (`DocumentDatabase_id`),
  KEY `FKBCA0187677A36936` (`documentDatabaseRemoteStorages_id`),
  CONSTRAINT `FKBCA0187677A36936` FOREIGN KEY (`documentDatabaseRemoteStorages_id`) REFERENCES `DocumentDatabaseRemoteStorage` (`id`),
  CONSTRAINT `FKBCA01876B9CE9909` FOREIGN KEY (`DocumentDatabase_id`) REFERENCES `DocumentDatabase` (`id`)
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
DROP TABLE IF EXISTS `Domain`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `Domain` (
  `destinationString` varchar(255) NOT NULL,
  `id` int(11) NOT NULL,
  PRIMARY KEY (`id`),
  KEY `FK7A58C0E41955299D` (`id`),
  CONSTRAINT `FK7A58C0E41955299D` FOREIGN KEY (`id`) REFERENCES `Item` (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `EmailNotificationItem`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `EmailNotificationItem` (
  `id` int(11) NOT NULL,
  PRIMARY KEY (`id`),
  KEY `FKC7FE323AAE930AA8` (`id`),
  CONSTRAINT `FKC7FE323AAE930AA8` FOREIGN KEY (`id`) REFERENCES `NotificationItem` (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `Endpoint`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `Endpoint` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `endpointAlias` varchar(32) NOT NULL,
  `endpointId` varchar(32) NOT NULL,
  `endpointSecret` varchar(32) NOT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `endpointId` (`endpointId`),
  UNIQUE KEY `endpointSecret` (`endpointSecret`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `EndpointItem`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `EndpointItem` (
  `id` int(11) NOT NULL,
  `endpoint_id` int(11) NOT NULL,
  PRIMARY KEY (`id`),
  KEY `FKD7128E681955299D` (`id`),
  KEY `FKD7128E68D69B5F89` (`endpoint_id`),
  CONSTRAINT `FKD7128E68D69B5F89` FOREIGN KEY (`endpoint_id`) REFERENCES `Endpoint` (`id`),
  CONSTRAINT `FKD7128E681955299D` FOREIGN KEY (`id`) REFERENCES `Item` (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `FileSystemDirectory`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `FileSystemDirectory` (
  `destinationString` varchar(255) NOT NULL,
  `id` int(11) NOT NULL,
  PRIMARY KEY (`id`),
  KEY `FK678733C21955299D` (`id`),
  CONSTRAINT `FK678733C21955299D` FOREIGN KEY (`id`) REFERENCES `Item` (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `Hostname`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `Hostname` (
  `hostname` varchar(255) NOT NULL,
  `id` int(11) NOT NULL,
  PRIMARY KEY (`id`),
  KEY `FKF1FBC0531955299D` (`id`),
  CONSTRAINT `FKF1FBC0531955299D` FOREIGN KEY (`id`) REFERENCES `Item` (`id`)
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
) ENGINE=InnoDB AUTO_INCREMENT=62 DEFAULT CHARSET=utf8mb4;
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
) ENGINE=InnoDB AUTO_INCREMENT=104 DEFAULT CHARSET=utf8mb4;
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
) ENGINE=InnoDB AUTO_INCREMENT=147 DEFAULT CHARSET=utf8mb4;
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
DROP TABLE IF EXISTS `InventoryGroup`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `InventoryGroup` (
  `itemType` varchar(255) NOT NULL,
  `id` int(11) NOT NULL,
  PRIMARY KEY (`id`),
  KEY `FKC0FA32A33F451ED9` (`id`),
  CONSTRAINT `FKC0FA32A33F451ED9` FOREIGN KEY (`id`) REFERENCES `InventoryBase` (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `InventoryItem`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `InventoryItem` (
  `id` int(11) NOT NULL,
  `group_id` int(11) DEFAULT NULL,
  `item_id` int(11) NOT NULL,
  PRIMARY KEY (`id`),
  KEY `FKD4AE2A6F3F451ED9` (`id`),
  KEY `FKD4AE2A6F9777D649` (`item_id`),
  KEY `FKD4AE2A6FC7D9C00D` (`group_id`),
  CONSTRAINT `FKD4AE2A6FC7D9C00D` FOREIGN KEY (`group_id`) REFERENCES `InventoryGroup` (`id`),
  CONSTRAINT `FKD4AE2A6F3F451ED9` FOREIGN KEY (`id`) REFERENCES `InventoryBase` (`id`),
  CONSTRAINT `FKD4AE2A6F9777D649` FOREIGN KEY (`item_id`) REFERENCES `Item` (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `Item`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `Item` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB AUTO_INCREMENT=107 DEFAULT CHARSET=utf8mb4;
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `LicenseInformation`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `LicenseInformation` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `administrativeUserCount` bigint(20) NOT NULL,
  `expirationDate` bigint(20) NOT NULL,
  `licenseType` varchar(255) DEFAULT NULL,
  `logoKey` varchar(255) DEFAULT NULL,
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
) ENGINE=InnoDB AUTO_INCREMENT=104 DEFAULT CHARSET=utf8mb4;
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
) ENGINE=InnoDB AUTO_INCREMENT=67 DEFAULT CHARSET=utf8mb4;
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
DROP TABLE IF EXISTS `NotificationItem`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `NotificationItem` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `authUser_id` int(11) NOT NULL,
  `rule_id` int(11) NOT NULL,
  PRIMARY KEY (`id`),
  KEY `FK2DCD4EBE75A7E1A9` (`rule_id`),
  KEY `FK2DCD4EBE71D65EE9` (`authUser_id`),
  CONSTRAINT `FK2DCD4EBE71D65EE9` FOREIGN KEY (`authUser_id`) REFERENCES `AuthUser` (`id`),
  CONSTRAINT `FK2DCD4EBE75A7E1A9` FOREIGN KEY (`rule_id`) REFERENCES `Rule` (`id`)
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
DROP TABLE IF EXISTS `RemoteStorage`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `RemoteStorage` (
  `id` int(11) NOT NULL,
  PRIMARY KEY (`id`),
  KEY `FKDE0857351955299D` (`id`),
  CONSTRAINT `FKDE0857351955299D` FOREIGN KEY (`id`) REFERENCES `Item` (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `RemoteStorageFTPFS`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `RemoteStorageFTPFS` (
  `address` varchar(255) DEFAULT NULL,
  `password` varchar(255) DEFAULT NULL,
  `path` varchar(255) DEFAULT NULL,
  `username` varchar(255) DEFAULT NULL,
  `id` int(11) NOT NULL,
  PRIMARY KEY (`id`),
  KEY `FK7B08B37A48A2BFE1` (`id`),
  CONSTRAINT `FK7B08B37A48A2BFE1` FOREIGN KEY (`id`) REFERENCES `RemoteStorage` (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `RemoteStorageNFS`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `RemoteStorageNFS` (
  `address` varchar(255) DEFAULT NULL,
  `path` varchar(255) DEFAULT NULL,
  `id` int(11) NOT NULL,
  PRIMARY KEY (`id`),
  KEY `FK2C9D8E4648A2BFE1` (`id`),
  CONSTRAINT `FK2C9D8E4648A2BFE1` FOREIGN KEY (`id`) REFERENCES `RemoteStorage` (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `RemoteStorageSSHFS`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `RemoteStorageSSHFS` (
  `address` varchar(255) DEFAULT NULL,
  `password` varchar(255) DEFAULT NULL,
  `path` varchar(255) DEFAULT NULL,
  `port` int(11) NOT NULL,
  `username` varchar(255) DEFAULT NULL,
  `id` int(11) NOT NULL,
  PRIMARY KEY (`id`),
  KEY `FK7BBF52A048A2BFE1` (`id`),
  CONSTRAINT `FK7BBF52A048A2BFE1` FOREIGN KEY (`id`) REFERENCES `RemoteStorage` (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `RemoteStorageWindowsShare`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `RemoteStorageWindowsShare` (
  `password` varchar(255) DEFAULT NULL,
  `uncPath` varchar(255) DEFAULT NULL,
  `username` varchar(255) DEFAULT NULL,
  `id` int(11) NOT NULL,
  PRIMARY KEY (`id`),
  KEY `FKF67C5AF148A2BFE1` (`id`),
  CONSTRAINT `FKF67C5AF148A2BFE1` FOREIGN KEY (`id`) REFERENCES `RemoteStorage` (`id`)
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
  `notificationEnabled` bit(1) DEFAULT NULL,
  `priority` bigint(20) NOT NULL,
  `userMessage` varchar(255) DEFAULT NULL,
  `customAction_id` int(11) DEFAULT NULL,
  PRIMARY KEY (`id`),
  KEY `FK270B1CCAE822E9` (`customAction_id`),
  CONSTRAINT `FK270B1CCAE822E9` FOREIGN KEY (`customAction_id`) REFERENCES `CustomAction` (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `RuleItem`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `RuleItem` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `ruleColumn` varchar(16) DEFAULT NULL,
  `item_id` int(11) NOT NULL,
  `rule_id` int(11) NOT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `item_id` (`item_id`,`rule_id`,`ruleColumn`),
  KEY `FK31570C4F9777D649` (`item_id`),
  KEY `FK31570C4F75A7E1A9` (`rule_id`),
  CONSTRAINT `FK31570C4F75A7E1A9` FOREIGN KEY (`rule_id`) REFERENCES `Rule` (`id`),
  CONSTRAINT `FK31570C4F9777D649` FOREIGN KEY (`item_id`) REFERENCES `Item` (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `RuleItemGroup`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `RuleItemGroup` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `group_id` int(11) NOT NULL,
  `rule_id` int(11) NOT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `group_id` (`group_id`,`rule_id`),
  KEY `FK121D537075A7E1A9` (`rule_id`),
  KEY `FK121D5370C7D9C00D` (`group_id`),
  CONSTRAINT `FK121D5370C7D9C00D` FOREIGN KEY (`group_id`) REFERENCES `InventoryGroup` (`id`),
  CONSTRAINT `FK121D537075A7E1A9` FOREIGN KEY (`rule_id`) REFERENCES `Rule` (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `RuleSchedule`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `RuleSchedule` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `rule_id` int(11) NOT NULL,
  `schedule_id` int(11) NOT NULL,
  `scheduleIntervals_id` int(11) NOT NULL,
  PRIMARY KEY (`id`),
  KEY `FKEBA925D3936C2849` (`schedule_id`),
  KEY `FKEBA925D375A7E1A9` (`rule_id`),
  KEY `FKEBA925D38732C12B` (`scheduleIntervals_id`),
  CONSTRAINT `FKEBA925D38732C12B` FOREIGN KEY (`scheduleIntervals_id`) REFERENCES `ScheduleIntervals` (`id`),
  CONSTRAINT `FKEBA925D375A7E1A9` FOREIGN KEY (`rule_id`) REFERENCES `Rule` (`id`),
  CONSTRAINT `FKEBA925D3936C2849` FOREIGN KEY (`schedule_id`) REFERENCES `Schedule` (`id`)
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
DROP TABLE IF EXISTS `Schedule`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `Schedule` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `hour` int(11) NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `ScheduleDayInterval`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `ScheduleDayInterval` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `hour00` bit(1) DEFAULT NULL,
  `hour01` bit(1) DEFAULT NULL,
  `hour02` bit(1) DEFAULT NULL,
  `hour03` bit(1) DEFAULT NULL,
  `hour04` bit(1) DEFAULT NULL,
  `hour05` bit(1) DEFAULT NULL,
  `hour06` bit(1) DEFAULT NULL,
  `hour07` bit(1) DEFAULT NULL,
  `hour08` bit(1) DEFAULT NULL,
  `hour09` bit(1) DEFAULT NULL,
  `hour10` bit(1) DEFAULT NULL,
  `hour11` bit(1) DEFAULT NULL,
  `hour12` bit(1) DEFAULT NULL,
  `hour13` bit(1) DEFAULT NULL,
  `hour14` bit(1) DEFAULT NULL,
  `hour15` bit(1) DEFAULT NULL,
  `hour16` bit(1) DEFAULT NULL,
  `hour17` bit(1) DEFAULT NULL,
  `hour18` bit(1) DEFAULT NULL,
  `hour19` bit(1) DEFAULT NULL,
  `hour20` bit(1) DEFAULT NULL,
  `hour21` bit(1) DEFAULT NULL,
  `hour22` bit(1) DEFAULT NULL,
  `hour23` bit(1) DEFAULT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `ScheduleIntervals`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `ScheduleIntervals` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `fri_id` int(11) DEFAULT NULL,
  `mon_id` int(11) DEFAULT NULL,
  `sat_id` int(11) DEFAULT NULL,
  `sun_id` int(11) DEFAULT NULL,
  `thu_id` int(11) DEFAULT NULL,
  `tue_id` int(11) DEFAULT NULL,
  `wed_id` int(11) DEFAULT NULL,
  PRIMARY KEY (`id`),
  KEY `FK76627B57E64E2349` (`mon_id`),
  KEY `FK76627B57DA843798` (`fri_id`),
  KEY `FK76627B57F0DFC409` (`sun_id`),
  KEY `FK76627B57EFC8A82F` (`sat_id`),
  KEY `FK76627B57F6CD22BF` (`wed_id`),
  KEY `FK76627B57F2908551` (`tue_id`),
  KEY `FK76627B57F1E099B4` (`thu_id`),
  CONSTRAINT `FK76627B57F1E099B4` FOREIGN KEY (`thu_id`) REFERENCES `ScheduleDayInterval` (`id`),
  CONSTRAINT `FK76627B57DA843798` FOREIGN KEY (`fri_id`) REFERENCES `ScheduleDayInterval` (`id`),
  CONSTRAINT `FK76627B57E64E2349` FOREIGN KEY (`mon_id`) REFERENCES `ScheduleDayInterval` (`id`),
  CONSTRAINT `FK76627B57EFC8A82F` FOREIGN KEY (`sat_id`) REFERENCES `ScheduleDayInterval` (`id`),
  CONSTRAINT `FK76627B57F0DFC409` FOREIGN KEY (`sun_id`) REFERENCES `ScheduleDayInterval` (`id`),
  CONSTRAINT `FK76627B57F2908551` FOREIGN KEY (`tue_id`) REFERENCES `ScheduleDayInterval` (`id`),
  CONSTRAINT `FK76627B57F6CD22BF` FOREIGN KEY (`wed_id`) REFERENCES `ScheduleDayInterval` (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `StringArgument`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `StringArgument` (
  `argument` varchar(255) NOT NULL,
  `id` int(11) NOT NULL,
  PRIMARY KEY (`id`),
  KEY `FK11614E0E53C6BDC7` (`id`),
  CONSTRAINT `FK11614E0E53C6BDC7` FOREIGN KEY (`id`) REFERENCES `Argument` (`id`)
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
) ENGINE=InnoDB AUTO_INCREMENT=2 DEFAULT CHARSET=utf8mb4;
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `WebServer`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `WebServer` (
  `address` varchar(255) DEFAULT NULL,
  `digDepth` int(11) NOT NULL,
  `port` int(11) NOT NULL,
  `proto` varchar(255) DEFAULT NULL,
  `startPath` varchar(255) DEFAULT NULL,
  `id` int(11) NOT NULL,
  PRIMARY KEY (`id`),
  KEY `FK32A237971955299D` (`id`),
  CONSTRAINT `FK32A237971955299D` FOREIGN KEY (`id`) REFERENCES `Item` (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `WeeklySchedule`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `WeeklySchedule` (
  `fri` bit(1) DEFAULT NULL,
  `mon` bit(1) DEFAULT NULL,
  `sat` bit(1) DEFAULT NULL,
  `sun` bit(1) DEFAULT NULL,
  `thu` bit(1) DEFAULT NULL,
  `tue` bit(1) DEFAULT NULL,
  `wed` bit(1) DEFAULT NULL,
  `id` int(11) NOT NULL,
  PRIMARY KEY (`id`),
  KEY `FK1E53325888EDF1A1` (`id`),
  CONSTRAINT `FK1E53325888EDF1A1` FOREIGN KEY (`id`) REFERENCES `Schedule` (`id`)
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

