CREATE TABLE DocumentDatabaseExcludeFile (
    documentDatabaseRemoteStorage_id INT(11) NOT NULL,
    excludeFilename VARCHAR(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NULL DEFAULT NULL,
    INDEX FK50ED10C09E8B7A6B (documentDatabaseRemoteStorage_id)
) ENGINE=InnoDB COLLATE=utf8mb4_general_ci;
CREATE TABLE DocumentDatabaseRemoteStorage (
    id INT(11) AUTO_INCREMENT NOT NULL,
    remoteStorage_id INT(11) NULL DEFAULT NULL,
    INDEX FK6C72031F30AA64EB (remoteStorage_id),
    PRIMARY KEY (id)
) ENGINE=InnoDB COLLATE=utf8mb4_general_ci;
CREATE TABLE DocumentDatabase_DocumentDatabaseRemoteStorage (
    DocumentDatabase_id INT(11) NOT NULL,
    documentDatabaseRemoteStorages_id INT(11) NOT NULL,
    INDEX FKBCA01876B9CE9909 (DocumentDatabase_id),
    INDEX FKBCA0187677A36936 (documentDatabaseRemoteStorages_id)
) ENGINE=InnoDB COLLATE=utf8mb4_general_ci;
ALTER TABLE DocumentDatabaseExcludeFile ADD CONSTRAINT FK50ED10C09E8B7A6B FOREIGN KEY (documentDatabaseRemoteStorage_id) REFERENCES DocumentDatabaseRemoteStorage (id) ON UPDATE NO ACTION ON DELETE NO ACTION;
ALTER TABLE DocumentDatabaseRemoteStorage ADD CONSTRAINT FK6C72031F30AA64EB FOREIGN KEY (remoteStorage_id) REFERENCES RemoteStorage (id) ON UPDATE NO ACTION ON DELETE NO ACTION;
ALTER TABLE DocumentDatabase_DocumentDatabaseRemoteStorage ADD CONSTRAINT FKBCA0187677A36936 FOREIGN KEY (documentDatabaseRemoteStorages_id) REFERENCES DocumentDatabaseRemoteStorage (id) ON UPDATE NO ACTION ON DELETE NO ACTION, ADD CONSTRAINT FKBCA01876B9CE9909 FOREIGN KEY (DocumentDatabase_id) REFERENCES DocumentDatabase (id) ON UPDATE NO ACTION ON DELETE NO ACTION;
