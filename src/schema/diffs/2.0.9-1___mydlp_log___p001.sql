ALTER TABLE IncidentLog ADD COLUMN groupId VARCHAR(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NULL DEFAULT NULL COMMENT '';
CREATE TABLE OperationLog (
    id INT(11) AUTO_INCREMENT NOT NULL,
    context VARCHAR(16) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL,
    date DATETIME NOT NULL,
    groupId VARCHAR(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NULL DEFAULT NULL,
    message VARCHAR(1000) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NULL DEFAULT NULL,
    messageKey VARCHAR(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NULL DEFAULT NULL,
    ruleId BIGINT(20) NULL DEFAULT NULL,
    severity TINYINT(4) NOT NULL,
    source VARCHAR(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NULL DEFAULT NULL,
    visible bit(1) NOT NULL,
    INDEX visibleIndex (visible),
    INDEX dateIndex (`date`),
    PRIMARY KEY (id)
) ENGINE=InnoDB COLLATE=utf8mb4_general_ci;
