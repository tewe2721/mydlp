CREATE TABLE MatchingDetail (
    id INT(11) AUTO_INCREMENT NOT NULL,
    incidentLogId INT(11) NOT NULL,
    matcherId INT(11) NOT NULL,
    matchingData VARCHAR(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NULL DEFAULT NULL,
    INDEX incidentLogIndex (incidentLogId),
    PRIMARY KEY (id)
) ENGINE=InnoDB COLLATE=utf8mb4_general_ci;
