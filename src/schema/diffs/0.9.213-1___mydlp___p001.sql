CREATE TABLE Endpoint (
    id INT(11) AUTO_INCREMENT NOT NULL,
    endpointAlias VARCHAR(32) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL,
    endpointId VARCHAR(32) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL,
    endpointSecret VARCHAR(32) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL,
    CONSTRAINT endpointId UNIQUE KEY(endpointId),
    CONSTRAINT endpointSecret UNIQUE KEY(endpointSecret),
    PRIMARY KEY (id)
) ENGINE=InnoDB COLLATE=utf8mb4_general_ci;
