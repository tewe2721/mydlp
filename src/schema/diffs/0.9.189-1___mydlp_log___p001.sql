CREATE TABLE TemporaryAccessToken (
    id INT(11) AUTO_INCREMENT NOT NULL,
    expirationDate DATETIME NOT NULL,
    ipAddress VARCHAR(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL,
    lastUpdate DATETIME NOT NULL,
    serviceName VARCHAR(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL,
    serviceParam VARCHAR(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NULL DEFAULT NULL,
    tokenKey VARCHAR(32) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL,
    username VARCHAR(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL,
    CONSTRAINT tokenKey UNIQUE KEY(tokenKey),
    INDEX tokenKeyIndex (tokenKey),
    PRIMARY KEY (id)
) ENGINE=InnoDB COLLATE=utf8mb4_general_ci;
