ALTER TABLE EndpointStatus DROP INDEX ipAddressIndex, DROP INDEX firstAppearedIndex;
ALTER TABLE EndpointStatus MODIFY COLUMN ipAddress VARCHAR(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NULL DEFAULT NULL;
ALTER TABLE EndpointStatus ADD COLUMN discoverInProg bit(1) NULL DEFAULT NULL COMMENT '', ADD COLUMN endpointAlias VARCHAR(32) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL COMMENT '', ADD INDEX endpointAliasIndex (endpointAlias), ADD INDEX lastUpdateIndex (lastUpdate);
