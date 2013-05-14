ALTER TABLE RuleItem ADD COLUMN ruleColumn VARCHAR(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NULL DEFAULT NULL COMMENT '';
SET @@foreign_key_checks = 0;
ALTER TABLE SourceDomainName DROP FOREIGN KEY FK6DBC240A1955299D;
UPDATE RuleItem SET ruleColumn="SOURCE" WHERE item_id IN (SELECT id FROM SourceDomainName);
INSERT INTO Domain (id, destinationString) VALUES (SELECT id, sourceDomain as destinationString FROM SourceDomainName);
DROP TABLE SourceDomainName;
SET @@foreign_key_checks = 1;
