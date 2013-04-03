ALTER TABLE ADDomain DROP KEY domainName;
ALTER TABLE ADDomainItem DROP KEY distinguishedNameHash;
ALTER TABLE ADDomain ADD COLUMN baseDistinguishedName VARCHAR(1024) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL COMMENT '', ADD COLUMN baseDistinguishedNameHash INT(11) NOT NULL COMMENT '', ADD CONSTRAINT baseDistinguishedNameHash UNIQUE KEY(baseDistinguishedNameHash);
ALTER TABLE ADDomainItem ADD COLUMN domainId INT(11) NOT NULL COMMENT '', ADD INDEX domainIdIndex (domainId);
