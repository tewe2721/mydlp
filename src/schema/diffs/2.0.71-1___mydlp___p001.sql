ALTER TABLE ADDomainItem ADD CONSTRAINT domainId UNIQUE KEY(domainId, distinguishedNameHash);
