ALTER TABLE Rule ADD COLUMN customAction_id INT(11) NULL DEFAULT NULL COMMENT '', ADD INDEX FK270B1CCAE822E9 (customAction_id);
CREATE TABLE BundledKeywordGroup (
    descriptionKey VARCHAR(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL,
    filename VARCHAR(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL,
    id INT(11) NOT NULL,
    PRIMARY KEY (id)
) ENGINE=InnoDB COLLATE=utf8mb4_general_ci;
CREATE TABLE CustomAction (
    id INT(11) AUTO_INCREMENT NOT NULL,
    name VARCHAR(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NULL DEFAULT NULL,
    nameKey VARCHAR(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NULL DEFAULT NULL,
    typeKey VARCHAR(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL,
    PRIMARY KEY (id)
) ENGINE=InnoDB COLLATE=utf8mb4_general_ci;
CREATE TABLE CustomActionDescription (
    id INT(11) AUTO_INCREMENT NOT NULL,
    coupledCustomAction_id INT(11) NOT NULL,
    INDEX FK2AD48C55FBFE25C5 (coupledCustomAction_id),
    PRIMARY KEY (id)
) ENGINE=InnoDB COLLATE=utf8mb4_general_ci;
CREATE TABLE CustomActionDescriptionSeclore (
    activityComment VARCHAR(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL,
    hotFolderId INT(11) NOT NULL,
    id INT(11) NOT NULL,
    PRIMARY KEY (id)
) ENGINE=InnoDB COLLATE=utf8mb4_general_ci;
ALTER TABLE BundledKeywordGroup ADD CONSTRAINT FK7C890B3853C6BDC7 FOREIGN KEY (id) REFERENCES Argument (id) ON UPDATE NO ACTION ON DELETE NO ACTION;
ALTER TABLE CustomActionDescription ADD CONSTRAINT FK2AD48C55FBFE25C5 FOREIGN KEY (coupledCustomAction_id) REFERENCES CustomAction (id) ON UPDATE NO ACTION ON DELETE NO ACTION;
ALTER TABLE CustomActionDescriptionSeclore ADD CONSTRAINT FK399E64B23E0273C1 FOREIGN KEY (id) REFERENCES CustomActionDescription (id) ON UPDATE NO ACTION ON DELETE NO ACTION;
ALTER TABLE Rule ADD CONSTRAINT FK270B1CCAE822E9 FOREIGN KEY FK270B1CCAE822E9 (customAction_id) REFERENCES CustomAction (id) ON UPDATE NO ACTION ON DELETE NO ACTION;
