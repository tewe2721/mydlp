ALTER TABLE RuleItem MODIFY COLUMN ruleColumn VARCHAR(16) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NULL DEFAULT NULL;
ALTER TABLE RuleItem ADD CONSTRAINT item_id UNIQUE KEY(item_id, rule_id, ruleColumn);
ALTER TABLE RuleItemGroup ADD CONSTRAINT group_id UNIQUE KEY(group_id, rule_id);
