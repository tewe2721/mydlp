-- MyDLP
-- plugin_id: 9099

DELETE FROM plugin WHERE id = "9099";
DELETE FROM plugin_sid where plugin_id = "9099";

INSERT IGNORE INTO plugin (id, type, name, description, product_type, vendor) VALUES (9099, 1, 'mydlp', 'MyDLP: Data Loss Prevention', 7, 'MyDLP');

INSERT IGNORE INTO plugin_sid (plugin_id, sid, category_id, class_id, name, priority, reliability) VALUES (9099, 1, NULL, NULL, 'MyDLP: Logged incident', 5, 8);
INSERT IGNORE INTO plugin_sid (plugin_id, sid, category_id, class_id, name, priority, reliability) VALUES (9099, 2, NULL, NULL, 'MyDLP: Blocked incident', 5, 8);
INSERT IGNORE INTO plugin_sid (plugin_id, sid, category_id, class_id, name, priority, reliability) VALUES (9099, 3, NULL, NULL, 'MyDLP: Blocked incident and deleted incident file', 5, 8);
INSERT IGNORE INTO plugin_sid (plugin_id, sid, category_id, class_id, name, priority, reliability) VALUES (9099, 4, NULL, NULL, 'MyDLP: Blocked incident and quarantined incident file', 5, 8);
INSERT IGNORE INTO plugin_sid (plugin_id, sid, category_id, class_id, name, priority, reliability) VALUES (9099, 5, NULL, NULL, 'MyDLP: Logged incident and archived incident file', 5, 8);
INSERT IGNORE INTO plugin_sid (plugin_id, sid, category_id, class_id, name, priority, reliability) VALUES (9099, 6, NULL, NULL, 'MyDLP: Logged incident and encrypted incident file', 5, 8);
