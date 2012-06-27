-- Version setzen auf "creating" oder "updating"
UPDATE INFO set value='updating 0.0.2 to 0.0.3'  where key ='Version';

GRANT SELECT ON TABLE pdb.cross_section TO GROUP pdb_user;
GRANT SELECT ON TABLE pdb.cross_section_part TO GROUP pdb_user;
GRANT SELECT ON TABLE pdb."document" TO GROUP pdb_user;
GRANT SELECT ON TABLE pdb.event TO GROUP pdb_user;
GRANT SELECT ON TABLE pdb.info TO GROUP pdb_user;
GRANT SELECT ON TABLE pdb.point TO GROUP pdb_user;
GRANT SELECT ON TABLE pdb.point_kind TO GROUP pdb_user;
GRANT SELECT ON TABLE pdb.roughness TO GROUP pdb_user;
GRANT SELECT ON TABLE pdb.state TO GROUP pdb_user;
GRANT SELECT ON TABLE pdb.vegetation TO GROUP pdb_user;
GRANT SELECT ON TABLE pdb.water_body TO GROUP pdb_user;
GRANT SELECT ON TABLE pdb.waterlevel_fixation TO GROUP pdb_user;

GRANT SELECT, UPDATE, INSERT, DELETE ON TABLE pdb.cross_section TO GROUP pdb_admin;
GRANT SELECT, UPDATE, INSERT, DELETE ON TABLE pdb.cross_section_part TO GROUP pdb_admin;
GRANT SELECT, UPDATE, INSERT, DELETE ON TABLE pdb."document" TO GROUP pdb_admin;
GRANT SELECT, UPDATE, INSERT, DELETE ON TABLE pdb.event TO GROUP pdb_admin;
GRANT SELECT, UPDATE, INSERT, DELETE ON TABLE pdb.info TO GROUP pdb_admin;
GRANT SELECT, UPDATE, INSERT, DELETE ON TABLE pdb.point TO GROUP pdb_admin;
GRANT SELECT, UPDATE, INSERT, DELETE ON TABLE pdb.point_kind TO GROUP pdb_admin;
GRANT SELECT, UPDATE, INSERT, DELETE ON TABLE pdb.roughness TO GROUP pdb_admin; 
GRANT SELECT, UPDATE, INSERT, DELETE ON TABLE pdb.state TO GROUP pdb_admin;
GRANT SELECT, UPDATE, INSERT, DELETE ON TABLE pdb.vegetation TO GROUP pdb_admin;
GRANT SELECT, UPDATE, INSERT, DELETE ON TABLE pdb.water_body TO GROUP pdb_admin;
GRANT SELECT, UPDATE, INSERT, DELETE ON TABLE pdb.waterlevel_fixation TO GROUP pdb_admin;

GRANT SELECT ON TABLE pdb.seq_pdb TO GROUP pdb_admin;

INSERT INTO INFO("key", "value") VALUES ('srsXName', 'X');
INSERT INTO INFO("key", "value") VALUES ('srsMinX', '4300000.0');
INSERT INTO INFO("key", "value") VALUES ('srsMaxX', '4600000.0');
INSERT INTO INFO("key", "value") VALUES ('srsTolX', '0.0005');

INSERT INTO INFO("key", "value") VALUES ('srsYName', 'Y');
INSERT INTO INFO("key", "value") VALUES ('srsMinY', '5500000.0');
INSERT INTO INFO("key", "value") VALUES ('srsMaxY', '5800000.0');
INSERT INTO INFO("key", "value") VALUES ('srsTolY', '0.0005');

INSERT INTO INFO("key", "value") VALUES ('srsZName', 'Z');
INSERT INTO INFO("key", "value") VALUES ('srsMinZ', '-1000.0');
INSERT INTO INFO("key", "value") VALUES ('srsMaxZ', '10000.0');
INSERT INTO INFO("key", "value") VALUES ('srsTolZ', '0.0005');

-- Version endgültig setzen
UPDATE INFO set value='0.0.3' where key ='Version';
commit;
