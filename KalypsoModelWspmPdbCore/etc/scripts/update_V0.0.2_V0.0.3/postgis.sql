-- Version setzen auf "creating" oder "updateing"
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

-- Version endgültig setzen
UPDATE INFO set value='0.0.3' where key ='Version';
commit;
