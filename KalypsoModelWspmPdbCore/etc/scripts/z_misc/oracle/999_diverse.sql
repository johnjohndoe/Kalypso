-- DELETE FROM USER_SDO_GEOM_METADATA WHERE TABLE_NAME = 'WATERLEVEL_FIXATION';

--select count(*) from     USER_SDO_GEOM_METADATA;
--commit;

---- verify geometries...
--SELECT sdo_geom.validate_geometry_with_context(q.location, 0.001) Fehler, q.id FROM point q WHERE sdo_geom.validate_geometry_with_context(q.location, 0.0005) <> 'TRUE';
--SELECT sdo_geom.validate_geometry_with_context(q.line, 0.001) Fehler, q.id, q.name FROM cross_section q WHERE sdo_geom.validate_geometry_with_context(q.line, 0.0005) <> 'TRUE';
--SELECT sdo_geom.validate_geometry_with_context(q.line, 0.001) Fehler, q.id FROM cross_section_part q WHERE sdo_geom.validate_geometry_with_context(q.line, 0.0005) <> 'TRUE';

--SQL> SELECT A.ObjectID, SDO_GEOM.VALIDATE_GEOMETRY_WITH_CONTEXT(A.SHAPE, M.diminfo) AS PROBLEM
--
--  2  FROM tot_errors A,USER_SDO_GEOM_METADATA M
--
--  3  WHERE M.table_name = 'TOT_ERRORS'
--
--  4  AND M.column_name = 'SHAPE'
--
--  5  AND SDO_GEOM.VALIDATE_GEOMETRY_WITH_CONTEXT(A.SHAPE, M.diminfo) <> 'TRUE';

--  describe sdo_coord_ref_system;
--SELECT s.CMPD_HORIZ_SRID FROM sdo_coord_ref_system s WHERE srid=31468;
--SELECT s.cs_bounds.get_WKT() FROM mdsys.sdo_cs_srs s WHERE srid=31468;


-- http://download.oracle.com/docs/cd/B19306_01/appdev.102/b14255/sdo_objrelschema.htm#BGHBAIDC
-- http://download.oracle.com/docs/cd/B19306_01/appdev.102/b14255/sdo_objrelschema.htm
--select cs.location.get_wkt() from pdb_admin.point cs;

--select cs.CS_NAME,cs.SRID,cs.wktext, cs.cs_bounds.get_wkt() from  MDSYS.CS_SRS cs WHERE srid=31468;
--select SDO_UTIL.TO_WKBGEOMETRY(cs.line) from cross_section cs where name='OHNE_117';
--select cs.line.Get_GType() from cross_section cs where name='OHNE_117';





--select index_name,index_type,status,domidx_status,domidx_opstatus from user_indexes where index_type = 'DOMAIN' and (domidx_status <> 'VALID' or domidx_opstatus <> 'VALID');

SELECT index_name, status, domidx_status, domidx_opstatus FROM user_indexes;