-- ggf. noch ein re-create machen (oder erst droppen und dann neu)
-- ALTER INDEX IX_POINT__LOCATION REBUILD;
-- ALTER INDEX IX_CROSS_SECTION_PART__LINE REBUILD;
-- ALTER INDEX IX_CROSS_SECTION__LINE REBUILD;
-- ALTER INDEX IX_WATER_BODY__RIVERLINE REBUILD;
-- ALTER INDEX IX_WL_FIX__LOCATION REBUILD;

--DROP INDEX IX_POINT__LOCATION FORCE;
--DROP INDEX IX_CROSS_SECTION_PART__LINE FORCE;
--DROP INDEX IX_CROSS_SECTION__LINE FORCE;
--DROP INDEX IX_WATER_BODY__RIVERLINE FORCE;
--DROP INDEX IX_WL_FIX__LOCATION FORCE;
--commit;
--


-- SDO_INDX_DIMS ggf. auf 2 setzen (oder weg lassen)
-- layer_gtype nicht setzen
--CREATE INDEX IX_POINT__LOCATION ON "POINT"("LOCATION") INDEXTYPE IS MDSYS.SPATIAL_INDEX PARAMETERS('layer_gtype=2001 SDO_INDX_DIMS=3');
--CREATE INDEX IX_CROSS_SECTION_PART__LINE ON CROSS_SECTION_PART(LINE) INDEXTYPE IS MDSYS.SPATIAL_INDEX PARAMETERS('layer_gtype=2002 SDO_INDX_DIMS=3');
--CREATE INDEX IX_CROSS_SECTION__LINE ON CROSS_SECTION(LINE) INDEXTYPE IS MDSYS.SPATIAL_INDEX PARAMETERS('layer_gtype=2002 SDO_INDX_DIMS=3');
--CREATE INDEX IX_WATER_BODY__RIVERLINE ON WATER_BODY(RIVERLINE) INDEXTYPE IS MDSYS.SPATIAL_INDEX PARAMETERS('layer_gtype=2002 SDO_INDX_DIMS=3');
--CREATE INDEX IX_WL_FIX__LOCATION ON WATERLEVEL_FIXATION(LOCATION) INDEXTYPE IS MDSYS.SPATIAL_INDEX PARAMETERS('layer_gtype=2001 SDO_INDX_DIMS=3');

CREATE INDEX IX_POINT__LOCATION ON "POINT"("LOCATION") INDEXTYPE IS MDSYS.SPATIAL_INDEX ;
CREATE INDEX IX_CROSS_SECTION_PART__LINE ON CROSS_SECTION_PART(LINE) INDEXTYPE IS MDSYS.SPATIAL_INDEX ;
CREATE INDEX IX_CROSS_SECTION__LINE ON CROSS_SECTION(LINE) INDEXTYPE IS MDSYS.SPATIAL_INDEX ;
CREATE INDEX IX_WATER_BODY__RIVERLINE ON WATER_BODY(RIVERLINE) INDEXTYPE IS MDSYS.SPATIAL_INDEX ;
CREATE INDEX IX_WL_FIX__LOCATION ON WATERLEVEL_FIXATION(LOCATION) INDEXTYPE IS MDSYS.SPATIAL_INDEX ;
commit;

