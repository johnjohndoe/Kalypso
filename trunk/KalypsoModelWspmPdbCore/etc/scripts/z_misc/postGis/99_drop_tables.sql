-- drop table info cascade;

drop table WATERLEVEL_FIXATION cascade;
drop table EVENT cascade;
drop table POINT cascade;
drop table CROSS_SECTION_PART cascade;
drop table CROSS_SECTION cascade;
drop table WATER_BODY cascade;
drop table STATE cascade;
drop table ROUGHNESS cascade;
drop table VEGETATION cascade;
drop table POINT_KIND cascade;

---- ODER Tabellen löschen einschließlich der Geometry-Registrierungen
--SELECT DropGeometryTable('waterlevel_fixation');
--drop table EVENT cascade;
--SELECT DropGeometryTable('point');
--SELECT DropGeometryTable('cross_section_part');
--SELECT DropGeometryTable('cross_section');
--SELECT DropGeometryTable('water_body');
--drop table STATE cascade;
--drop table ROUGHNESS cascade;
--drop table VEGETATION cascade;
--drop table POINT_KIND cascade;


