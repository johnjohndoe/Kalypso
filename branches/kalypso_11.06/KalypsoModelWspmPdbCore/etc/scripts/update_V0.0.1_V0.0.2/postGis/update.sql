-- Generated by Oracle SQL Developer Data Modeler 3.0.0.665
--   at:        2011-07-13 16:56:15 MESZ
--   site:      Oracle Database 10g
--   type:      Oracle Database 10g


CREATE TABLE Document
    (
     ID numeric (20)  NOT NULL ,
     Name varchar (100)  NOT NULL ,
     Location Geometry ,
     FileName varchar (2048)  NOT NULL ,
     MimeType varchar (50) , 
     Creation_Date TIMESTAMP (0)  NOT NULL , 
     Editing_Date TIMESTAMP (0)  NOT NULL , 
     Editing_User varchar (50)  NOT NULL , 
     Measurement_Date TIMESTAMP (0) ,

--  [�] - Aufnahmerichtung (0-360)
 ShotDirection numeric (8,3) CHECK ( ShotDirection BETWEEN 0 AND 360) ,

--  [�] - �ffnungswinkel (0-360)
 ViewAngle numeric (8,3) CHECK ( ViewAngle BETWEEN 0 AND 360) , 
     Description varchar (255) , 
     Cross_Section_ID numeric (20) ,
     Water_Body_ID numeric (20) ,
     State_ID numeric (20)
    ) 
;



COMMENT ON COLUMN Document.ShotDirection IS '[�] - Aufnahmerichtung (0-360)'
;

COMMENT ON COLUMN Document.ViewAngle IS '[�] - �ffnungswinkel (0-360)'
;

ALTER TABLE Document
    ADD CONSTRAINT "Document PK" PRIMARY KEY ( ID  ) ;

ALTER TABLE Document 
    ADD CONSTRAINT "Document Name UK" UNIQUE ( FileName ) ;




COMMENT ON COLUMN Cross_Section.Station IS '[m]'
;


COMMENT ON COLUMN Event.Type IS 'Art des Ereignisses: Messung, Simulation'
;


COMMENT ON COLUMN Point.Code IS '(GAF-) Kennzeichnung'
;


COMMENT ON COLUMN Point.Width IS '[m]'
;


COMMENT ON COLUMN Point.Height IS '[m NN]'
;


COMMENT ON COLUMN Point.Roughness_K_Value IS '[mm]'
;


COMMENT ON COLUMN Point.Roughness_Kst_Value IS '[m^.33/s]'
;


COMMENT ON COLUMN Point.Vegetation_Dp IS '[m]'
;


COMMENT ON COLUMN Point.Vegetation_Ax IS '[m]'
;


COMMENT ON COLUMN Point.Vegetation_Ay IS '[m]'
;


COMMENT ON COLUMN Point_Kind.Name IS 'Quelle f�r Import: GAF, WPROF, ...'
;


COMMENT ON COLUMN Roughness.K_Value IS '[mm]'
;


COMMENT ON COLUMN Roughness.Kst_Value IS '[m^.33/s]'
;


COMMENT ON COLUMN Roughness.Source IS 'Angabe einer Literaturstelle'
;


COMMENT ON COLUMN State.IsStateZero IS 'Handelt es sich bei diesem Zustand um einen Ur-Zustand (d.h. einen Erstimport)?'
;


COMMENT ON COLUMN State.Source IS 'data source (e.g. file name)'
;


COMMENT ON COLUMN Vegetation.Dp IS '[m]'
;


COMMENT ON COLUMN Vegetation.Ax IS '[m]'
;


COMMENT ON COLUMN Vegetation.Ay IS '[m]'
;


COMMENT ON COLUMN Vegetation.Source IS 'Angabe einer Literaturstelle'
;


COMMENT ON COLUMN Water_Body.Rank IS 'Gew�sserordnung (ggf. zur Darstellung)'
;


COMMENT ON COLUMN Waterlevel_Fixation.Station IS '[m]'
;


COMMENT ON COLUMN Waterlevel_Fixation.Waterlevel IS '[m NN]'
;

COMMENT ON COLUMN Waterlevel_Fixation.Discharge IS '[m�/s]'
;

ALTER TABLE Event DROP CONSTRAINT "Event Name UK" CASCADE
;

DROP INDEX "Event Name UKX"
;
CREATE UNIQUE INDEX "Event Name UKX" ON Event
    (
     Name,
     Water_Body ASC
    )
;

ALTER TABLE Event ADD
    CONSTRAINT "Event Name UK" UNIQUE (
    Name ,
     Water_Body
    ) ;



ALTER TABLE Document
    ADD CONSTRAINT CS_Document FOREIGN KEY
    (
     Cross_Section_ID
    )
    REFERENCES Cross_Section
    (
     ID
    )
    ON DELETE SET NULL
    NOT DEFERRABLE
;


ALTER TABLE Document
    ADD CONSTRAINT State_Document FOREIGN KEY
    (
     State_ID
    )
    REFERENCES State
    (
     ID
    )
    ON DELETE SET NULL
    NOT DEFERRABLE
;


ALTER TABLE Document
    ADD CONSTRAINT WB_Document FOREIGN KEY
    (
     Water_Body_ID
    )
    REFERENCES Water_Body
    (
     ID
    )
    ON DELETE SET NULL
    NOT DEFERRABLE
;


-- Oracle SQL Developer Data Modeler Summary Report:
--
-- CREATE TABLE                             1
-- CREATE INDEX                             0
-- CREATE VIEW                              0
-- ALTER TABLE                              8
-- DROP TABLE                               0
-- DROP INDEX                               0
-- CREATE SEQUENCE                          0
-- CREATE MATERIALIZED VIEW                 0
-- ALTER SEQUENCE                           0
-- ALTER MATERIALIZED VIEW                  0
-- DROP VIEW                                0

--
-- ERRORS                                   0
-- WARNINGS                                 0

-- 05_register_geometries
INSERT INTO geometry_columns(f_table_catalog, f_table_schema, f_table_name, f_geometry_column, coord_dimension, srid, "type")
VALUES ( '', 'pdb_admin', 'document', 'location', 3, 31468, 'POINT' );

-- 06_prefill_table_info
UPDATE INFO set value='0.0.2'  where key ='Version';
INSERT INTO INFO("KEY", "VALUE") VALUES ('DocumentServer', '');

-- 99_create_spatial_indexes
CREATE INDEX IX_DOCUMENT__LOCATION ON DOCUMENT USING GIST ( LOCATION );

commit;