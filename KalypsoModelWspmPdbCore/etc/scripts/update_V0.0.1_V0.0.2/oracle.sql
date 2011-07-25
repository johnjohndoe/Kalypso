-- Version setzen auf "creating" oder "updateing"
UPDATE INFO set value='updateing 0.0.1 to 0.0.2'  where key ='Version';
-- 03_create_tables

-- Generated by Oracle SQL Developer Data Modeler 3.0.0.665
--   at:        2011-07-13 16:56:15 MESZ
--   site:      Oracle Database 10g
--   type:      Oracle Database 10g

CREATE TABLE Document
    (
     ID NUMBER (20)  NOT NULL ,
     Name VARCHAR2 (100)  NOT NULL ,
     Location MDSYS.SDO_GEOMETRY ,
     FileName VARCHAR2 (2048)  NOT NULL , 
     MimeType VARCHAR2 (50) ,
     Creation_Date TIMESTAMP (0)  NOT NULL , 
     Editing_Date TIMESTAMP (0)  NOT NULL , 
     Editing_User VARCHAR2 (50)  NOT NULL , 
     Measurement_Date TIMESTAMP (0) , 

--  [°] - Aufnahmerichtung (0-360)
 ShotDirection NUMBER (8,3) CHECK ( ShotDirection BETWEEN 0 AND 360) ,

--  [°] - Öffnungswinkel (0-360)
 ViewAngle NUMBER (8,3) CHECK ( ViewAngle BETWEEN 0 AND 360) ,
     Description VARCHAR2 (255) , 
     Cross_Section_ID NUMBER (20) ,
     Water_Body_ID NUMBER (20) ,
     State_ID NUMBER (20)
    ) 
;


COMMENT ON COLUMN Document.ShotDirection IS '[°] - Aufnahmerichtung (0-360)'
;

COMMENT ON COLUMN Document.ViewAngle IS '[°] - Öffnungswinkel (0-360)'
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


COMMENT ON COLUMN Point_Kind.Name IS 'Quelle für Import: GAF, WPROF, ...'
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


COMMENT ON COLUMN Water_Body.Rank IS 'Gewässerordnung (ggf. zur Darstellung)'
;


COMMENT ON COLUMN Waterlevel_Fixation.Station IS '[m]'
;


COMMENT ON COLUMN Waterlevel_Fixation.Waterlevel IS '[m NN]'
;


COMMENT ON COLUMN Waterlevel_Fixation.Discharge IS '[m³/s]'
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
INSERT INTO USER_SDO_GEOM_METADATA (TABLE_NAME,COLUMN_NAME,DIMINFO,SRID)
VALUES('DOCUMENT','LOCATION',
MDSYS.SDO_DIM_ARRAY
(MDSYS.SDO_DIM_ELEMENT('X',4300000,4600000,0.0005),
MDSYS.SDO_DIM_ELEMENT('Y',5600000,5800000,0.0005),
MDSYS.SDO_DIM_ELEMENT('Z',-1000,10000,0.0005)
), 31468
);

-- 06_prefill_table_info
INSERT INTO INFO("KEY", "VALUE") VALUES ('DocumentServer', '');

-- 99_create_spatial_indexes
CREATE INDEX IX_DOCUMENT__LOCATION ON "DOCUMENT"("LOCATION") INDEXTYPE IS MDSYS.SPATIAL_INDEX ;


-- Version endgültig setzen
UPDATE INFO set value='0.0.2'  where key ='Version';
