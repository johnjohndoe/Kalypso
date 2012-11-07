-- Version setzen auf "creating" oder "updating"
UPDATE INFO set value='updating 0.0.4 to 0.0.5'  where key ='Version';


UPDATE INFO set value='updating 0.0.4 to 0.0.5: create CS_Part_Param'  where key ='Version';
CREATE TABLE CS_Part_Parameter
    (
     ID NUMBER (20)  NOT NULL ,
     Key VARCHAR2 (50)  NOT NULL ,
     Value VARCHAR2 (255)  NOT NULL ,
     Cross_Section_Part_ID NUMBER (20)  NOT NULL
    )
;


ALTER TABLE CS_Part_Parameter
    ADD CONSTRAINT "CS_Part_Parameter PK" PRIMARY KEY ( ID  ) ;

ALTER TABLE CS_Part_Parameter
    ADD CONSTRAINT "CS_Part_Parameter UK" UNIQUE ( Key ,
     Cross_Section_Part_ID ) ;



UPDATE INFO set value='updating 0.0.4 to 0.0.5: create CS_Part_Type'  where key ='Version';
CREATE TABLE CS_Part_Type
    (
     Category VARCHAR2 (50)  NOT NULL ,
     Description VARCHAR2 (255) ,
     Style_Array_ID NUMBER (20)  NOT NULL
    )
;


ALTER TABLE CS_Part_Type
    ADD CONSTRAINT "CS_Part_Type PK" PRIMARY KEY ( Category  ) ;


UPDATE INFO set value='updating 0.0.4 to 0.0.5: create DHM_Index'  where key ='Version';
CREATE TABLE DHM_Index
    (
     ID NUMBER (20)  NOT NULL ,
     Name VARCHAR2 (100)  NOT NULL ,
     Location MDSYS.SDO_GEOMETRY  NOT NULL ,
     FileName VARCHAR2 (2048)  NOT NULL ,
     MimeType VARCHAR2 (50)  NOT NULL ,
     Creation_Date TIMESTAMP (0)  NOT NULL ,
     Editing_Date TIMESTAMP (0)  NOT NULL ,
     Editing_User VARCHAR2 (50)  NOT NULL ,
     Measurement_Date TIMESTAMP (0) ,
     Source VARCHAR2 (255) ,
     Editor VARCHAR2 (255) ,
     Measurement_Accuracy VARCHAR2 (50) ,
     Description VARCHAR2 (255) ,
     Copyright VARCHAR2 (255),
     SRID VARCHAR2 (15)  NOT NULL
    )
;



COMMENT ON COLUMN DHM_Index.Source IS 'Source / Author'
;

COMMENT ON COLUMN DHM_Index.Editor IS 'Editor (German: Herausgeber)'
;

ALTER TABLE DHM_Index
    ADD CONSTRAINT "DHM_Index PK" PRIMARY KEY ( ID  ) ;

ALTER TABLE DHM_Index
    ADD CONSTRAINT "DHM_Index UK" UNIQUE ( FileName ) ;




UPDATE INFO set value='updating 0.0.4 to 0.0.5: refactor Event'  where key ='Version';
ALTER TABLE Event DROP CONSTRAINT Water_Body__Event CASCADE ;

ALTER TABLE Waterlevel_Fixation DROP CONSTRAINT Event__Waterlevel_Fixation CASCADE ;

ALTER TABLE Event DROP CONSTRAINT "Event PK" CASCADE ;

ALTER TABLE Event DROP CONSTRAINT "Event Name UK" CASCADE ;
ALTER TABLE Event RENAME TO bcp_Event
;

CREATE TABLE Event
    (
     ID NUMBER (20)  NOT NULL ,
     Name VARCHAR2 (100)  NOT NULL ,
     Creation_Date TIMESTAMP (0)  NOT NULL ,
     Editing_Date TIMESTAMP (0)  NOT NULL ,
     Editing_User VARCHAR2 (50)  NOT NULL ,
     Measurement_Date TIMESTAMP (0) ,
     Source VARCHAR2 (255) ,
     Type VARCHAR2 (50) ,
     WL_Type VARCHAR2 (25)  NOT NULL ,
     Description VARCHAR2 (255) ,
     Water_Body NUMBER (20)  NOT NULL ,
     State_ID NUMBER (20) ,
     Style_Array_ID NUMBER (20)
    )
;



COMMENT ON COLUMN Event.Type IS 'Art des Ereignisses: Messung, Simulation'
;

INSERT INTO Event
    (ID , Name , Creation_Date , Editing_Date , Editing_User , Measurement_Date , Source , Type, WL_Type , Description , Water_Body )
SELECT
    ID , Name , Creation_Date , Editing_Date , Editing_User , Measurement_Date , Source , Type, 'WL_1D' , Description , Water_Body
FROM
    bcp_Event
;

ALTER TABLE Event
    ADD CONSTRAINT "Event PK" PRIMARY KEY ( ID  ) ;

ALTER TABLE Event
    ADD CONSTRAINT "Event Name UK" UNIQUE ( Name ,
     Water_Body,
     State_ID ) ;




UPDATE INFO set value='updating 0.0.4 to 0.0.5: create Style'  where key ='Version';
CREATE TABLE Style
    (
     ID NUMBER (20)  NOT NULL ,
     Consecutive_Num NUMBER (11)  NOT NULL ,
     Name VARCHAR2 (50)  NOT NULL ,
     Description VARCHAR2 (255) ,
     Style_Array_ID NUMBER (20)  NOT NULL
    )
;


ALTER TABLE Style
    ADD CONSTRAINT "Style PK" PRIMARY KEY ( ID  ) ;

ALTER TABLE Style
    ADD CONSTRAINT "Style Name UK" UNIQUE ( Name ) ;


ALTER TABLE Style
    ADD CONSTRAINT "Style Number in Array UK" UNIQUE ( Consecutive_Num ,
     Style_Array_ID ) ;




UPDATE INFO set value='updating 0.0.4 to 0.0.5: create Style_Array'  where key ='Version';
CREATE TABLE Style_Array
    (
     ID NUMBER (20)  NOT NULL ,
     Name VARCHAR2 (50)  NOT NULL
    )
;


ALTER TABLE Style_Array
    ADD CONSTRAINT "Style_Array PK" PRIMARY KEY ( ID  ) ;



UPDATE INFO set value='updating 0.0.4 to 0.0.5: create Style_Param'  where key ='Version';
CREATE TABLE Style_Parameter
    (
     ID NUMBER (20)  NOT NULL ,
     Key VARCHAR2 (50)  NOT NULL ,
     Value VARCHAR2 (255)  NOT NULL ,
     Style_ID NUMBER (20)  NOT NULL
    )
;


ALTER TABLE Style_Parameter
    ADD CONSTRAINT "Style_Parameter PK" PRIMARY KEY ( ID  ) ;


UPDATE INFO set value='updating 0.0.4 to 0.0.5: alter CS_Part'  where key ='Version';
ALTER TABLE Cross_Section_Part ADD
    (
     Event_ID NUMBER (20)
    )
;


UPDATE INFO set value='updating 0.0.4 to 0.0.5: alter CS_Part_Param'  where key ='Version';
ALTER TABLE CS_Part_Parameter
    ADD CONSTRAINT CS_Part__CS_Part_Param FOREIGN KEY
    (
     Cross_Section_Part_ID
    )
    REFERENCES Cross_Section_Part
    (
     ID
    )
    ON DELETE CASCADE
    NOT DEFERRABLE
;


UPDATE INFO set value='updating 0.0.4 to 0.0.5: alter CS_Part_Type'  where key ='Version';
ALTER TABLE CS_Part_Type
    ADD CONSTRAINT Style_Array__CS_Part_Type FOREIGN KEY
    (
     Style_Array_ID
    )
    REFERENCES Style_Array
    (
     ID
    )
    NOT DEFERRABLE
;
UPDATE INFO set value='updating 0.0.4 to 0.0.5: insert into Style_Array'  where key ='Version';
INSERT INTO style_array (id, name) VALUES (1,'Profil' );
INSERT INTO style_array (id, name) VALUES (2,'Schlammsohle' );
INSERT INTO style_array (id, name) VALUES (3,'WSP-Punkte' );
INSERT INTO style_array (id, name) VALUES (4,'Allgemeiner Bauwerkspunkt' );
INSERT INTO style_array (id, name) VALUES (5,'Bauwerksunterkante' );
INSERT INTO style_array (id, name) VALUES (6,'Durchlass' );
INSERT INTO style_array (id, name) VALUES (7,'Bauwerksoberkante' );
INSERT INTO style_array (id, name) VALUES (8,'Energieverlust' );
INSERT INTO style_array (id, name) VALUES (9,'Sinuosität' );
INSERT INTO style_array (id, name) VALUES (10,'WSP-Linien' );

UPDATE INFO set value='updating 0.0.4 to 0.0.5: insert into Style'  where key ='Version';
INSERT INTO style (id, consecutive_num, name, description, style_array_id) VALUES (1, 1, 'Profillinie', '', 1 );
INSERT INTO style (id, consecutive_num, name, description, style_array_id) VALUES (2, 1, 'Schlammsohlenlinie', '', 2 );
INSERT INTO style (id, consecutive_num, name, description, style_array_id) VALUES (3, 1, 'Wasserspiegel (Punkte)', '', 3 );
INSERT INTO style (id, consecutive_num, name, description, style_array_id) VALUES (4, 1, 'Allgemeiner Bauwerkspunkt', '', 4 );
INSERT INTO style (id, consecutive_num, name, description, style_array_id) VALUES (5, 1, 'Bauwerksunterkante (Linie)', '', 5 );
INSERT INTO style (id, consecutive_num, name, description, style_array_id) VALUES (6, 1, 'Durchlass (Punkt)', '', 6 );
INSERT INTO style (id, consecutive_num, name, description, style_array_id) VALUES (7, 1, 'Bauwerksoberkante (Linie)', '', 7 );
INSERT INTO style (id, consecutive_num, name, description, style_array_id) VALUES (8, 1, 'Wasserspiegel (Linie)', '', 10 );

UPDATE INFO set value='updating 0.0.4 to 0.0.5: insert into Style_Param'  where key ='Version';
INSERT INTO style_parameter (id, key, value, style_id) VALUES (1, 'type', 'line', 1);
INSERT INTO style_parameter (id, key, value, style_id) VALUES (2, 'strokeWidth', '2', 1);
INSERT INTO style_parameter (id, key, value, style_id) VALUES (3, 'strokeColor', '#FF9600', 1);
INSERT INTO style_parameter (id, key, value, style_id) VALUES (4, 'type', 'line', 2);
INSERT INTO style_parameter (id, key, value, style_id) VALUES (5, 'strokeWidth', '2', 2);
INSERT INTO style_parameter (id, key, value, style_id) VALUES (6, 'strokeColor', '#BE2CBE', 2);
INSERT INTO style_parameter (id, key, value, style_id) VALUES (7, 'type', 'point', 3);
INSERT INTO style_parameter (id, key, value, style_id) VALUES (8, 'markerWidth', '8', 3);
INSERT INTO style_parameter (id, key, value, style_id) VALUES (9, 'markerHeight', '8', 3);
INSERT INTO style_parameter (id, key, value, style_id) VALUES (10, 'fillColor', '#FFFFFF', 3);
INSERT INTO style_parameter (id, key, value, style_id) VALUES (11, 'strokeWidth', '2', 3);
INSERT INTO style_parameter (id, key, value, style_id) VALUES (12, 'strokeColor', '#00AAFF', 3);
INSERT INTO style_parameter (id, key, value, style_id) VALUES (13, 'type', 'point', 4);
INSERT INTO style_parameter (id, key, value, style_id) VALUES (14, 'markerWidth', '8', 4);
INSERT INTO style_parameter (id, key, value, style_id) VALUES (15, 'markerHeight', '8', 4);
INSERT INTO style_parameter (id, key, value, style_id) VALUES (16, 'fillColor', '#DA9696', 4);
INSERT INTO style_parameter (id, key, value, style_id) VALUES (17, 'strokeWidth', '1', 4);
INSERT INTO style_parameter (id, key, value, style_id) VALUES (18, 'strokeColor', '#000000', 4);
INSERT INTO style_parameter (id, key, value, style_id) VALUES (19, 'type', 'line', 5);
INSERT INTO style_parameter (id, key, value, style_id) VALUES (20, 'strokeWidth', '2', 5);
INSERT INTO style_parameter (id, key, value, style_id) VALUES (21, 'strokeColor', '#0080B3', 5);
INSERT INTO style_parameter (id, key, value, style_id) VALUES (22, 'type', 'point', 6);
INSERT INTO style_parameter (id, key, value, style_id) VALUES (23, 'markerWidth', '8', 6);
INSERT INTO style_parameter (id, key, value, style_id) VALUES (24, 'markerHeight', '8', 6);
INSERT INTO style_parameter (id, key, value, style_id) VALUES (25, 'fillColor', '#FF00FF', 6);
INSERT INTO style_parameter (id, key, value, style_id) VALUES (26, 'strokeWidth', '1', 6);
INSERT INTO style_parameter (id, key, value, style_id) VALUES (27, 'strokeColor', '#000000', 6);
INSERT INTO style_parameter (id, key, value, style_id) VALUES (28, 'type', 'line', 7);
INSERT INTO style_parameter (id, key, value, style_id) VALUES (29, 'strokeWidth', '2', 7);
INSERT INTO style_parameter (id, key, value, style_id) VALUES (30, 'strokeColor', '#008000', 7);
INSERT INTO style_parameter (id, key, value, style_id) VALUES (31, 'type', 'line', 8);
INSERT INTO style_parameter (id, key, value, style_id) VALUES (32, 'strokeWidth', '2', 8);
INSERT INTO style_parameter (id, key, value, style_id) VALUES (33, 'strokeColor', '#00AAFF', 8);

UPDATE INFO set value='updating 0.0.4 to 0.0.5: insert into CS_Part_Type'  where key ='Version';
INSERT INTO cs_part_type (category, description, style_array_id) VALUES ('P', 'Profil', 1);
INSERT INTO cs_part_type (category, description, style_array_id) VALUES ('S', 'Schlammsohle', 2);
INSERT INTO cs_part_type (category, description, style_array_id) VALUES ('W', 'WSP-Punkte', 3);
INSERT INTO cs_part_type (category, description, style_array_id) VALUES ('W2D', 'WSP-Linien', 10);
INSERT INTO cs_part_type (category, description, style_array_id) VALUES ('A', 'Allgemeiner Bauwerkspunkt', 4);
INSERT INTO cs_part_type (category, description, style_array_id) VALUES ('UK', 'Bauwerksunterkante', 5);
INSERT INTO cs_part_type (category, description, style_array_id) VALUES ('K', 'Kreisdurchlass', 6);
INSERT INTO cs_part_type (category, description, style_array_id) VALUES ('EI', 'Ei-Norm-Profil', 6);
INSERT INTO cs_part_type (category, description, style_array_id) VALUES ('MA', 'Maul-Norm-Profil', 6);
INSERT INTO cs_part_type (category, description, style_array_id) VALUES ('AR', 'ARMCO71-Profil', 6);
INSERT INTO cs_part_type (category, description, style_array_id) VALUES ('HA', 'HAMCO84-Profil', 6);
INSERT INTO cs_part_type (category, description, style_array_id) VALUES ('OK', 'Bauwerksoberkante', 7);
INSERT INTO cs_part_type (category, description, style_array_id) VALUES ('TR', 'Trapez', 6);
INSERT INTO cs_part_type (category, description, style_array_id) VALUES ('ENERGYLOSS', 'Energieverlust', 8);
INSERT INTO cs_part_type (category, description, style_array_id) VALUES ('SINUOSITAET', 'Sinuosität', 9);

UPDATE INFO set value='updating 0.0.4 to 0.0.5: alter CS_Part (2)'  where key ='Version';
ALTER TABLE Cross_Section_Part
    ADD CONSTRAINT CS_Part_Type__CS_Part FOREIGN KEY
    (
     Category
    )
    REFERENCES CS_Part_Type
    (
     Category
    )
    NOT DEFERRABLE
;

UPDATE INFO set value='updating 0.0.4 to 0.0.5: alter CS_Part (3)'  where key ='Version';
ALTER TABLE Cross_Section_Part
    ADD CONSTRAINT Event__CS_Part FOREIGN KEY
    (
     Event_ID
    )
    REFERENCES Event
    (
     ID
    )
    ON DELETE CASCADE
    NOT DEFERRABLE
;


UPDATE INFO set value='updating 0.0.4 to 0.0.5: alter Event'  where key ='Version';
ALTER TABLE Event
    ADD CONSTRAINT State__Event FOREIGN KEY
    (
     State_ID
    )
    REFERENCES State
    (
     ID
    )
    ON DELETE CASCADE
    NOT DEFERRABLE
;


UPDATE INFO set value='updating 0.0.4 to 0.0.5: alter Event (2)'  where key ='Version';
ALTER TABLE Event
    ADD CONSTRAINT Style_Array__Event FOREIGN KEY
    (
     Style_Array_ID
    )
    REFERENCES Style_Array
    (
     ID
    )
    ON DELETE SET NULL
    NOT DEFERRABLE
;


UPDATE INFO set value='updating 0.0.4 to 0.0.5: alter Event (3)'  where key ='Version';
ALTER TABLE Event
    ADD CONSTRAINT Water_Body__Event FOREIGN KEY
    (
      Water_Body
    )
    REFERENCES Water_Body
    (
     ID
    )
    ON DELETE CASCADE
    NOT DEFERRABLE
;


UPDATE INFO set value='updating 0.0.4 to 0.0.5: alter Style'  where key ='Version';
ALTER TABLE Style
    ADD CONSTRAINT Style_Array__Style FOREIGN KEY
    (
     Style_Array_ID
    )
    REFERENCES Style_Array
    (
     ID
    )
    ON DELETE CASCADE
    NOT DEFERRABLE
;

UPDATE INFO set value='updating 0.0.4 to 0.0.5: alter Style_Param'  where key ='Version';
ALTER TABLE Style_Parameter
    ADD CONSTRAINT Style__Style_Param FOREIGN KEY
    (
     Style_ID
    )
    REFERENCES Style
    (
     ID
    )
    ON DELETE CASCADE
    NOT DEFERRABLE
;


UPDATE INFO set value='updating 0.0.4 to 0.0.5: alter WL_Fix'  where key ='Version';
ALTER TABLE Waterlevel_Fixation
    ADD CONSTRAINT Event__Waterlevel_Fixation FOREIGN KEY
    (
      Event
    )
    REFERENCES Event
    (
     ID
    )
    ON DELETE CASCADE
    NOT DEFERRABLE
;
-- insert geometry info
UPDATE INFO set value='updating 0.0.4 to 0.0.5: insert geometry info'  where key ='Version';

-- register_geometries
-- Oracle
INSERT INTO USER_SDO_GEOM_METADATA (TABLE_NAME,COLUMN_NAME,DIMINFO,SRID)
VALUES('DHM_INDEX','LOCATION',
MDSYS.SDO_DIM_ARRAY
(MDSYS.SDO_DIM_ELEMENT('${srsXName}',${srsMinX},${srsMaxX},${srsTolX}),
MDSYS.SDO_DIM_ELEMENT('${srsYName}',${srsMinY},${srsMaxY},${srsTolY}),
MDSYS.SDO_DIM_ELEMENT('${srsZName}',${srsMinZ},${srsMaxZ},${srsTolZ})
), ${SRID}
);
CREATE INDEX IX_DHM_INDEX__LOCATION ON "DHM_INDEX"("LOCATION") INDEXTYPE IS MDSYS.SPATIAL_INDEX ;


-- drop temp table 'bcp_event'
UPDATE INFO set value='updating 0.0.4 to 0.0.5: drop temp table ''bcp_event'''  where key ='Version';
DROP TABLE BCP_EVENT;

-- set root path for DHM files
UPDATE INFO set value='updating 0.0.4 to 0.0.5: set root path for DHM files (''DEMServer'')'  where key ='Version';
INSERT INTO INFO("KEY", "VALUE") VALUES ('DEMServer', '${DEMServer}');


-- grant rights to new tables
UPDATE INFO set value='updating 0.0.4 to 0.0.5: grant rights'  where key ='Version';

GRANT SELECT ON CS_PART_PARAMETER TO PDB_USER;
GRANT SELECT ON CS_PART_TYPE TO PDB_USER;
GRANT SELECT ON DHM_INDEX TO PDB_USER;
GRANT SELECT ON EVENT TO PDB_USER;
GRANT SELECT ON STYLE TO PDB_USER;
GRANT SELECT ON STYLE_ARRAY TO PDB_USER;
GRANT SELECT ON STYLE_PARAMETER TO PDB_USER;

GRANT SELECT, UPDATE, INSERT, DELETE ON CS_PART_PARAMETER TO PDB_ADMIN;
GRANT SELECT, UPDATE, INSERT, DELETE ON CS_PART_TYPE TO PDB_ADMIN;
GRANT SELECT, UPDATE, INSERT, DELETE ON DHM_INDEX TO PDB_ADMIN;
GRANT SELECT, UPDATE, INSERT, DELETE ON EVENT TO PDB_ADMIN;
GRANT SELECT, UPDATE, INSERT, DELETE ON STYLE TO PDB_ADMIN;
GRANT SELECT, UPDATE, INSERT, DELETE ON STYLE_ARRAY TO PDB_ADMIN;
GRANT SELECT, UPDATE, INSERT, DELETE ON STYLE_PARAMETER TO PDB_ADMIN;

-- Version endgültig setzen
UPDATE INFO set value='12.11.0'  where key ='Version';
