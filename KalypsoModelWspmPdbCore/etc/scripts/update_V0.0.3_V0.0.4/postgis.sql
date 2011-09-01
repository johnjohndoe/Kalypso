-- Version setzen auf "creating" oder "updateing"
UPDATE INFO set value='updateing 0.0.3 to 0.0.4'  where key ='Version';

-- refill tables roughness and vegetation to overcome encoding problems
delete from roughness;
delete from vegetation;

-- 08_prefill_table__roughness
INSERT INTO roughness (name, label, description, point_kind) VALUES ('-1', 'unknown', 'unknown','GAF');

INSERT INTO roughness (name, label, description, point_kind, k_value, kst_value) VALUES ('1', 'glatt', 'glatt', 'GAF',0.001, 80);
INSERT INTO roughness (name, label, description, point_kind, k_value, kst_value) VALUES ('2', 'Feinsand, Schlamm', 'Feinsand, Schlamm', 'GAF',0.03, 55);
INSERT INTO roughness (name, label, description, point_kind, k_value, kst_value) VALUES ('3', 'Sand oder Feinkies', 'Sand oder Feinkies', 'GAF',0.05, 53);
INSERT INTO roughness (name, label, description, point_kind, k_value, kst_value) VALUES ('4', 'Feinkies', 'Feinkies', 'GAF',0.05, 50);
INSERT INTO roughness (name, label, description, point_kind, k_value, kst_value) VALUES ('5', 'mittlerer Kies', 'mittlerer Kies', 'GAF',0.08, 40);
INSERT INTO roughness (name, label, description, point_kind, k_value, kst_value) VALUES ('6', 'Schotter, mittlerer Grobkies, verkrautete Erdkanäle', 'Schotter, mittlerer Grobkies, verkrautete Erdkanäle', 'GAF',0.082, 35);
INSERT INTO roughness (name, label, description, point_kind, k_value, kst_value) VALUES ('7', 'Lehm, Wasserpflanzen', 'Lehm, Wasserpflanzen', 'GAF',0.1, 33);
INSERT INTO roughness (name, label, description, point_kind, k_value, kst_value) VALUES ('8', 'Steinschüttung,stark geschiebeführender Fluss,Wurzeln', 'Steinschüttung,stark geschiebeführender Fluss,Wurzeln', 'GAF',0.15, 30);
INSERT INTO roughness (name, label, description, point_kind, k_value, kst_value) VALUES ('9', 'Kiesanlandung, Wurzelgeflecht', 'Kiesanlandung, Wurzelgeflecht', 'GAF',0.2, 28);
INSERT INTO roughness (name, label, description, point_kind, k_value, kst_value) VALUES ('10', 'grobe Steine, Geröllanlandung', 'grobe Steine, Geröllanlandung', 'GAF',0.3, 25);
INSERT INTO roughness (name, label, description, point_kind, k_value, kst_value) VALUES ('11', 'Gebirgsflüsse mit grobem Geröll, verkrautete Erdkanäle', 'Gebirgsflüsse mit grobem Geröll, verkrautete Erdkanäle', 'GAF',0.45, 22.5);
INSERT INTO roughness (name, label, description, point_kind, k_value, kst_value) VALUES ('12', 'Fels', 'Fels', 'GAF',0.6, 20);
INSERT INTO roughness (name, label, description, point_kind, k_value, kst_value) VALUES ('13', 'Wildbach', 'Wildbach', 'GAF',0.9, 15);
INSERT INTO roughness (name, label, description, point_kind, k_value, kst_value) VALUES ('14', 'Wildbach mit starkem Geschiebetrieb, roher Felsausbruch', 'Wildbach mit starkem Geschiebetrieb, roher Felsausbruch', 'GAF',2, 12);
INSERT INTO roughness (name, label, description, point_kind, k_value, kst_value) VALUES ('20', 'Stahl, Zementputz geglättet, Beton aus Vakuumschalung', 'Stahl, Zementputz geglättet, Beton aus Vakuumschalung', 'GAF',0.001, 95);
INSERT INTO roughness (name, label, description, point_kind, k_value, kst_value) VALUES ('21', 'Holz, ungehobelt', 'Holz, ungehobelt', 'GAF',0.002, 90);
INSERT INTO roughness (name, label, description, point_kind, k_value, kst_value) VALUES ('22', 'Beton, glatt, Asphaltbeton, Klinker, sorgfältig verfugt', 'Beton, glatt, Asphaltbeton, Klinker, sorgfältig verfugt', 'GAF',0.003, 75);
INSERT INTO roughness (name, label, description, point_kind, k_value, kst_value) VALUES ('23', 'Ziegelmauerwerk, Rauputz, Verbundpflaster', 'Ziegelmauerwerk, Rauputz, Verbundpflaster', 'GAF',0.005, 70);
INSERT INTO roughness (name, label, description, point_kind, k_value, kst_value) VALUES ('24', 'Beton rauh, glatte Bruchsteine', 'Beton rauh, glatte Bruchsteine', 'GAF',0.015, 60);
INSERT INTO roughness (name, label, description, point_kind, k_value, kst_value) VALUES ('25', 'Pflaster, ARMCO-Profile', 'Pflaster, ARMCO-Profile', 'GAF',0.04, 50);
INSERT INTO roughness (name, label, description, point_kind, k_value, kst_value) VALUES ('26', 'Beton mit Fugen, grobes Bruchsteinmauerwerk', 'Beton mit Fugen, grobes Bruchsteinmauerwerk', 'GAF',0.02, 48);
INSERT INTO roughness (name, label, description, point_kind, k_value, kst_value) VALUES ('27', 'Natursteine, rauh', 'Natursteine, rauh', 'GAF',0.09, 40);
INSERT INTO roughness (name, label, description, point_kind, k_value, kst_value) VALUES ('28', 'Spundwände', 'Spundwände', 'GAF',0.06, 35);
INSERT INTO roughness (name, label, description, point_kind, k_value, kst_value) VALUES ('29', 'Schotter, Steinschüttung, Rasengittersteine', 'Schotter, Steinschüttung, Rasengittersteine', 'GAF',0.2, 30);
INSERT INTO roughness (name, label, description, point_kind, k_value, kst_value) VALUES ('30', 'grobe Steinschüttung', 'grobe Steinschüttung', 'GAF',0.4, 25);
INSERT INTO roughness (name, label, description, point_kind, k_value, kst_value) VALUES ('31', 'Steinschüttung mit Krautbewuchs', 'Steinschüttung mit Krautbewuchs', 'GAF',0.5, 23.5);
INSERT INTO roughness (name, label, description, point_kind, k_value, kst_value) VALUES ('32', 'Rauhe Sohlrampe', 'Rauhe Sohlrampe', 'GAF',1.5, 15);
INSERT INTO roughness (name, label, description, point_kind, k_value, kst_value) VALUES ('50', 'Rasen', 'Rasen', 'GAF',0.06, 40);
INSERT INTO roughness (name, label, description, point_kind, k_value, kst_value) VALUES ('51', 'Gras, Acker ohne Bewuchs', 'Gras, Acker ohne Bewuchs', 'GAF',0.2, 30);
INSERT INTO roughness (name, label, description, point_kind, k_value, kst_value) VALUES ('52', 'Waldboden', 'Waldboden', 'GAF',0.24, 27);
INSERT INTO roughness (name, label, description, point_kind, k_value, kst_value) VALUES ('53', 'Wiese, felsiger Waldboden', 'Wiese, felsiger Waldboden', 'GAF',0.25, 25);
INSERT INTO roughness (name, label, description, point_kind, k_value, kst_value) VALUES ('54', 'Gras mit Stauden', 'Gras mit Stauden', 'GAF',0.3, 24);
INSERT INTO roughness (name, label, description, point_kind, k_value, kst_value) VALUES ('55', 'Krautiger Bewuchs', 'Krautiger Bewuchs', 'GAF',0.4, 22);
INSERT INTO roughness (name, label, description, point_kind, k_value, kst_value) VALUES ('56', 'Acker mit Kulturen', 'Acker mit Kulturen', 'GAF',0.6, 21);
INSERT INTO roughness (name, label, description, point_kind, k_value, kst_value) VALUES ('57', 'unregelmäßiges Vorland', 'unregelmäßiges Vorland', 'GAF',0.8, 15);
INSERT INTO roughness (name, label, description, point_kind, k_value, kst_value) VALUES ('58', 'sehr unregelmäßiges Vorland mit Verbauungen', 'sehr unregelmäßiges Vorland mit Verbauungen', 'GAF',1, 12);


-- 09_prefill_table__vegetation
INSERT INTO vegetation(name, label, description, point_kind, dp, ax, ay) VALUES ('-1', 'unknown', 'unknown','GAF',0,0,0);
INSERT INTO vegetation(name, label, description, point_kind, dp, ax, ay) VALUES ('1', 'Röhricht, licht', 'Röhricht, licht', 'GAF',0.003, 0.03, 0.03);
INSERT INTO vegetation(name, label, description, point_kind, dp, ax, ay) VALUES ('2', 'Röhricht, dicht', 'Röhricht, dicht', 'GAF',0.005, 0.02, 0.02);
INSERT INTO vegetation(name, label, description, point_kind, dp, ax, ay) VALUES ('3', 'Sträucher, licht', 'Sträucher, licht', 'GAF',0.03, 0.35, 0.35);
INSERT INTO vegetation(name, label, description, point_kind, dp, ax, ay) VALUES ('4', 'Sträucher, mittel', 'Sträucher, mittel', 'GAF',0.045, 0.25, 0.25);
INSERT INTO vegetation(name, label, description, point_kind, dp, ax, ay) VALUES ('5', 'Sträucher, dicht', 'Sträucher, dicht', 'GAF',0.06, 0.15, 0.15);
INSERT INTO vegetation(name, label, description, point_kind, dp, ax, ay) VALUES ('6', 'Bäume, licht', 'Bäume, licht', 'GAF',0.05, 5, 5);
INSERT INTO vegetation(name, label, description, point_kind, dp, ax, ay) VALUES ('7', 'Bäume, mittel', 'Bäume, mittel', 'GAF',0.2, 10, 10);
INSERT INTO vegetation(name, label, description, point_kind, dp, ax, ay) VALUES ('8', 'Bäume, dicht', 'Bäume, dicht', 'GAF',1, 5, 5);
INSERT INTO vegetation(name, label, description, point_kind, dp, ax, ay) VALUES ('11', 'Büsche, einjährig', 'Büsche, einjährig', 'GAF',0.5, 10, 10);
INSERT INTO vegetation(name, label, description, point_kind, dp, ax, ay) VALUES ('12', 'Büsche, mehrjährig', 'Büsche, mehrjährig', 'GAF',3.5, 10, 10);
INSERT INTO vegetation(name, label, description, point_kind, dp, ax, ay) VALUES ('13', 'Bäume, einjährig', 'Bäume, einjährig', 'GAF',0.05, 20, 20);
INSERT INTO vegetation(name, label, description, point_kind, dp, ax, ay) VALUES ('14', 'Bäume, mehrjährig', 'Bäume, mehrjährig', 'GAF',1, 20, 20);



-- Generated by Oracle SQL Developer Data Modeler 3.0.0.665
--   at:        2011-09-01 11:25:42 MESZ
--   site:      Oracle Database 10g
--   type:      Oracle Database 10g



ALTER TABLE Document DROP CONSTRAINT "Document Name UK" CASCADE 
;

ALTER TABLE Point DROP CONSTRAINT CS_Part__Point CASCADE ;

ALTER TABLE Point DROP CONSTRAINT Roughness__Point CASCADE ;

ALTER TABLE Point DROP CONSTRAINT Vegetation__Point CASCADE ;

ALTER TABLE Point DROP CONSTRAINT "Point PK" CASCADE ;
ALTER TABLE Point RENAME TO bcp_Point 
;

CREATE TABLE Point 
    ( 
     ID NUMBER (20)  NOT NULL , 
     Name VARCHAR2 (50)  NOT NULL , 
     Location SDO_GEOMETRY , 
     Consecutive_Num NUMBER (11)  NOT NULL , 
    
--  (GAF-) Kennzeichnung
 Code VARCHAR2 (50) , 
     HyK VARCHAR2 (50) , 
    
--  [m]
 Width NUMBER (8,4) , 
    
--  [m NN]
 Height NUMBER (8,4) , 
    
--  [mm]
 Roughness_K_Value NUMBER (8,1) , 
    
--  [m^.33/s]
 Roughness_Kst_Value NUMBER (8,1) , 
     Roughness_Factor NUMBER (8,5) , 
    
--  [m]
 Vegetation_Dp NUMBER (8,3) , 
    
--  [m]
 Vegetation_Ax NUMBER (8,3) , 
    
--  [m]
 Vegetation_Ay NUMBER (8,3) , 
     Description VARCHAR2 (255) , 
     Roughness_Point_Kind VARCHAR2 (50) , 
     Roughness VARCHAR2 (50) , 
     Vegetation_Point_Kind VARCHAR2 (50) , 
     Vegetation VARCHAR2 (50) , 
     Cross_Section_Part NUMBER (20)  NOT NULL 
    ) LOGGING 
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

COMMENT ON COLUMN Point.Roughness_Factor IS 'factor to apply to roughness value' 
;

COMMENT ON COLUMN Point.Vegetation_Dp IS '[m]' 
;

COMMENT ON COLUMN Point.Vegetation_Ax IS '[m]' 
;

COMMENT ON COLUMN Point.Vegetation_Ay IS '[m]' 
;

INSERT INTO Point 
    (ID , Name , Location , Consecutive_Num , Code , HyK , Width , Height , Roughness_K_Value , Roughness_Kst_Value , Vegetation_Dp , Vegetation_Ax , Vegetation_Ay , Description , Roughness_Point_Kind , Roughness , Vegetation_Point_Kind , Vegetation , Cross_Section_Part )
SELECT 
    ID , Name , Location , Consecutive_Num , Code , HyK , Width , Height , Roughness_K_Value , Roughness_Kst_Value , Vegetation_Dp , Vegetation_Ax , Vegetation_Ay , Description , Roughness_Point_Kind , Roughness , Vegetation_Point_Kind , Vegetation , Cross_Section_Part 
FROM 
    bcp_Point 
;

ALTER TABLE Point 
    ADD CONSTRAINT "Point PK" PRIMARY KEY ( ID  ) ;



ALTER TABLE Roughness DROP CONSTRAINT Point_Kind__Roughness CASCADE ;

ALTER TABLE Roughness DROP CONSTRAINT "Roughness PK" CASCADE ;
ALTER TABLE Roughness RENAME TO bcp_Roughness 
;

CREATE TABLE Roughness 
    ( 
     Name VARCHAR2 (50)  NOT NULL , 
    
--  [mm]
 K_Value NUMBER (8,1) , 
    
--  [m^.33/s]
 Kst_Value NUMBER (8,1) , 
     Label VARCHAR2 (100) , 
    
--  Angabe einer Literaturstelle
 Source VARCHAR2 (255) , 
     Validity VARCHAR2 (255) , 
     Description VARCHAR2 (255) , 
     Color Color , 
     Point_Kind VARCHAR2 (50)  NOT NULL 
    ) LOGGING 
;



COMMENT ON COLUMN Roughness.K_Value IS '[mm]' 
;

COMMENT ON COLUMN Roughness.Kst_Value IS '[m^.33/s]' 
;

COMMENT ON COLUMN Roughness.Source IS 'Angabe einer Literaturstelle' 
;

COMMENT ON COLUMN Roughness.Color IS 'color (hex)' 
;

INSERT INTO Roughness 
    (Name , K_Value , Kst_Value , Label , Source , Validity , Description , Point_Kind )
SELECT 
    Name , K_Value , Kst_Value , Label , Source , Validity , Description , Point_Kind 
FROM 
    bcp_Roughness 
;

ALTER TABLE Roughness 
    ADD CONSTRAINT "Roughness PK" PRIMARY KEY ( Name ,
     Point_Kind  ) ;



ALTER TABLE Vegetation DROP CONSTRAINT Point_Kind__Vegetation CASCADE ;

ALTER TABLE Vegetation DROP CONSTRAINT "Vegetation PK" CASCADE ;
ALTER TABLE Vegetation RENAME TO bcp_Vegetation 
;

CREATE TABLE Vegetation 
    ( 
     Name VARCHAR2 (50)  NOT NULL , 
    
--  [m]
 Dp NUMBER (8,3)  NOT NULL , 
    
--  [m]
 Ax NUMBER (8,3)  NOT NULL , 
    
--  [m]
 Ay NUMBER (8,3)  NOT NULL , 
     Label VARCHAR2 (100) , 
    
--  Angabe einer Literaturstelle
 Source VARCHAR2 (255) , 
     Description VARCHAR2 (255) , 
     Color Color , 
     Point_Kind VARCHAR2 (50)  NOT NULL 
    ) LOGGING 
;



COMMENT ON COLUMN Vegetation.Dp IS '[m]' 
;

COMMENT ON COLUMN Vegetation.Ax IS '[m]' 
;

COMMENT ON COLUMN Vegetation.Ay IS '[m]' 
;

COMMENT ON COLUMN Vegetation.Source IS 'Angabe einer Literaturstelle' 
;

COMMENT ON COLUMN Vegetation.Color IS 'color (hex)' 
;

INSERT INTO Vegetation 
    (Name , Dp , Ax , Ay , Label , Source , Description , Point_Kind )
SELECT 
    Name , Dp , Ax , Ay , Label , Source , Description , Point_Kind 
FROM 
    bcp_Vegetation 
;

ALTER TABLE Vegetation 
    ADD CONSTRAINT "Vegetation PK" PRIMARY KEY ( Name ,
     Point_Kind  ) ;




ALTER TABLE Point 
    ADD CONSTRAINT CS_Part__Point FOREIGN KEY 
    ( 
      Cross_Section_Part
    ) 
    REFERENCES Cross_Section_Part 
    ( 
     ID
    ) 
    ON DELETE CASCADE 
    NOT DEFERRABLE 
;


ALTER TABLE Point 
    ADD CONSTRAINT Roughness__Point FOREIGN KEY 
    ( 
      Roughness,
     Roughness_Point_Kind
    ) 
    REFERENCES Roughness 
    ( 
     Point_Kind,
     Name
    ) 
    ON DELETE SET NULL 
    NOT DEFERRABLE 
;


ALTER TABLE Point 
    ADD CONSTRAINT Vegetation__Point FOREIGN KEY 
    ( 
      Vegetation,
     Vegetation_Point_Kind
    ) 
    REFERENCES Vegetation 
    ( 
     Point_Kind,
     Name
    ) 
    ON DELETE SET NULL 
    NOT DEFERRABLE 
;


ALTER TABLE Roughness 
    ADD CONSTRAINT Point_Kind__Roughness FOREIGN KEY 
    ( 
      Point_Kind
    ) 
    REFERENCES Point_Kind 
    ( 
     Name
    ) 
    ON DELETE CASCADE 
    NOT DEFERRABLE 
;


ALTER TABLE Vegetation 
    ADD CONSTRAINT Point_Kind__Vegetation FOREIGN KEY 
    ( 
      Point_Kind
    ) 
    REFERENCES Point_Kind 
    ( 
     Name
    ) 
    ON DELETE CASCADE 
    NOT DEFERRABLE 
;


-- Oracle SQL Developer Data Modeler Summary Report: 
-- 
-- CREATE TABLE                             3
-- CREATE INDEX                             0
-- CREATE VIEW                              0
-- ALTER TABLE                             20
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


-- Version endgültig setzen
UPDATE INFO set value='0.0.4'  where key ='Version';
commit;