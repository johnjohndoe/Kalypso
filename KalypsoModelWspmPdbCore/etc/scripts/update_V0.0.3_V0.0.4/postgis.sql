-- Version setzen auf "creating" oder "updateing"
UPDATE INFO set value='updating 0.0.3 to 0.0.4'  where key ='Version';

-- add missing rights
GRANT USAGE ON SCHEMA pdb TO pdb_user;
GRANT SELECT, UPDATE ON TABLE pdb.seq_pdb TO pdb_admin;

-- 
ALTER TABLE "document" DROP CONSTRAINT "Document Name UK";

ALTER TABLE "document" DROP CONSTRAINT cs_document;
ALTER TABLE "document"
  ADD CONSTRAINT cs_document FOREIGN KEY (cross_section_id)
      REFERENCES cross_section (id) MATCH SIMPLE
      ON UPDATE NO ACTION ON DELETE CASCADE;

ALTER TABLE "document" DROP CONSTRAINT state_document;
ALTER TABLE "document"
  ADD CONSTRAINT state_document FOREIGN KEY (state_id)
      REFERENCES state (id) MATCH SIMPLE
      ON UPDATE NO ACTION ON DELETE CASCADE;

ALTER TABLE "document" DROP CONSTRAINT wb_document;
ALTER TABLE "document"
  ADD CONSTRAINT wb_document FOREIGN KEY (water_body_id)
      REFERENCES water_body (id) MATCH SIMPLE
      ON UPDATE NO ACTION ON DELETE CASCADE;

ALTER TABLE Point ADD COLUMN Roughness_Factor numeric (8,5);
COMMENT ON COLUMN Point.Roughness_Factor IS 'factor to apply to roughness value';
ALTER TABLE pdb.point
   ALTER COLUMN roughness_k_value TYPE numeric(10,3);
COMMENT ON COLUMN point.roughness_k_value IS '[m]';

ALTER TABLE Roughness ADD COLUMN Color character varying(10);
COMMENT ON COLUMN Roughness.Color IS 'color (hex)';
ALTER TABLE pdb.roughness
   ALTER COLUMN k_value TYPE numeric(10,3);
COMMENT ON COLUMN Roughness.k_value IS '[m]';

ALTER TABLE Vegetation ADD COLUMN Color character varying(10);
COMMENT ON COLUMN Vegetation.Color IS 'color (hex)';

-- update tables roughness and vegetation to overcome encoding problems and wrong k_value digits
DELETE FROM pdb.roughness WHERE TRUE;

INSERT INTO pdb.roughness (name, label, description, point_kind) VALUES ('-1', 'unknown', 'unknown','GAF');
INSERT INTO pdb.roughness (name, label, description, point_kind, k_value, kst_value) VALUES ('1', 'glatt', 'glatt', 'GAF',0.001, 80);
INSERT INTO pdb.roughness (name, label, description, point_kind, k_value, kst_value) VALUES ('2', 'Feinsand, Schlamm', 'Feinsand, Schlamm', 'GAF',0.03, 55);
INSERT INTO pdb.roughness (name, label, description, point_kind, k_value, kst_value) VALUES ('3', 'Sand oder Feinkies', 'Sand oder Feinkies', 'GAF',0.05, 53);
INSERT INTO pdb.roughness (name, label, description, point_kind, k_value, kst_value) VALUES ('4', 'Feinkies', 'Feinkies', 'GAF',0.05, 50);
INSERT INTO pdb.roughness (name, label, description, point_kind, k_value, kst_value) VALUES ('5', 'mittlerer Kies', 'mittlerer Kies', 'GAF',0.08, 40);
INSERT INTO pdb.roughness (name, label, description, point_kind, k_value, kst_value) VALUES ('6', 'Schotter, mittlerer Grobkies, verkrautete Erdkanäle', 'Schotter, mittlerer Grobkies, verkrautete Erdkanäle', 'GAF',0.082, 35);
INSERT INTO pdb.roughness (name, label, description, point_kind, k_value, kst_value) VALUES ('7', 'Lehm, Wasserpflanzen', 'Lehm, Wasserpflanzen', 'GAF',0.1, 33);
INSERT INTO pdb.roughness (name, label, description, point_kind, k_value, kst_value) VALUES ('8', 'Steinschüttung,stark geschiebeführender Fluss,Wurzeln', 'Steinschüttung,stark geschiebeführender Fluss,Wurzeln', 'GAF',0.15, 30);
INSERT INTO pdb.roughness (name, label, description, point_kind, k_value, kst_value) VALUES ('9', 'Kiesanlandung, Wurzelgeflecht', 'Kiesanlandung, Wurzelgeflecht', 'GAF',0.2, 28);
INSERT INTO pdb.roughness (name, label, description, point_kind, k_value, kst_value) VALUES ('10', 'grobe Steine, Geröllanlandung', 'grobe Steine, Geröllanlandung', 'GAF',0.3, 25);
INSERT INTO pdb.roughness (name, label, description, point_kind, k_value, kst_value) VALUES ('11', 'Gebirgsflüsse mit grobem Geröll, verkrautete Erdkanäle', 'Gebirgsflüsse mit grobem Geröll, verkrautete Erdkanäle', 'GAF',0.45, 22.5);
INSERT INTO pdb.roughness (name, label, description, point_kind, k_value, kst_value) VALUES ('12', 'Fels', 'Fels', 'GAF',0.6, 20);
INSERT INTO pdb.roughness (name, label, description, point_kind, k_value, kst_value) VALUES ('13', 'Wildbach', 'Wildbach', 'GAF',0.9, 15);
INSERT INTO pdb.roughness (name, label, description, point_kind, k_value, kst_value) VALUES ('14', 'Wildbach mit starkem Geschiebetrieb, roher Felsausbruch', 'Wildbach mit starkem Geschiebetrieb, roher Felsausbruch', 'GAF',2, 12);
INSERT INTO pdb.roughness (name, label, description, point_kind, k_value, kst_value) VALUES ('20', 'Stahl, Zementputz geglättet, Beton aus Vakuumschalung', 'Stahl, Zementputz geglättet, Beton aus Vakuumschalung', 'GAF',0.001, 95);
INSERT INTO pdb.roughness (name, label, description, point_kind, k_value, kst_value) VALUES ('21', 'Holz, ungehobelt', 'Holz, ungehobelt', 'GAF',0.002, 90);
INSERT INTO pdb.roughness (name, label, description, point_kind, k_value, kst_value) VALUES ('22', 'Beton, glatt, Asphaltbeton, Klinker, sorgfältig verfugt', 'Beton, glatt, Asphaltbeton, Klinker, sorgfältig verfugt', 'GAF',0.003, 75);
INSERT INTO pdb.roughness (name, label, description, point_kind, k_value, kst_value) VALUES ('23', 'Ziegelmauerwerk, Rauputz, Verbundpflaster', 'Ziegelmauerwerk, Rauputz, Verbundpflaster', 'GAF',0.005, 70);
INSERT INTO pdb.roughness (name, label, description, point_kind, k_value, kst_value) VALUES ('24', 'Beton rauh, glatte Bruchsteine', 'Beton rauh, glatte Bruchsteine', 'GAF',0.015, 60);
INSERT INTO pdb.roughness (name, label, description, point_kind, k_value, kst_value) VALUES ('25', 'Pflaster, ARMCO-Profile', 'Pflaster, ARMCO-Profile', 'GAF',0.04, 50);
INSERT INTO pdb.roughness (name, label, description, point_kind, k_value, kst_value) VALUES ('26', 'Beton mit Fugen, grobes Bruchsteinmauerwerk', 'Beton mit Fugen, grobes Bruchsteinmauerwerk', 'GAF',0.02, 48);
INSERT INTO pdb.roughness (name, label, description, point_kind, k_value, kst_value) VALUES ('27', 'Natursteine, rauh', 'Natursteine, rauh', 'GAF',0.09, 40);
INSERT INTO pdb.roughness (name, label, description, point_kind, k_value, kst_value) VALUES ('28', 'Spundwände', 'Spundwände', 'GAF',0.06, 35);
INSERT INTO pdb.roughness (name, label, description, point_kind, k_value, kst_value) VALUES ('29', 'Schotter, Steinschüttung, Rasengittersteine', 'Schotter, Steinschüttung, Rasengittersteine', 'GAF',0.2, 30);
INSERT INTO pdb.roughness (name, label, description, point_kind, k_value, kst_value) VALUES ('30', 'grobe Steinschüttung', 'grobe Steinschüttung', 'GAF',0.4, 25);
INSERT INTO pdb.roughness (name, label, description, point_kind, k_value, kst_value) VALUES ('31', 'Steinschüttung mit Krautbewuchs', 'Steinschüttung mit Krautbewuchs', 'GAF',0.5, 23.5);
INSERT INTO pdb.roughness (name, label, description, point_kind, k_value, kst_value) VALUES ('32', 'Rauhe Sohlrampe', 'Rauhe Sohlrampe', 'GAF',1.5, 15);
INSERT INTO pdb.roughness (name, label, description, point_kind, k_value, kst_value) VALUES ('50', 'Rasen', 'Rasen', 'GAF',0.06, 40);
INSERT INTO pdb.roughness (name, label, description, point_kind, k_value, kst_value) VALUES ('51', 'Gras, Acker ohne Bewuchs', 'Gras, Acker ohne Bewuchs', 'GAF',0.2, 30);
INSERT INTO pdb.roughness (name, label, description, point_kind, k_value, kst_value) VALUES ('52', 'Waldboden', 'Waldboden', 'GAF',0.24, 27);
INSERT INTO pdb.roughness (name, label, description, point_kind, k_value, kst_value) VALUES ('53', 'Wiese, felsiger Waldboden', 'Wiese, felsiger Waldboden', 'GAF',0.25, 25);
INSERT INTO pdb.roughness (name, label, description, point_kind, k_value, kst_value) VALUES ('54', 'Gras mit Stauden', 'Gras mit Stauden', 'GAF',0.3, 24);
INSERT INTO pdb.roughness (name, label, description, point_kind, k_value, kst_value) VALUES ('55', 'Krautiger Bewuchs', 'Krautiger Bewuchs', 'GAF',0.4, 22);
INSERT INTO pdb.roughness (name, label, description, point_kind, k_value, kst_value) VALUES ('56', 'Acker mit Kulturen', 'Acker mit Kulturen', 'GAF',0.6, 21);
INSERT INTO pdb.roughness (name, label, description, point_kind, k_value, kst_value) VALUES ('57', 'unregelmäßiges Vorland', 'unregelmäßiges Vorland', 'GAF',0.8, 15);
INSERT INTO pdb.roughness (name, label, description, point_kind, k_value, kst_value) VALUES ('58', 'sehr unregelmäßiges Vorland mit Verbauungen', 'sehr unregelmäßiges Vorland mit Verbauungen', 'GAF',1, 12);


-- 09_prefill_table__vegetation
 update vegetation set label= 'Röhricht, licht', description='Röhricht, licht' where name ='1';
 update vegetation set label= 'Röhricht, dicht', description='Röhricht, dicht' where name ='2';
 update vegetation set label= 'Sträucher, licht', description='Sträucher, licht' where name ='3';
 update vegetation set label= 'Sträucher, mittel', description='Sträucher, mittel' where name ='4';
 update vegetation set label= 'Sträucher, dicht', description='Sträucher, dicht' where name ='5';
 update vegetation set label= 'Bäume, licht', description='Bäume, licht' where name ='6';
 update vegetation set label= 'Bäume, mittel', description='Bäume, mittel' where name ='7';
 update vegetation set label= 'Bäume, dicht', description='Bäume, dicht' where name ='8';
 update vegetation set label= 'Büsche, einjährig', description='Büsche, einjährig' where name ='11';
 update vegetation set label= 'Büsche, mehrjährig', description='Büsche, mehrjährig' where name ='12';
 update vegetation set label= 'Bäume, einjährig', description='Bäume, einjährig' where name ='13';
 update vegetation set label= 'Bäume, mehrjährig', description='Bäume, mehrjährig' where name ='14';


-- set default colors for vegetation and roughness
update vegetation set color='#FF0000' where name ='-1';
update vegetation set color='#00FFFF' where name ='1';
update vegetation set color='#00CED1' where name ='2';
update vegetation set color='#FFE1FF' where name ='3';
update vegetation set color='#EE7AE9' where name ='4';
update vegetation set color='#B452CD' where name ='5';
update vegetation set color='#8FBC8F' where name ='6';
update vegetation set color='#3CB357' where name ='7';
update vegetation set color='#006400' where name ='8';
update vegetation set color='#7CFC00' where name ='11';
update vegetation set color='#00CD00' where name ='12';
update vegetation set color='#CCCC00' where name ='13';
update vegetation set color='#999900' where name ='14';

-- natürl. Gewässer
update roughness set color='#FF0000' where name ='-1';
update roughness set color='#FFB90F' where name ='1';
update roughness set color='#FF8247' where name ='2';
update roughness set color='#EEE685' where name ='3';
update roughness set color='#EEEE00' where name ='4';
update roughness set color='#EEAD0E' where name ='5';
update roughness set color='#EE7942' where name ='6';
update roughness set color='#CDC673' where name ='7';
update roughness set color='#CDCD00' where name ='8';
update roughness set color='#CD950C' where name ='9';
update roughness set color='#CD6839' where name ='10';
update roughness set color='#8B864E' where name ='11';
update roughness set color='#8B8B00' where name ='12';
update roughness set color='#8B670B' where name ='13';
update roughness set color='#8B4726' where name ='14';
-- 
update roughness set color='#FCFCFC' where name ='20';
update roughness set color='#EAEAEA' where name ='21';
update roughness set color='#D8D8D8' where name ='22';
update roughness set color='#C6C6C6' where name ='23';
update roughness set color='#B4B4B4' where name ='24';
update roughness set color='#A2A2A2' where name ='25';
update roughness set color='#909090' where name ='26';
update roughness set color='#7E7E7E' where name ='27';
update roughness set color='#6C6C6C' where name ='28';
update roughness set color='#5A5A5A' where name ='29';
update roughness set color='#484848' where name ='30';
update roughness set color='#363636' where name ='31';
update roughness set color='#242424' where name ='32';
-- Vorländer
update roughness set color='#CCFF99' where name ='50';
update roughness set color='#98FB98' where name ='51';
update roughness set color='#00FF7F' where name ='52';
update roughness set color='#7FFF00' where name ='53';
update roughness set color='#32CD32' where name ='54';
update roughness set color='#00FA9A' where name ='55';
update roughness set color='#9ACD32' where name ='56';
update roughness set color='#228B22' where name ='57';
update roughness set color='#006400' where name ='58';


-- Version endgültig setzen
UPDATE INFO set value='0.0.4'  where key ='Version';
commit;