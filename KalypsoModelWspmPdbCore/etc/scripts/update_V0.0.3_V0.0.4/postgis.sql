-- Version setzen auf "creating" oder "updateing"
UPDATE INFO set value='updating 0.0.3 to 0.0.4'  where key ='Version';

-- add missing rights
GRANT USAGE ON SCHEMA pdb TO pdb_user;
GRANT SELECT, UPDATE ON TABLE pdb.seq_pdb TO pdb_admin;

-- update tables roughness and vegetation to overcome encoding problems
 update roughness set label='Schotter, mittlerer Grobkies, verkrautete Erdkanäle', description='Schotter, mittlerer Grobkies, verkrautete Erdkanäle' where name ='6';
 update roughness set label='Steinschüttung, stark geschiebeführender Fluss, Wurzeln', description='Steinschüttung, stark geschiebeführender Fluss, Wurzeln' where name ='8';
 update roughness set label='grobe Steine, Geröllanlandung', description='grobe Steine, Geröllanlandung' where name ='10';
 update roughness set label='Gebirgsflüsse mit grobem Geröll, verkrautete Erdkanäle', description='Gebirgsflüsse mit grobem Geröll, verkrautete Erdkanäle' where name ='11';
 update roughness set label='Spundwände', description='Spundwände' where name ='28';
 update roughness set label='Schotter, Steinschüttung, Rasengittersteine', description='Schotter, Steinschüttung, Rasengittersteine' where name ='29';
 update roughness set label='grobe Steinschüttung', description='grobe Steinschüttung', point_kind='GAF' where name ='30';
 update roughness set label='Steinschüttung mit Krautbewuchs', description='Steinschüttung mit Krautbewuchs' where name ='31';
 update roughness set label='unregelmäßiges Vorland', description='unregelmäßiges Vorland' where name ='57';
 update roughness set label='sehr unregelmäßiges Vorland mit Verbauungen', description='sehr unregelmäßiges Vorland mit Verbauungen' where name ='58';
 update roughness set label='Stahl, Zementputz geglättet, Beton aus Vakuumschalung', description='Stahl, Zementputz geglättet, Beton aus Vakuumschalung' where name ='20';


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

ALTER TABLE Roughness ADD COLUMN Color character varying(10);
COMMENT ON COLUMN Roughness.Color IS 'color (hex)';

ALTER TABLE Vegetation ADD COLUMN Color character varying(10);
COMMENT ON COLUMN Vegetation.Color IS 'color (hex)';



-- Version endgültig setzen
UPDATE INFO set value='0.0.4'  where key ='Version';
commit;