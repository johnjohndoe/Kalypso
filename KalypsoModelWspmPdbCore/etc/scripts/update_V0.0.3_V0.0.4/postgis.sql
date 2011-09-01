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