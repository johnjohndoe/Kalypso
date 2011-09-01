-- Version setzen auf "creating" oder "updateing"
UPDATE INFO set value='updating 0.0.3 to 0.0.4'  where key ='Version';

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
ALTER TABLE Document DROP CONSTRAINT "Document Name UK" CASCADE;
ALTER TABLE DOCUMENT DROP CONSTRAINT CS_DOCUMENT CASCADE;
ALTER TABLE DOCUMENT ADD
(
  CONSTRAINT CS_DOCUMENT 
  FOREIGN KEY( CROSS_SECTION_ID)
  REFERENCES CROSS_SECTION( ID)
  ON DELETE CASCADE 
  NOT DEFERRABLE
  ENABLE
);


ALTER TABLE DOCUMENT DROP CONSTRAINT STATE_DOCUMENT CASCADE;
ALTER TABLE DOCUMENT ADD
(
  CONSTRAINT STATE_DOCUMENT 
  FOREIGN KEY( STATE_ID)
  REFERENCES STATE( ID)
  ON DELETE CASCADE 
  NOT DEFERRABLE
  ENABLE
);

ALTER TABLE DOCUMENT DROP CONSTRAINT WB_DOCUMENT CASCADE;
ALTER TABLE DOCUMENT ADD
(
  CONSTRAINT WB_DOCUMENT 
  FOREIGN KEY( WATER_BODY_ID)
  REFERENCES WATER_BODY( ID)
  ON DELETE CASCADE 
  NOT DEFERRABLE
  ENABLE
);

ALTER TABLE Point ADD( Roughness_Factor NUMBER (8,5) NULL );
COMMENT ON COLUMN Point.Roughness_Factor IS 'factor to apply to roughness value';

ALTER TABLE Roughness ADD( Color VARCHAR2(10) NULL );
COMMENT ON COLUMN Roughness.Color IS 'color (hex)';

ALTER TABLE Vegetation ADD( Color VARCHAR2(10) NULL );
COMMENT ON COLUMN Vegetation.Color IS 'color (hex)';



-- Version endgültig setzen
UPDATE INFO set value='0.0.4'  where key ='Version';
commit;