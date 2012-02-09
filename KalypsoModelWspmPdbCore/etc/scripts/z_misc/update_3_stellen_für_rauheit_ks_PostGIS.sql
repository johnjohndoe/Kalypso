-- FRAGE: alle Rauheitswerte mit 0.0 löschen, damit ab 0.04 die klasse benutzt wird?

ALTER TABLE pdb.roughness
   ALTER COLUMN k_value TYPE numeric(10,3);
COMMENT ON COLUMN pdb.roughness.k_value IS '[m]'

ALTER TABLE pdb.point
   ALTER COLUMN roughness_k_value TYPE numeric(10,3);
COMMENT ON COLUMN pdb.point.roughness.k_value IS '[m]'

DELETE FROM pdb.roughness WHERE TRUE;

-- 08_prefill_table__pdb.roughness
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
