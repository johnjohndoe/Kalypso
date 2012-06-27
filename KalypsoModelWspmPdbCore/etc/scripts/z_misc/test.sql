-- select name, cross_section_part, roughness from point;
--  DROP TABLE bcp_Point cascade constraints;
--  DROP TABLE bcp_Roughness cascade constraints;
--  DROP TABLE bcp_Vegetation cascade constraints;
--  update roughness set label ='hallo', description = 'unknown', point_kind= 'GAF'  where name ='-1';

select cross_section_id, water_body_id, state_id from document;

--delete from document;