CREATE DATABASE pdb
  WITH OWNER = pdb_admin
       TEMPLATE = template_postgis
       ENCODING = 'UTF8'
       TABLESPACE = pg_default
       CONNECTION LIMIT = -1;
