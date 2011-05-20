/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 * 
 *  and
 *  
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 * 
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 * 
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 * 
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 * 
 *  Contact:
 * 
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *   
 *  ---------------------------------------------------------------------------*/
package org.kalypso.model.wspm.pdb.connect.internal.postgis;

import org.hibernate.cfg.Configuration;
import org.hibernatespatial.postgis.PostgisDialect;
import org.kalypso.model.wspm.pdb.connect.internal.HibernateConnection;

/**
 * @author Gernot Belger
 */
public class PostGisConnection extends HibernateConnection<PostgisSettings>
{
  public PostGisConnection( final PostgisSettings settings )
  {
    super( settings );
  }

  @Override
  protected void doConfiguration( final Configuration configuration )
  {
    final PostgisSettings settings = getSettings();

    final org.hibernate.dialect.PostgreSQLDialect dialect = new PostgisDialect();

    configuration.setProperty( "hibernate.connection.driver_class", org.postgresql.Driver.class.getName() );

    final String connectionUrl = String.format( "jdbc:postgresql://%s:%d/%s", settings.getHost(), settings.getPort(), settings.getDbName() );

    configuration.setProperty( "hibernate.connection.url", connectionUrl );
    configuration.setProperty( "hibernate.connection.username", settings.getUsername() );
    configuration.setProperty( "hibernate.connection.password", settings.getPassword() );

    configuration.setProperty( "hibernate.dialect", dialect.getClass().getName() );
    configuration.setProperty( "hibernate.spatial.dialect", dialect.getClass().getName() );
  }

  // ///////////////////
  // TODO: create DB //
  // /////////////////
// configuration.generateDropSchemaScript( new org.hibernate.dialect.PostgreSQLDialect() );
// configuration.generateSchemaUpdateScript( new org.hibernate.dialect.PostgreSQLDialect(), null );
  // FIXME: should not be necessary, but it is...
// final String[] creationScripts = configuration.generateSchemaCreationScript( dialect );
// for( final String creationScript : creationScripts )
// {
// final SQLQuery sqlQuery = session.createSQLQuery( creationScript );
// sqlQuery.executeUpdate();
// }
  // FIXME: this does not work: but it DOES work if already one point is there and the tables are present
  // maybe we cannot execute that in one single transaction?
// final String pdbPointGeomSql = createInsertGeomColumn( "pdbpoint", "point", "POINT", 2, 31467 );
// final SQLQuery pdPointGeomQuery = session.createSQLQuery( pdbPointGeomSql );
// pdPointGeomQuery.executeUpdate();

// FIXME: postgis specific
// private String createInsertGeomColumn( final String table, final String column, final String geometryType, final int
// coordDim, final int srsId )
// {
// final StringWriter sw = new StringWriter();
// final PrintWriter pw = new PrintWriter( sw );
//
// pw.println(
// "INSERT INTO geometry_columns(f_table_catalog, f_table_schema, f_table_name, f_geometry_column, coord_dimension, srid, type) "
// );
// pw.format( "SELECT '', 'public', '%s', '%s', %d, %d, '%s'%n", table, column, coordDim, srsId, geometryType );
// pw.format( "FROM public.pdbpoint LIMIT 1;" );
//
// pw.flush();
// sw.flush();
// return sw.toString();
// }
}