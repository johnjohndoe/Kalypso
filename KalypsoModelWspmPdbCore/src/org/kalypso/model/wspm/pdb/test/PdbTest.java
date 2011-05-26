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
package org.kalypso.model.wspm.pdb.test;

import junit.framework.Assert;

import org.hibernate.Session;
import org.junit.Test;
import org.kalypso.model.wspm.pdb.connect.Executor;
import org.kalypso.model.wspm.pdb.connect.IPdbConnection;
import org.kalypso.model.wspm.pdb.connect.PdbConnectException;
import org.kalypso.model.wspm.pdb.connect.internal.AddObjectOperation;
import org.kalypso.model.wspm.pdb.connect.internal.oracle.OracleSettings;
import org.kalypso.model.wspm.pdb.db.PdbInfo;
import org.kalypso.model.wspm.pdb.db.mapping.WaterBody;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineString;

/**
 * @author Gernot Belger
 */
public class PdbTest extends Assert
{
  @Test
  public void testPdb( ) throws PdbConnectException
  {
    final OracleSettings connectInfo = new OracleSettings();
    connectInfo.setHost( "lfulg-kv-02" );
    connectInfo.setDbName( "XE" );
    connectInfo.setUsername( "pdb_admin" );
    connectInfo.setPassword( "pdb_admin" );
// final PostgisSettings connectInfo = new PostgisSettings();
// connectInfo.setHost( "map.bjoernsen.de" );
// connectInfo.setDbName( "moni" );
// connectInfo.setUsername( "pdb_admin" );
// connectInfo.setPassword( "pdb_admin" );

    final IPdbConnection connection = connectInfo.createConnection();
    connection.connect();

    final PdbInfo info = connection.getInfo();
    final String version = info.getVersion();
    System.out.println( "Version=" + version );

// final Point onePoint = new Point();
// // FIXME
// onePoint.setId( (int) System.currentTimeMillis() );
// onePoint.setLocation( new GeometryFactory().createPoint( new Coordinate( 3.14, 2.79 ) ) );
// onePoint.setName( "N1" );
// onePoint.setLabel( "PNAM1" );

    final WaterBody waterBody = new WaterBody();
    waterBody.setDescription( "Comment" );
    waterBody.setId( 1 );
    waterBody.setLabel( "GKN" );
    waterBody.setName( "sososo" );

    final LineString line = new GeometryFactory().createLineString( new Coordinate[] { new Coordinate( 3.14, 2.79 ), new Coordinate( 3.14, 2.79 ) } );
    waterBody.setRiverline( line );

// final State state = new State();
// final Date now = new Date();
// state.setCreationDate( now );
// state.setDescription( "kommentar" );
// state.setEditingDate( now );
// state.setEditingUser( "Moni" );
// state.setId( 815 );
// state.setIsstatezero( 'T' );
// state.setMeasurementDate( now );
// state.setName( "DER EINE" );
// state.setSource( "nix" );

    final Session session = connection.openSession();
    final AddObjectOperation operation = new AddObjectOperation( waterBody );
    new Executor( session, operation ).execute();
    session.close();
  }

// // @Test
// public void testPdb2( )
// {
// final Configuration configuration = new Configuration();
//
// configuration.setProperty( "hibernate.order_updates", "true" );
//
// // FIXME: why does this not work???
// // configuration.setProperty( "hibernate.hbm2dll.auto", "create" );
// // configuration.setProperty( "org.hibernate.tool.hbm2ddl", "debug" );
//
// configuration.setProperty( "connection.pool_size", "1" );
// configuration.setProperty( "current_session_context_class", "thread" );
// configuration.setProperty( "cache.provider_class", "org.hibernate.cache.NoCacheProvider" );
//
// // TODO: via tracing
// configuration.setProperty( "show_sql", "true" );
//
// final org.hibernate.dialect.PostgreSQLDialect dialect = new PostgisDialect();
//
// configuration.setProperty( "hibernate.connection.driver_class", org.postgresql.Driver.class.getName() );
//
// final String connectionUrl = "jdbc:postgresql://map.bjoernsen.de:5432/pdb";
//
// configuration.setProperty( "hibernate.connection.url", connectionUrl );
// configuration.setProperty( "hibernate.connection.username", "pdb_admin" );
// configuration.setProperty( "hibernate.connection.password", "pdb_admin" );
//
// configuration.setProperty( "hibernate.dialect", dialect.getClass().getName() );
// configuration.setProperty( "hibernate.spatial.dialect", dialect.getClass().getName() );
//
// final ClassLoader classLoader = getClass().getClassLoader();
// Thread.currentThread().setContextClassLoader( classLoader );
//
// configuration.addAnnotatedClass( Info.class );
// // FIXME
// // configuration.addAnnotatedClass( PdbPoint.class );
// configuration.addResource( "/org/kalypso/model/wspm/pdb/db/pdbpoint.xml", classLoader );
//
//
// final String[] creationScripts = configuration.generateSchemaCreationScript( dialect );
// for( final String creationScript : creationScripts )
// System.out.println( creationScript );
//
// final SessionFactory sessionFactory = configuration.buildSessionFactory();
//
// final Session session = sessionFactory.openSession();
//
// final Transaction transaction = session.beginTransaction();
// final List<Info> allInfo = session.createQuery( String.format( "from %s", Info.class.getName() ) ).list();
// transaction.commit();
//
// final PdbInfo info = new PdbInfo( allInfo );
//
// final String version = info.getVersion();
// System.out.println( "Version=" + version );
//
// final Transaction transaction2 = session.beginTransaction();
//
// final Point onePoint = new Point();
// onePoint.setPoint( "" + System.currentTimeMillis() );
// onePoint.setLocation( new GeometryFactory().createPoint( new Coordinate( 3.14, 2.79 ) ) );
// session.save( onePoint );
// transaction2.commit();
//
// session.flush();
// session.close();
// }

}