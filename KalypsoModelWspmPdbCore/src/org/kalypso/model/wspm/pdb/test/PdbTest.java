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
import org.kalypso.model.wspm.pdb.db.PdbInfo;
import org.kalypso.model.wspm.pdb.db.mapping.WaterBody;
import org.kalypso.model.wspm.pdb.internal.connect.AddObjectOperation;
import org.kalypso.model.wspm.pdb.internal.connect.oracle.OracleSettings;

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
    connectInfo.setHost( "lfulg-kv-02" ); //$NON-NLS-1$
    connectInfo.setDbName( "XE" ); //$NON-NLS-1$
    connectInfo.setUsername( "pdb" ); //$NON-NLS-1$
    connectInfo.setPassword( "pdb" ); //$NON-NLS-1$
// final PostgisSettings connectInfo = new PostgisSettings();
// connectInfo.setHost( "map.bjoernsen.de" );
// connectInfo.setDbName( "moni" );
// connectInfo.setUsername( "pdb" );
// connectInfo.setPassword( "pdb" );

    final IPdbConnection connection = connectInfo.createConnection();
    connection.connect();

    final Session session = connection.openSession();

    final PdbInfo info = new PdbInfo( session );
    System.out.println( "Version=" + info.getVersion() ); //$NON-NLS-1$
    System.out.println( "SRID=" + info.getSRID() ); //$NON-NLS-1$

// final Point onePoint = new Point();
// // FIXME
// onePoint.setId( (int) System.currentTimeMillis() );
// onePoint.setLocation( new GeometryFactory().createPoint( new Coordinate( 3.14, 2.79 ) ) );
// onePoint.setName( "N1" );
// onePoint.setLabel( "PNAM1" );

    final WaterBody waterBody = new WaterBody();
    waterBody.setDescription( "Comment" ); //$NON-NLS-1$
    waterBody.setLabel( "GKN" ); //$NON-NLS-1$
    waterBody.setName( "sososo" ); //$NON-NLS-1$

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

    final AddObjectOperation operation = new AddObjectOperation( waterBody );
    new Executor( session, operation ).execute();
    session.close();
  }
}