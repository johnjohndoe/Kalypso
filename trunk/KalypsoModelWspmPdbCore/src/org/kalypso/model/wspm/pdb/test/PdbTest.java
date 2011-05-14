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

import org.junit.Test;
import org.kalypso.model.wspm.pdb.connect.IPdbConnection;
import org.kalypso.model.wspm.pdb.connect.PdbConnectException;
import org.kalypso.model.wspm.pdb.connect.internal.postgis.PostgisConnectInfo;
import org.kalypso.model.wspm.pdb.db.PdbInfo;
import org.kalypso.model.wspm.pdb.db.PdbPoint;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.Point;

/**
 * @author Gernot Belger
 */
public class PdbTest extends Assert
{
  @Test
  public void testPdb( ) throws PdbConnectException
  {
    final PostgisConnectInfo connectInfo = new PostgisConnectInfo();
    connectInfo.setHost( "map.bjoernsen.de" );
    connectInfo.setDbName( "pdb" );
    connectInfo.setUsername( "pdb_admin" );
    connectInfo.setPassword( "pdb_admin" );

    final IPdbConnection connection = connectInfo.createConnection();
    connection.connect();

    final PdbInfo info = connection.getInfo();
    final String version = info.getVersion();

    final Point aPoint = new GeometryFactory().createPoint( new Coordinate( 3.14, 2.79 ) );

    final PdbPoint onePoint = new PdbPoint();
    onePoint.setPoint( new GeometryFactory().createPoint( new Coordinate( 3.14, 2.79 ) ) );
    connection.addPoint( onePoint );

    try
    {

// final PdbProperty version = new PdbProperty();
// version.setKey( "Version" );
// version.setValue( "0.0.1" );
// session.save( version );
//
    }
    catch( final Throwable t )
    {
      t.printStackTrace();
    }
    finally
    {
    }
  }

}