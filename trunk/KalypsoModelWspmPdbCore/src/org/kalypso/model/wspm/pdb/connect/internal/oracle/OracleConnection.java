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
package org.kalypso.model.wspm.pdb.connect.internal.oracle;

import org.hibernate.cfg.Configuration;
import org.hibernate.cfg.Environment;
import org.hibernatespatial.SpatialDialect;
import org.hibernatespatial.oracle.OracleSpatial10gDialect;
import org.kalypso.model.wspm.pdb.connect.internal.HibernateConnection;

/**
 * @author Gernot Belger
 */
public class OracleConnection extends HibernateConnection<OracleSettings>
{
  public OracleConnection( final OracleSettings settings )
  {
    super( settings );
  }

  @Override
  public SpatialDialect createSpatialDialect( )
  {
    return new OracleSpatial10gDialect();
  }

  @Override
  protected void doConfiguration( final Configuration configuration )
  {
    final OracleSettings settings = getSettings();

    configuration.setProperty( Environment.DRIVER, oracle.jdbc.OracleDriver.class.getName() );

    final String connectionUrl = String.format( "jdbc:oracle:thin:@%s:%d:%s", settings.getHost(), settings.getPort(), settings.getDbName() );

    configuration.setProperty( Environment.URL, connectionUrl );
    configuration.setProperty( Environment.USER, settings.getUsername() );
    configuration.setProperty( Environment.PASS, settings.getPassword() );

// final OracleConnectInfo connectInfo = getConnectInfo();
//
// final org.hibernate.dialect.PostgreSQLDialect dialect = new PostgisDialect();
//
// configuration.setProperty( "hibernate.connection.driver_class", org.postgresql.Driver.class.getName() );
//
// final String connectionUrl = String.format( "jdbc:postgresql://%s:%d/%s", connectInfo.getHost(),
// connectInfo.getPort(), connectInfo.getDbName() );
//
// configuration.setProperty( "hibernate.connection.url", connectionUrl );
// configuration.setProperty( "hibernate.connection.username", connectInfo.getUsername() );
// configuration.setProperty( "hibernate.connection.password", connectInfo.getPassword() );
//
// configuration.setProperty( "hibernate.dialect", dialect.getClass().getName() );
// configuration.setProperty( "hibernate.spatial.dialect", dialect.getClass().getName() );
  }

  // ///////////////////
  // TODO: create DB //
  // /////////////////
}