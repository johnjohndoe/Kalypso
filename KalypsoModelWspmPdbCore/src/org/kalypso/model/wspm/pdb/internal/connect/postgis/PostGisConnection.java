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
package org.kalypso.model.wspm.pdb.internal.connect.postgis;

import java.util.List;

import org.hibernate.HibernateException;
import org.hibernate.SQLQuery;
import org.hibernate.Session;
import org.hibernate.cfg.Configuration;
import org.hibernate.cfg.Environment;
import org.hibernatespatial.SpatialDialect;
import org.hibernatespatial.postgis.PostgisDialect;
import org.kalypso.model.wspm.pdb.connect.PDBRole;
import org.kalypso.model.wspm.pdb.internal.connect.HibernateConnection;

import com.vividsolutions.jts.geom.Envelope;

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
  protected SpatialDialect createSpatialDialect( )
  {
    return new PostgisDialect();
  }

  @Override
  protected void doConfiguration( final Configuration configuration )
  {
    final PostgisSettings settings = getSettings();

    configuration.setProperty( Environment.DRIVER, org.postgresql.Driver.class.getName() );

    final String connectionUrl = String.format( "jdbc:postgresql://%s:%d/%s", settings.getHost(), settings.getPort(), settings.getDbName() ); //$NON-NLS-1$

    configuration.setProperty( Environment.URL, connectionUrl );
    configuration.setProperty( Environment.USER, settings.getUsername() );
    configuration.setProperty( Environment.PASS, settings.getPassword() );

    // In order to use ssl, we need a way to accept the certificate
    // configuration.setProperty( Environment.CONNECTION_PREFIX + ".ssl", Boolean.TRUE.toString() );
  }

  @Override
  public Envelope getCrsEnvelope( final Integer srid )
  {
    // FIXME: implement SQL query
    final Envelope env = new Envelope( 4300000, 4600000, 5500000, 5800000 );

    return env;
  }

  @Override
  protected PDBRole readRole( final Session session )
  {
    final String username = getSettings().getUsername();
    if( SUPERUSER.compareToIgnoreCase( username ) == 0 )
      return PDBRole.superuser;

    try
    {
      final String statement = String.format( "select count(*) from information_schema.applicable_roles where upper(role_name)='%s'", PDBRole.fadmin.getName() ); //$NON-NLS-1$

      final SQLQuery query = session.createSQLQuery( statement );
      final List< ? > result = query.list();
      if( result.size() != 1 )
        return PDBRole.user;

      final Object object = result.get( 0 );
      if( object instanceof Number )
      {
        final int count = ((Number) object).intValue();
        if( count > 0 )
          return PDBRole.fadmin;
      }
    }
    catch( final HibernateException e )
    {
      e.printStackTrace();
    }

    return PDBRole.user;
  }

}