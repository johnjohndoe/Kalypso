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
package org.kalypso.model.wspm.pdb.internal.connect.oracle;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.databinding.DataBindingContext;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.widgets.Composite;
import org.kalypso.model.wspm.pdb.connect.IPdbConnection;
import org.kalypso.model.wspm.pdb.connect.IPdbSettings;
import org.kalypso.model.wspm.pdb.connect.IPdbSettingsControl;
import org.kalypso.model.wspm.pdb.internal.WspmPdbCoreImages;
import org.kalypso.model.wspm.pdb.internal.connect.HibernateSettings;

/**
 * @author Gernot Belger
 */
public class OracleSettings extends HibernateSettings
{
  public static final String TYPE = "oracle"; //$NON-NLS-1$

  private static final String DEFAULT_HOST = "localhost"; //$NON-NLS-1$

  static final int DEFAULT_PORT = 1521;

  static final String PROPERTY_DBNAME = "dbname"; //$NON-NLS-1$

  static final String PROPERTY_HOST = "host";//$NON-NLS-1$

  static final String PROPERTY_PORT = "port";//$NON-NLS-1$

  public OracleSettings( )
  {
    super( "Oracle" ); //$NON-NLS-1$
  }

  public OracleSettings( final String name, final OracleSettings settings )
  {
    super( name, settings );
  }

  @Override
  public String getType( )
  {
    return TYPE;
  }

  @Override
  public ImageDescriptor getImage( )
  {
    return WspmPdbCoreImages.IMAGE_ORACLE_32x32;
  }

  @Override
  public IPdbConnection createConnection( )
  {
    return new OracleConnection( this );
  }

  @Override
  protected String getDefaultValue( final String property )
  {
    if( PROPERTY_HOST.equals( property ) )
      return DEFAULT_HOST;

    if( PROPERTY_PORT.equals( property ) )
      return Integer.toString( DEFAULT_PORT );

    if( PROPERTY_DBNAME.equals( property ) )
      return DEFAULT_DBNAME;

    if( PROPERTY_USERNAME.equals( property ) )
      return System.getProperty( "user.name" ); //$NON-NLS-1$

    if( PROPERTY_PASSWORD.equals( property ) )
      return StringUtils.EMPTY;

    if( PROPERTY_NAME.equals( property ) )
      return "Oracle"; //$NON-NLS-1$

    throw new IllegalArgumentException( String.format( "Unknwon property: %s", property ) ); //$NON-NLS-1$
  }

  public void setHost( final String host )
  {
    setProperty( PROPERTY_HOST, host );
  }

  String getHost( )
  {
    return getProperty( PROPERTY_HOST );
  }

  public void setPort( final Integer port )
  {
    if( port == null )
      setProperty( PROPERTY_PORT, null );
    else
      setProperty( PROPERTY_PORT, Integer.toString( port ) );
  }

  int getPort( )
  {
    try
    {
      final String property = getProperty( PROPERTY_PORT );
      return Integer.valueOf( property );
    }
    catch( final NumberFormatException e )
    {
      e.printStackTrace();
      return DEFAULT_PORT;
    }
  }

  public void setDbName( final String dbName )
  {
    setProperty( PROPERTY_DBNAME, dbName );
  }

  String getDbName( )
  {
    return getProperty( PROPERTY_DBNAME );
  }

  public void setUsername( final String username )
  {
    setProperty( PROPERTY_USERNAME, username );
  }

  @Override
  public String getUsername( )
  {
    return getProperty( PROPERTY_USERNAME );
  }

  public void setPassword( final String password )
  {
    setProperty( PROPERTY_PASSWORD, password );
  }

  String getPassword( )
  {
    return getProperty( PROPERTY_PASSWORD );
  }

  @Override
  public String toString( )
  {
    return String.format( "%s@%s:%s/%s", getUsername(), getHost(), getPort(), getDbName() ); //$NON-NLS-1$
  }

  @Override
  public IPdbSettings copy( )
  {
    return new OracleSettings( getName(), this );
  }

  @Override
  public IPdbSettingsControl createEditControl( final DataBindingContext binding, final Composite parent )
  {
    return new OracleSettingsControl( binding, parent, this );
  }
}