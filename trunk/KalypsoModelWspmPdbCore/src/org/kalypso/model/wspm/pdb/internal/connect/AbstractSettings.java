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
package org.kalypso.model.wspm.pdb.internal.connect;

import java.util.Properties;
import java.util.Set;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.equinox.security.storage.ISecurePreferences;
import org.eclipse.equinox.security.storage.StorageException;
import org.kalypso.model.wspm.pdb.connect.IPdbSettings;

/**
 * @author Gernot Belger
 */
public abstract class AbstractSettings implements IPdbSettings
{
  protected static final String PROPERTY_NAME = "name";//$NON-NLS-1$

  public static final String PROPERTY_PASSWORD = "password";//$NON-NLS-1$

  private final Properties m_properties = new Properties();

  public AbstractSettings( final String name )
  {
    setName( name );
  }

  public AbstractSettings( final String name, final AbstractSettings settings )
  {
    setName( name );
    m_properties.putAll( settings.m_properties );
  }

  public final void setName( final String name )
  {
    m_properties.put( PROPERTY_NAME, name );
  }

  @Override
  public final String getName( )
  {
    return getProperty( PROPERTY_NAME );
  }

  @Override
  public int compareTo( final IPdbSettings o )
  {
    final String n1 = getName();
    final String n2 = o.getName();

    return n1.compareToIgnoreCase( n2 );
  }

  protected String getProperty( final String name, final String defaultValue )
  {
    final String property = m_properties.getProperty( name, defaultValue );
    if( StringUtils.isBlank( property ) )
      return defaultValue;

    return property;
  }

  protected String getProperty( final String name )
  {
    return getProperty( name, getDefaultValue( name ) );
  }

  protected void setProperty( final String name, final String value )
  {
    m_properties.setProperty( name, value );
  }

  protected abstract String getDefaultValue( final String property );

  @Override
  public void saveState( final ISecurePreferences preferences ) throws StorageException
  {
    final Set<String> names = m_properties.stringPropertyNames();
    for( final String name : names )
    {
      final boolean encrypt = PROPERTY_PASSWORD.equals( name );
      final String value = m_properties.getProperty( name );
      preferences.put( name, value, encrypt );
    }
  }

  @Override
  public void readState( final ISecurePreferences preferences ) throws StorageException
  {
    final String[] keys = preferences.keys();
    for( final String key : keys )
    {
      final String value = preferences.get( key, null );
      m_properties.setProperty( key, value );
    }
  }

}