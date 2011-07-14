/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
package org.kalypso.model.wspm.pdb.db;

import java.util.List;
import java.util.Properties;

import org.apache.commons.lang.StringUtils;
import org.hibernate.Session;
import org.kalypso.contribs.java.lang.NumberUtils;
import org.kalypso.model.wspm.pdb.connect.PdbConnectException;
import org.kalypso.model.wspm.pdb.connect.command.GetPdbList;
import org.kalypso.model.wspm.pdb.db.mapping.Info;

/**
 * @author Gernot Belger
 */
public class PdbInfo
{
  public static final int UNKNOWN_SRID = -1;

  private static final Object CURRENT_VERSION = "0.0.2"; //$NON-NLS-1$

  private final String PROPERTY_VERSION = "Version"; //$NON-NLS-1$

  private final String PROPERTY_SRID = "SRID"; //$NON-NLS-1$

  private final String PROPERTY_DOCUMENT_SERVER = "DocumentServer"; //$NON-NLS-1$

  private final Properties m_properties = new Properties();

  public PdbInfo( final Session session ) throws PdbConnectException
  {
    final List<Info> list = GetPdbList.getList( session, Info.class );
    for( final Info property : list )
    {
      final String value = property.getValue();
      m_properties.put( property.getKey(), StringUtils.defaultString( value ) );
    }
  }

  public String getVersion( )
  {
    return m_properties.getProperty( PROPERTY_VERSION );
  }

  public int getSRID( )
  {
    final String property = m_properties.getProperty( PROPERTY_SRID, Integer.toString( UNKNOWN_SRID ) );
    return NumberUtils.parseQuietInt( property, UNKNOWN_SRID );
  }

  /**
   * @return base part of document server's URL; Will be used to resolve document URLs.
   */
  public String getDocumentServer( )
  {
    return m_properties.getProperty( PROPERTY_DOCUMENT_SERVER );
  }

  public void validate( ) throws PdbConnectException
  {
    final String version = getVersion();
    if( !CURRENT_VERSION.equals( version ) )
    {
      final String message = String.format( "Unknown Version of PDB: %s (should be %s)", version, CURRENT_VERSION );
      throw new PdbConnectException( message );
    }

    final int srid = getSRID();
    if( UNKNOWN_SRID == srid )
    {
      final String message = String.format( "Failed to determine SRID: %s", m_properties.get( PROPERTY_SRID ) );
      throw new PdbConnectException( message );
    }
  }
}