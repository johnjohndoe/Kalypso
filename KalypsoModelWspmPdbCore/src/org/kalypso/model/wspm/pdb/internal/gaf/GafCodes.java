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
package org.kalypso.model.wspm.pdb.internal.gaf;

import java.io.IOException;
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Properties;

import org.apache.commons.lang3.StringUtils;
import org.kalypso.contribs.java.util.PropertiesUtilities;
import org.kalypso.model.wspm.pdb.gaf.GafCode;
import org.kalypso.model.wspm.pdb.internal.i18n.Messages;

/**
 * Represents the 'Kennziffer' (KZ) of a GAF file.
 * 
 * @author Gernot Belger
 */
public class GafCodes
{
  public static final GafCode NULL_HYK = new GafCode( -1, Messages.getString("GafCodes.0"), StringUtils.EMPTY, StringUtils.EMPTY, StringUtils.EMPTY, StringUtils.EMPTY ); //$NON-NLS-1$

  private final Map<String, GafCode> m_codes = new LinkedHashMap<String, GafCode>();

  private final Map<String, GafCode> m_hykCodes = new LinkedHashMap<String, GafCode>();

  public GafCodes( ) throws IOException
  {
    final Properties properties = PropertiesUtilities.load( getClass().getResource( "kz.properties" ) ); //$NON-NLS-1$
    for( final String key : properties.stringPropertyNames() )
    {
      final String value = properties.getProperty( key );

      final GafCode gafCode = new GafCode( key, value );

      m_codes.put( gafCode.getCode(), gafCode );
      m_hykCodes.put( gafCode.getHyk(), gafCode );

      // m_codes.put( StringUtils.EMPTY, NULL_CODE );
      m_hykCodes.put( StringUtils.EMPTY, NULL_HYK );
    }
  }

  public GafCode getCode( final String kz )
  {
    return m_codes.get( kz );
  }

  public GafCode getHykCode( final String hyk )
  {
    return m_hykCodes.get( hyk );
  }

  public GafCode[] getAllCodes( )
  {
    final Collection<GafCode> values = m_codes.values();
    return values.toArray( new GafCode[values.size()] );
  }

  public GafCode[] getAllHyks( )
  {
    final Collection<GafCode> values = m_hykCodes.values();
    return values.toArray( new GafCode[values.size()] );
  }
}