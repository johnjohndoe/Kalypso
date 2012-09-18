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
package org.kalypso.model.wspm.tuhh.core.wspwin.prf;

import java.util.HashMap;
import java.util.Map;

import org.apache.commons.lang3.StringUtils;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.wspwin.core.prf.IWspWinConstants;
import org.kalypso.wspwin.core.prf.datablock.DataBlockDescription;
import org.kalypso.wspwin.core.prf.datablock.DataBlockHeader;

/**
 * Maps component id's to prf-headers.
 *
 * @author Gernot Belger
 */
public final class LengthSectionMapping
{
  private static LengthSectionMapping INSTANCE;

  public static final String BLANC200 = StringUtils.repeat( " ", 200 ); //$NON-NLS-1$

  private final Map<String, DataBlockDescription> m_propertyMap = new HashMap<>();

  private LengthSectionMapping( )
  {
    m_propertyMap.put( IWspmConstants.LENGTH_SECTION_PROPERTY_STATION, DataBlockDescription.STATION );
    m_propertyMap.put( IWspmConstants.LENGTH_SECTION_PROPERTY_GROUND, DataBlockDescription.SOHLHOEHE );
    m_propertyMap.put( IWspmConstants.LENGTH_SECTION_PROPERTY_BOE_LI, DataBlockDescription.BOESCHUNG_LI );
    m_propertyMap.put( IWspmConstants.LENGTH_SECTION_PROPERTY_BOE_RE, DataBlockDescription.BOESCHUNG_RE );
    m_propertyMap.put( IWspmConstants.LENGTH_SECTION_PROPERTY_WEIR_OK, DataBlockDescription.OK_WEHRS );
    m_propertyMap.put( IWspmConstants.LENGTH_SECTION_PROPERTY_BRIDGE_OK, DataBlockDescription.DECKENOBERK );
    m_propertyMap.put( IWspmConstants.LENGTH_SECTION_PROPERTY_BRIDGE_UK, DataBlockDescription.DECKENUNTERK );
    m_propertyMap.put( IWspmConstants.POINT_PROPERTY_COMMENT, DataBlockDescription.TEXT );
    m_propertyMap.put( IWspmConstants.LENGTH_SECTION_PROPERTY_TEXT, DataBlockDescription.TEXT );
    m_propertyMap.put( "TEXT", DataBlockDescription.TEXT ); //$NON-NLS-1$
    m_propertyMap.put( IWspmConstants.LENGTH_SECTION_PROPERTY_WATERLEVEL, DataBlockDescription.WASSERSPIEGEL );
  }

  public static synchronized DataBlockHeader createHeader( final String id )
  {
    if( INSTANCE == null )
    {
      INSTANCE = new LengthSectionMapping();
    }

    return INSTANCE.createH( id );
  }

  private DataBlockHeader createH( final String id )
  {
    final DataBlockDescription fl = m_propertyMap.get( id );
    if( fl == null )
      return null;

    return new DataBlockHeader( fl.getName(), fl.getSpecialDataBlockId() );
  }

  /**
   * Returns <code>true</code>, if the given header is a text datablock.
   */
  public static boolean isText( final DataBlockHeader dbh )
  {
    return dbh.getSpecification( 8 ) == IWspWinConstants.SPEZIALPROFIL_TEXT;
  }
}
