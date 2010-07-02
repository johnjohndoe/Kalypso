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

import org.apache.commons.lang.StringUtils;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.wspwin.core.prf.datablock.DataBlockHeader;

/**
 * Maps component id's to prf-headers.
 * 
 * @author Gernot Belger
 */
public final class LengthSectionMapping
{
  private static LengthSectionMapping INSTANCE;

  public final static String blanc200 = StringUtils.repeat( " ", 200 );

  private final Map<String, String[]> m_propertyMap = new HashMap<String, String[]>();

  private LengthSectionMapping( )
  {
    m_propertyMap.put( IWspmConstants.LENGTH_SECTION_PROPERTY_STATION, new String[] { "STATION" } ); //$NON-NLS-1$
    m_propertyMap.put( IWspmConstants.LENGTH_SECTION_PROPERTY_GROUND, new String[] { "SOHLHOEHE" } ); //$NON-NLS-1$
    m_propertyMap.put( IWspmConstants.LENGTH_SECTION_PROPERTY_BOE_LI, new String[] { "BOESCHUNG-LI" } ); //$NON-NLS-1$
    m_propertyMap.put( IWspmConstants.LENGTH_SECTION_PROPERT_BOE_RE, new String[] { "BOESCHUNG-RE" } ); //$NON-NLS-1$
    m_propertyMap.put( IWspmConstants.LENGTH_SECTION_PROPERTY_WEIR_OK, new String[] { "OK-WEHRS" } ); //$NON-NLS-1$
    m_propertyMap.put( IWspmConstants.LENGTH_SECTION_PROPERTY_BRIDGE_OK, new String[] { "DECKENOBERK" } ); //$NON-NLS-1$
    m_propertyMap.put( IWspmConstants.LENGTH_SECTION_PROPERTY_BRIDGE_UK, new String[] { "DECKENUNTERK" } ); //$NON-NLS-1$
    // m_PropertyMap.put( IWspmConstants.LENGTH_SECTION_PROPERTY_BRIDGE_WIDTH, "BRIDGE_WIDTH" );
    // m_PropertyMap.put( IWspmConstants.LENGTH_SECTION_PROPERTY_ROHR_DN, "ROHR_DN" );
    m_propertyMap.put( IWspmConstants.POINT_PROPERTY_COMMENT, new String[] { "TEXT", "", " 0  0  0  0  0  0  0  0 12" } ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    m_propertyMap.put( IWspmConstants.LENGTH_SECTION_PROPERTY_TEXT, new String[] { "TEXT", "", " 0  0  0  0  0  0  0  0 12" } ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
    m_propertyMap.put( "TEXT", new String[] { "TEXT", "", " 0  0  0  0  0  0  0  0 12" } ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
    m_propertyMap.put( IWspmConstants.LENGTH_SECTION_PROPERTY_WATERLEVEL, new String[] { "Wasserspiegel NN+m" } );//$NON-NLS-1$
  }

  public synchronized static DataBlockHeader createHeader( final String id )
  {
    if( INSTANCE == null )
      INSTANCE = new LengthSectionMapping();

    return INSTANCE.createH( id );
  }

  private DataBlockHeader createH( final String id )
  {
    final DataBlockHeader dbh = new DataBlockHeader();
    final String[] fl = m_propertyMap.get( id );
    if( fl == null )
      return null;

    dbh.setFirstLine( fl.length > 0 ? fl[0] : id );
    dbh.setSecondLine( fl.length > 1 ? blanc200 + fl[1] + "@" : "" );//$NON-NLS-1$ //$NON-NLS-2$
    dbh.setThirdLine( fl.length > 2 ? fl[2] : " 0  0  0  0  0  0  0  0  0" );//$NON-NLS-1$
    return dbh;
  }

  /**
   * Returns <code>true</code>, if the given header is a text datablock.
   */
  public static boolean isText( final DataBlockHeader dbh )
  {
    return dbh.getSpecification( 8 ) == 12;
  }
}
