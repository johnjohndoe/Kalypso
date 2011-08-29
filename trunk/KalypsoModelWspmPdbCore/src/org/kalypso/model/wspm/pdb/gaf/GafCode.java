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
package org.kalypso.model.wspm.pdb.gaf;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.swt.graphics.RGB;
import org.kalypso.contribs.eclipse.swt.ColorUtilities;

/**
 * Represents the 'Kennziffer' (KZ) of a gaf file.
 * 
 * @author Gernot Belger
 */
public class GafCode implements Comparable<GafCode>
{
  private final String m_key;

  private final String m_dbCode;

  private final String m_description;

  private final String m_hyk;

  private final String m_kind;

  private final int m_number;

  private RGB m_color;

  public GafCode( final String key, final String value )
  {
    m_key = StringUtils.trim( key );

    final String[] tokens = value.split( ";", 6 ); //$NON-NLS-1$
    m_number = Integer.parseInt( tokens[0] );
    m_dbCode = StringUtils.trim( tokens[1] );
    m_description = StringUtils.trim( tokens[2] );
    m_hyk = StringUtils.trim( tokens[3] );
    m_kind = StringUtils.trim( tokens[4] );
    m_color = ColorUtilities.toRGBFromHTML( StringUtils.trim( tokens[5] ) );
  }

  public GafCode( final int number, final String key, final String dbCode, final String description, final String hyk, final String kind )
  {
    m_number = number;
    m_key = key;
    m_dbCode = dbCode;
    m_description = description;
    m_hyk = hyk;
    m_kind = kind;
  }

  @Override
  public String toString( )
  {
    if( StringUtils.isBlank( m_description ) )
      return m_key;

    return String.format( "%-4s - %s", m_key, m_description ); //$NON-NLS-1$
  }

  public String getDbCode( )
  {
    return m_dbCode;
  }

  public String getDescription( )
  {
    return m_description;
  }

  public String getHyk( )
  {
    return m_hyk;
  }

  public String getCode( )
  {
    return m_key;
  }

  public RGB getColor( )
  {
    return m_color;
  }

  /**
   * Classifies the point in different kinds of points; each kind representing one cross section part.
   */
  public String getKind( )
  {
    return m_kind;
  }

  @Override
  public int compareTo( final GafCode o )
  {
    return m_number - o.m_number;
  }
}