/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
 
 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.ogc.sensor.filter.filters.valuecomp;

import org.kalypso.commons.parser.ParserException;
import org.kalypso.ogc.sensor.IAxis;

/**
 * CompSmaller
 * 
 * @author schlienger
 */
public class CompSmaller extends AbstractValueComp
{
  private final Object m_value;

  private boolean m_modeInclusive;

  /**
   * @param axes
   * @param axisType
   * @param value
   * @param modeInclusive
   * @throws ParserException
   */
  public CompSmaller( final IAxis[] axes, final String axisType, final String value, final boolean modeInclusive )
      throws ParserException
  {
    super( axes, axisType );

    m_value = m_parser.parse( value );
    m_modeInclusive = modeInclusive;
  }

  /**
   * @see org.kalypso.ogc.sensor.filter.filters.valuecomp.AbstractValueComp#internalValidates(java.lang.Object)
   */
  @Override
  public boolean internalValidates( final Object element ) throws ParserException
  {
    if( m_modeInclusive )
      return m_parser.compare( element, m_value ) <= 0;

    return m_parser.compare( element, m_value ) < 0;
  }
}
