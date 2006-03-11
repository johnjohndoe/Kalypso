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
 * CompBetween
 * 
 * @author schlienger
 */
public class CompBetween extends AbstractValueComp
{
  private Object m_valueFrom;

  private Object m_valueTo;

  private boolean m_modeFromIncl;

  private boolean m_modeToIncl;

  /**
   * @param axes
   * @param axisType
   * @param valueFrom
   * @param modeFromInclusive
   * @param valueTo
   * @param modeToInclusive
   * @throws ParserException
   */
  public CompBetween( final IAxis[] axes, final String axisType, final String valueFrom, final boolean modeFromInclusive, final String valueTo, final boolean modeToInclusive ) throws ParserException
  {
    super( axes, axisType );

    m_valueFrom = m_parser.parse( valueFrom );
    m_valueTo = m_parser.parse( valueTo );
    m_modeFromIncl = modeFromInclusive;
    m_modeToIncl = modeToInclusive;
  }

  /**
   * @see org.kalypso.ogc.sensor.filter.filters.valuecomp.AbstractValueComp#internalValidates(java.lang.Object)
   */
  @Override
  public boolean internalValidates( final Object element ) throws ParserException
  {
    if( m_parser.compare( element, m_valueFrom ) < 0 )
      return false;

    if( m_parser.compare( element, m_valueTo ) > 0 )
      return false;

    if( !m_modeFromIncl && m_parser.compare( element, m_valueFrom ) <= 0 )
      return false;

    if( !m_modeToIncl && m_parser.compare( element, m_valueTo ) >= 0 )
      return false;

    return true;
  }
}
