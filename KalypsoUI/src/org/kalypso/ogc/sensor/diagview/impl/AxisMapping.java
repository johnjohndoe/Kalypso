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
package org.kalypso.ogc.sensor.diagview.impl;

import org.kalypso.ogc.sensor.IAxis;

/**
 * Default implementation of <code>IAxisMapping</code>.
 * 
 * @author schlienger
 *
 */
public class AxisMapping
{
  private final IAxis m_oAxis;
  private final DiagramAxis m_dAxis;

  public AxisMapping( final IAxis oAxis, final DiagramAxis dAxis )
  {
    m_oAxis = oAxis;
    m_dAxis = dAxis;
  }

  public IAxis getObservationAxis()
  {
    return m_oAxis;
  }

  public DiagramAxis getDiagramAxis()
  {
    return m_dAxis;
  }
  
  /**
   * @see java.lang.Object#toString()
   */
  public String toString( )
  {
    return m_oAxis + " - " + m_dAxis;
  }
  
//  /**
//   * Convenience method that builds a properties object and sets its property-entries
//   * so that they are adequate to the given mappings.
//   * 
//   * @param mappings
//   * @return properties
//   */
//  public static Properties saveAsProperties( IAxisMapping[] mappings )
//  {
//    Properties props = new Properties();
//    
//    for( int i = 0; i < mappings.length; i++ )
//      props.setProperty( mappings[i].getObservationAxis().getName(), mappings[i].getDiagramAxis().getIdentifier() );
//    
//    return props;
//  }
}
