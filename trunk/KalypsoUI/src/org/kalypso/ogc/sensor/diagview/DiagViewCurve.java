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
package org.kalypso.ogc.sensor.diagview;

import java.awt.Color;

import org.kalypso.ogc.sensor.template.IObsProvider;
import org.kalypso.ogc.sensor.template.ObsViewItem;

/**
 * Default implementation of the <code>ITableViewColumn</code> interface
 * 
 * @author schlienger
 */
public class DiagViewCurve extends ObsViewItem
{
  private final Color m_color;
  private final AxisMapping[] m_mappings;

  public DiagViewCurve( final DiagView view, final IObsProvider obsProvider, final String name, final Color color,
      final AxisMapping[] mappings
       )
  {
    super( view, obsProvider, name );

    m_color = color;
    m_mappings = mappings;
  }
  
  public AxisMapping[] getMappings( )
  {
    return m_mappings;
  }

  public Color getColor( )
  {
    return m_color;
  }
  
//  /**
//   * Two TableViewColumn objects are equal if they have the same name and belong
//   * to the same theme.
//   * 
//   * @see java.lang.Object#equals(java.lang.Object)
//   */
//  public boolean equals( final Object obj )
//  {
//    if( !this.getClass().equals( obj.getClass() ) )
//      return false;
//
//    final ObsViewItem col = (ObsViewItem) obj;
//
//    return new EqualsBuilder().append( col.m_name, m_name ).append(
//        col.m_theme, m_theme ).isEquals();
//  }
//
//  /**
//   * @see java.lang.Object#hashCode()
//   */
//  public int hashCode( )
//  {
//    return new HashCodeBuilder( 7, 31 ).append( m_name ).append( m_theme )
//        .toHashCode();
//  }
}