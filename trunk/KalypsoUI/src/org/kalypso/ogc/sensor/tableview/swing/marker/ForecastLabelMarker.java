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
package org.kalypso.ogc.sensor.tableview.swing.marker;

import java.awt.Color;
import java.util.Date;

import javax.swing.JLabel;

import org.apache.commons.lang.builder.HashCodeBuilder;
import org.kalypso.ogc.sensor.timeseries.TimeserieConstants;
import org.kalypso.ogc.sensor.timeseries.TimeserieUtils;
import org.kalypso.util.runtime.args.DateRangeArgument;

/**
 * ForecastLabelMarker
 * 
 * @author schlienger
 */
public class ForecastLabelMarker implements ILabelMarker
{
  private final static Color FORECAST_BG = TimeserieUtils.getColorForMD( TimeserieConstants.MD_VORHERSAGE );

  private final static String FORECAST_TT = TimeserieConstants.MD_VORHERSAGE;

  //  private final static Icon FORECAST_ICON = new ImageIcon(
  //      ObservationTable.class.getResource( "resource/warning_small.gif" ) );

  private final DateRangeArgument m_dra;

  /**
   * Constructor
   * 
   * @param dra
   */
  public ForecastLabelMarker( DateRangeArgument dra )
  {
    m_dra = dra;
  }

  /**
   * @see org.kalypso.ogc.sensor.tableview.swing.marker.ILabelMarker#validates(java.lang.Object)
   */
  public boolean validates( final Object value )
  {
    if( !( value instanceof Date ) )
      return false;

    return m_dra.contains( (Date)value );
  }

  /**
   * @see org.kalypso.ogc.sensor.tableview.swing.marker.ILabelMarker#apply(javax.swing.JLabel)
   */
  public void apply( final JLabel label )
  {
    label.setBackground( FORECAST_BG );
    label.setToolTipText( FORECAST_TT );
    //label.setIcon( FORECAST_ICON );
  }

  /**
   * @see org.kalypso.ogc.sensor.tableview.swing.marker.ILabelMarker#reset(javax.swing.JLabel)
   */
  public void reset( JLabel label )
  {
    label.setBackground( null );
    label.setToolTipText( "" );
    label.setIcon( null );
  }

  /**
   * @see java.lang.Comparable#compareTo(java.lang.Object)
   */
  public int compareTo( Object o )
  {
    if( !( o instanceof ForecastLabelMarker ) )
      return -1;

    return m_dra.compareTo( ( (ForecastLabelMarker)o ).m_dra );
  }

  /**
   * @see java.lang.Object#equals(java.lang.Object)
   */
  public boolean equals( Object obj )
  {
    return compareTo( obj ) == 0;
  }

  /**
   * @see java.lang.Object#hashCode()
   */
  public int hashCode()
  {
    return new HashCodeBuilder().append( m_dra.getFrom() ).append( m_dra.getTo() ).hashCode();
  }
}