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
package org.kalypso.ogc.sensor.tableview.swing.renderer;

import java.awt.Component;
import java.text.DateFormat;
import java.util.Date;
import java.util.Set;
import java.util.TimeZone;
import java.util.TreeSet;

import javax.swing.JLabel;
import javax.swing.JTable;
import javax.swing.table.DefaultTableCellRenderer;

import org.kalypso.ogc.sensor.tableview.swing.marker.ILabelMarker;
import org.kalypso.ogc.sensor.timeseries.TimeserieUtils;

/**
 * Helper: formatiert das Datum auf eine richtige Art und Weise
 * 
 * @author schlienger
 */
public class DateTableCellRenderer extends DefaultTableCellRenderer
{
  /** maps dates to markers */
  private final Set<ILabelMarker> m_markers = new TreeSet<ILabelMarker>();

  private final DateFormat m_df;

  public DateTableCellRenderer( )
  {
    m_df = TimeserieUtils.getDateFormat();
  }

  /**
   * @see javax.swing.table.DefaultTableCellRenderer#getTableCellRendererComponent(javax.swing.JTable, java.lang.Object,
   *      boolean, boolean, int, int)
   */
  @Override
  public Component getTableCellRendererComponent( final JTable table, final Object value, final boolean isSelected, final boolean hasFocus, final int row, final int column )
  {
    final JLabel label = (JLabel) super.getTableCellRendererComponent( table, value, isSelected, hasFocus, row, column );

    // WORKAROUND: sometimes it comes here but value is not a date. This must be
    // a threading problem. The workaround is to return null. I'm not sure
    // if that'll always work correctly though.
    if( !(value instanceof Date) )
      return null;

    label.setText( m_df.format( value ) );

    if( !isSelected )
    {
      // maybe mark this item
      for( final ILabelMarker marker : m_markers )
      {
        if( marker.validates( value ) )
          marker.apply( label );
        else
          marker.reset( label );
      }
    }
    else
    {
      label.setIcon( null );
    }

    return label;
  }

  /**
   * Adds a marker that modifies this renderer.
   * 
   * @param marker
   */
  public void addMarker( final ILabelMarker marker )
  {
    m_markers.add( marker );
  }

  public void removeMarker( final ILabelMarker marker )
  {
    m_markers.remove( marker );
  }

  /**
   * Clears all markers
   */
  public void clearMarkers( )
  {
    m_markers.clear();
  }

  /**
   * Set the timezone for the rendering component. This is the timezone into which the dates should be displayed in the
   * table. It allows you to override the default behavior which is uses the locale setting.
   */
  public void setTimeZone( final TimeZone tz )
  {
    m_df.setTimeZone( tz );
  }
}