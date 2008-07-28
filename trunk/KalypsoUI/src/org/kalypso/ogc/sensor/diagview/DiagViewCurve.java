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
import java.awt.Stroke;
import java.util.Set;

import org.kalypso.ogc.sensor.MetadataList;
import org.kalypso.ogc.sensor.template.IObsProvider;
import org.kalypso.ogc.sensor.template.ObsViewItem;
import org.kalypso.ogc.sensor.timeseries.TimeserieConstants;
import org.kalypso.ogc.sensor.timeseries.TimeserieUtils;

/**
 * Default implementation of the <code>ITableViewColumn</code> interface
 * 
 * @author schlienger
 */
public class DiagViewCurve extends ObsViewItem
{
  private Color m_color;

  private Stroke m_stroke;

  private final AxisMapping[] m_mappings;

  public DiagViewCurve( final DiagView view, final IObsProvider obsProvider, final String name, final Color color, final Stroke stroke, final AxisMapping[] mappings )
  {
    super( view, obsProvider, name );

    m_color = color;
    m_stroke = stroke;
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

  public void setColor( final Color color )
  {
    m_color = color;
  }

  public Stroke getStroke( )
  {
    return m_stroke;
  }

  public void setStroke( final Stroke stroke )
  {
    m_stroke = stroke;
  }
  
  /**
   * @return true when this curve represents a Water-Level and the Water-Level-Feature is activated in the view
   */
  public boolean isDisplayAlarmLevel( )
  {
    boolean hasWaterLevelAxis = false;
    for( final AxisMapping element : m_mappings )
    {
      if( element.getObservationAxis().getType().equals( TimeserieConstants.TYPE_WATERLEVEL ) )
      {
        hasWaterLevelAxis = true;
        break;
      }
    }

    return hasWaterLevelAxis && getView().isFeatureEnabled( TimeserieConstants.FEATURE_ALARMLEVEL );
  }

  /**
   * @return the list of alarm-levels, or an empty array if nothing found
   */
  public AlarmLevel[] getAlarmLevels( )
  {
    final String[] alarms = TimeserieUtils.findOutMDAlarmLevel( getObservation() );
    final AlarmLevel[] als = new AlarmLevel[alarms.length];

    final MetadataList mdl = getObservation().getMetadataList();

    for( int i = 0; i < alarms.length; i++ )
    {
      final Double value = new Double( mdl.getProperty( alarms[i] ) );

      als[i] = new AlarmLevel( value.doubleValue(), alarms[i] );
    }

    return als;
  }

  /**
   * Simple structre holding the alarm-level information
   * 
   * @author schlienger
   */
  public static class AlarmLevel
  {
    public final double value;

    public final String label;

    public final Color color;

    public AlarmLevel( final double val, final String lbl )
    {
      this.value = val;
      this.label = lbl;

      this.color = TimeserieUtils.getColorForAlarmLevel( lbl );
    }

    @Override
    public String toString( )
    {
      return label;
    }
  }

  /**
   * @see org.kalypso.ogc.sensor.template.ObsViewItem#shouldBeHidden(java.util.List)
   */
  @Override
  public boolean shouldBeHidden( final Set<String> hiddenTypes )
  {
    for( final AxisMapping element : m_mappings )
    {
      if( hiddenTypes.contains( element.getObservationAxis().getType() ) )
        return true;
    }

    return false;
  }
}