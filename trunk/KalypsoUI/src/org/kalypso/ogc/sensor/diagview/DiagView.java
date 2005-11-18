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
import java.util.Hashtable;
import java.util.List;
import java.util.Map;

import org.kalypso.contribs.java.awt.ColorUtilities;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.status.KalypsoStatusUtils;
import org.kalypso.ogc.sensor.template.IObsProvider;
import org.kalypso.ogc.sensor.template.ObsView;
import org.kalypso.ogc.sensor.template.ObsViewItem;
import org.kalypso.ogc.sensor.template.ObsViewUtils;
import org.kalypso.ogc.sensor.timeseries.TimeserieUtils;

/**
 * @author schlienger
 */
public class DiagView extends ObsView
{
  private String m_title;

  private String m_legendName;

  private boolean m_showLegend;

  /** axisID -> axis */
  private final Map m_axesMap = new Hashtable();

  public DiagView()
  {
    this( "", "", false );
  }

  public DiagView( final boolean showLegend )
  {
    this( "", "", showLegend );
  }

  public DiagView( final String title, final String legendName, final boolean showLegend )
  {
    super();

    m_title = title;
    m_legendName = legendName;
    m_showLegend = showLegend;
  }

  public void dispose()
  {
    removeAllAxes();

    super.dispose();
  }

  private void removeAllAxes()
  {
    m_axesMap.clear();
  }

  /**
   * @see org.kalypso.ogc.sensor.template.ObsView#removeAllItems()
   */
  public void removeAllItems()
  {
    removeAllAxes();

    super.removeAllItems();
  }

  public String getTitle()
  {
    return m_title;
  }

  public String toString()
  {
    return m_title;
  }

  public String getLegendName()
  {
    return m_legendName;
  }

  public boolean isShowLegend()
  {
    return m_showLegend;
  }

  public void setTitle( String title )
  {
    m_title = title;

    refreshView( null );
  }

  public void setLegendName( String name )
  {
    m_legendName = name;

    refreshView( null );
  }

  public void setShowLegend( boolean show )
  {
    m_showLegend = show;

    refreshView( null );
  }

  public DiagramAxis[] getDiagramAxes()
  {
    return (DiagramAxis[])m_axesMap.values().toArray( new DiagramAxis[m_axesMap.size()] );
  }

  public void addAxis( final DiagramAxis axis )
  {
    m_axesMap.put( axis.getIdentifier(), axis );

    refreshView( null );
  }

  public DiagramAxis getDiagramAxis( final String diagAxisId )
  {
    return (DiagramAxis)m_axesMap.get( diagAxisId );
  }

  /**
   * Update the diagView with the new observation, perform best guess to know which curves will be added to it.
   * 
   * @see org.kalypso.ogc.sensor.template.ObsView#addObservation(org.kalypso.ogc.sensor.template.IObsProvider,
   *      java.lang.String, org.kalypso.ogc.sensor.template.ObsView.ItemData)
   */
  public void addObservation( final IObsProvider provider, final String tokenizedName, final ItemData data )
  {
    final List ignoreTypeList = getIgnoreTypesAsList();

    final IObservation obs = provider.getObservation();
    if( obs != null )
    {
      final IAxis[] valueAxis = KalypsoStatusUtils.findAxesByClass( obs.getAxisList(), Number.class, true );
      final IAxis[] keyAxes = ObservationUtilities.findAxesByKey( obs.getAxisList() );

      if( keyAxes.length == 0 )
        return;

      final IAxis keyAxis = keyAxes[0];

      for( int i = 0; i < valueAxis.length; i++ )
      {
        if( keyAxis == valueAxis[i] )
          continue;

        final String valueAxisType = valueAxis[i].getType();
        if( !ignoreTypeList.contains( valueAxisType ) )
        {
          final AxisMapping[] mappings = new AxisMapping[2];

          // look for a date diagram axis
          DiagramAxis daDate = getDiagramAxis( keyAxis.getType() );
          if( daDate == null )
          {
            daDate = DiagViewUtils.createAxisFor( keyAxis );
            addAxis( daDate );
          }
          mappings[0] = new AxisMapping( keyAxis, daDate );

          // look for a value diagram axis
          DiagramAxis daValue = getDiagramAxis( valueAxisType );
          if( daValue == null )
          {
            daValue = DiagViewUtils.createAxisFor( valueAxis[i] );
            addAxis( daValue );
          }
          mappings[1] = new AxisMapping( valueAxis[i], daValue );

          // if color not defined, find suitable one
          final Color color;
          if( data.color != null )
            color = data.color;
          else
          {
            // look in existing items to see if one has same type
            int found = 0;
            final ObsViewItem[] items = getItems();
            for( int j = 0; j < items.length; j++ )
            {
              final AxisMapping[] mps = ( (DiagViewCurve)items[j] ).getMappings();
              if( mps[1].getObservationAxis().getType().equals( valueAxis[i].getType() ) )
                found++;
            }

            color = ColorUtilities.derivateColor( TimeserieUtils.getColorFor( valueAxis[i].getType() ), found );
          }

          final String name = ObsViewUtils.replaceTokens( tokenizedName, obs, valueAxis[i] );

          final DiagViewCurve curve = new DiagViewCurve( this, provider.copy(), name, color, data.stroke, mappings );

          addItem( curve );
        }
      }
    }
  }
}