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
package org.kalypso.ogc.sensor.timeseries.wq;

import java.io.StringReader;

import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.filter.filters.AbstractObservationFilter;
import org.kalypso.ogc.sensor.impl.DefaultAxis;
import org.kalypso.ogc.sensor.timeseries.TimeserieConstants;
import org.kalypso.ogc.sensor.timeseries.TimeserieUtils;
import org.kalypso.ogc.sensor.timeseries.wq.wechmann.WechmannException;
import org.kalypso.ogc.sensor.timeseries.wq.wechmann.WechmannFactory;
import org.kalypso.ogc.sensor.timeseries.wq.wechmann.WechmannGroup;
import org.kalypso.ogc.sensor.timeseries.wq.wechmann.WechmannSet;
import org.kalypso.util.runtime.IVariableArguments;
import org.xml.sax.InputSource;

/**
 * WQObservationFilter
 * 
 * @author schlienger
 */
public class WQObservationFilter extends AbstractObservationFilter
{
  private IAxis[] m_axes;

  private IAxis m_dateAxis;

  private IAxis m_srcAxis;

  private IAxis m_destAxis;

  /**
   * The argument conf is a String as defined in TimeserieConstants.TYPE_*. It
   * denotes the type of the value axis of the model that is transformed. Thus,
   * if the type is W, then this filter generates Q.
   * 
   * @see org.kalypso.ogc.sensor.filter.IObservationFilter#initFilter(java.lang.Object,
   *      org.kalypso.ogc.sensor.IObservation)
   */
  public void initFilter( final Object conf, final IObservation obs )
      throws SensorException
  {
    super.initFilter( conf, obs );

    // (one of TimeserieConstants.TYPE_*) denotes the type of the model
    final String type = conf.toString();

    final IAxis[] axes = obs.getAxisList();
    m_axes = new IAxis[axes.length + 1];
    for( int i = 0; i < axes.length; i++ )
      m_axes[i] = axes[i];

    m_dateAxis = ObservationUtilities.findAxisByType( axes,
        TimeserieConstants.TYPE_DATE );

    if( TimeserieConstants.TYPE_RUNOFF.equals( type ) )
    {
      final String name = TimeserieUtils
          .getName( TimeserieConstants.TYPE_WATERLEVEL );
      final String unit = TimeserieUtils
          .getUnit( TimeserieConstants.TYPE_WATERLEVEL );

      m_srcAxis = ObservationUtilities.findAxisByType( axes,
          TimeserieConstants.TYPE_RUNOFF );
      m_destAxis = new DefaultAxis( name, TimeserieConstants.TYPE_WATERLEVEL,
          unit, Double.class, false, false );
      m_axes[m_axes.length - 1] = m_destAxis;
    }
    else if( TimeserieConstants.TYPE_WATERLEVEL.equals( type ) )
    {
      final String name = TimeserieUtils
          .getName( TimeserieConstants.TYPE_RUNOFF );
      final String unit = TimeserieUtils
          .getUnit( TimeserieConstants.TYPE_RUNOFF );

      m_srcAxis = ObservationUtilities.findAxisByType( axes,
          TimeserieConstants.TYPE_WATERLEVEL );
      m_destAxis = new DefaultAxis( name, TimeserieConstants.TYPE_RUNOFF, unit,
          Double.class, false, false );
      m_axes[m_axes.length - 1] = m_destAxis;
    }
    else
      throw new IllegalArgumentException(
          "Type is not supported. Must one of W_AVAILABLE or Q_AVAILABE." );
  }

  /**
   * @see org.kalypso.ogc.sensor.filter.filters.AbstractObservationFilter#getAxisList()
   */
  public IAxis[] getAxisList( )
  {
    return m_axes;
  }

  /**
   * @see org.kalypso.ogc.sensor.filter.filters.AbstractObservationFilter#getValues(org.kalypso.util.runtime.IVariableArguments)
   */
  public ITuppleModel getValues( IVariableArguments args )
      throws SensorException
  {
    final String wechmann = getMetadataList().getProperty(
        TimeserieConstants.MD_WQ );
    final WechmannGroup group;
    if( wechmann != null )
    {
      try
      {
        group = WechmannFactory.parse( new InputSource( new StringReader(
            wechmann ) ) );
      }
      catch( WechmannException e )
      {
        throw new SensorException( e );
      }
    }
    else
    {
      group = new WechmannGroup( new WechmannSet[0] );
    }

    return new WQTuppleModel( super.getValues( args ), m_axes, m_dateAxis,
        m_srcAxis, m_destAxis, group );
  }

  /**
   * @see org.kalypso.ogc.sensor.filter.filters.AbstractObservationFilter#setValues(org.kalypso.ogc.sensor.ITuppleModel)
   */
  public void setValues( final ITuppleModel values ) throws SensorException
  {
    super.setValues( values );
  }

  /**
   * @return Returns the dateAxis.
   */
  public IAxis getDateAxis( )
  {
    return m_dateAxis;
  }

  /**
   * @return Returns the destAxis.
   */
  public IAxis getDestAxis( )
  {
    return m_destAxis;
  }

  /**
   * @return Returns the srcAxis.
   */
  public IAxis getSrcAxis( )
  {
    return m_srcAxis;
  }
}