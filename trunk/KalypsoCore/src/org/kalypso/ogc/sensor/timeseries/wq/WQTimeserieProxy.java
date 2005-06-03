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
import org.kalypso.ogc.sensor.impl.AbstractObservationDecorator;
import org.kalypso.ogc.sensor.impl.DefaultAxis;
import org.kalypso.ogc.sensor.status.KalypsoStatusUtils;
import org.kalypso.ogc.sensor.timeseries.TimeserieConstants;
import org.kalypso.ogc.sensor.timeseries.TimeserieUtils;
import org.kalypso.ogc.sensor.timeseries.wq.wechmann.WechmannFactory;
import org.kalypso.ogc.sensor.timeseries.wq.wqtable.WQTableFactory;
import org.kalypso.util.runtime.IVariableArguments;
import org.xml.sax.InputSource;

/**
 * WQTimeserieProxy for proxying W, Q, and V Timeseries.
 * 
 * @author schlienger
 */
public class WQTimeserieProxy extends AbstractObservationDecorator
{
  private IAxis[] m_axes;

  private IAxis m_dateAxis;

  private IAxis m_srcAxis;

  private IAxis m_destAxis;
  private IAxis m_destStatusAxis;

  private final String m_proxyAxisType;
  private final String m_realAxisType;

  private IWQConverter m_conv = null;

  /**
   * Constructor
   * 
   * @param realAxisType
   *          type of the real axis that will be used to proxy another axis
   * @param proxyAxisType
   *          type of the axis that should be generated based on the real axis
   * @param obs the underlying observation to proxy
   */
  public WQTimeserieProxy( final String realAxisType,
      final String proxyAxisType, final IObservation obs )
  {
    super( obs );

    m_realAxisType = realAxisType;
    m_proxyAxisType = proxyAxisType;

    configure( obs );
  }

  private final void configure( final IObservation obs )
  {
    final IAxis[] axes = obs.getAxisList();
    m_axes = new IAxis[axes.length + 2];
    for( int i = 0; i < axes.length; i++ )
      m_axes[i] = axes[i];

    m_dateAxis = ObservationUtilities.findAxisByType( axes,
        TimeserieConstants.TYPE_DATE );

    final String name = TimeserieUtils.getName( m_proxyAxisType );
    final String unit = TimeserieUtils.getUnit( m_proxyAxisType );

    m_srcAxis = ObservationUtilities.findAxisByType( axes, m_realAxisType );
    m_destAxis = new DefaultAxis( name, m_proxyAxisType, unit, Double.class,
        false, false );
    m_axes[m_axes.length - 2] = m_destAxis;
    
    m_destStatusAxis = KalypsoStatusUtils.createStatusAxisFor( m_destAxis, false );
    m_axes[m_axes.length - 1] = m_destStatusAxis;

    if( name.length() == 0 )
      throw new IllegalArgumentException(
          "Angegebene Typ für zu erzeugende Achsen wird nicht unterstützt: "
              + m_proxyAxisType );
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
    return new WQTuppleModel( super.getValues( args ), m_axes, m_dateAxis,
        m_srcAxis, m_destAxis, m_destStatusAxis, getWQConverter() );
  }

  /**
   * Lazy loading of WQ-Relation
   * 
   * @throws SensorException
   */
  private IWQConverter getWQConverter( ) throws SensorException
  {
    if( m_conv == null )
    {
      try
      {
        if( getMetadataList().containsKey( TimeserieConstants.MD_WQWECHMANN ) )
        {
          final String wechmann = getMetadataList().getProperty(
              TimeserieConstants.MD_WQWECHMANN );

          m_conv = WechmannFactory.parse( new InputSource( new StringReader(
              wechmann ) ) );
        }
        else if( getMetadataList().containsKey( TimeserieConstants.MD_WQTABLE ) )
        {
          final String wqtable = getMetadataList().getProperty(
              TimeserieConstants.MD_WQTABLE );

          m_conv = WQTableFactory.parse( new InputSource( new StringReader(
              wqtable ) ) );
        }
        else
          throw new IllegalStateException(
              "Kann keine WQ-Observation erzeugen: WQ-Beziehung fehlt" );
      }
      catch( WQException e )
      {
        throw new SensorException( e );
      }
    }

    return m_conv;
  }

  /**
   * @see org.kalypso.ogc.sensor.filter.filters.AbstractObservationFilter#setValues(org.kalypso.ogc.sensor.ITuppleModel)
   */
  public void setValues( final ITuppleModel values ) throws SensorException
  {
    super.setValues( WQTuppleModel.reverse( values, m_obs.getAxisList() ) );
  }

  public IAxis getDateAxis( )
  {
    return m_dateAxis;
  }

  public IAxis getDestAxis( )
  {
    return m_destAxis;
  }

  public IAxis getSrcAxis( )
  {
    return m_srcAxis;
  }
}