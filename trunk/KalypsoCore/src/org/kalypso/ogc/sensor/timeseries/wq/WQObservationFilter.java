package org.kalypso.ogc.sensor.timeseries.wq;

import java.io.StringReader;

import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.filter.AbstractObservationFilter;
import org.kalypso.ogc.sensor.impl.DefaultAxis;
import org.kalypso.ogc.sensor.timeseries.TimeserieConstants;
import org.kalypso.ogc.sensor.timeseries.wq.wechmann.WechmannException;
import org.kalypso.ogc.sensor.timeseries.wq.wechmann.WechmannFactory;
import org.kalypso.ogc.sensor.timeseries.wq.wechmann.WechmannGroup;
import org.kalypso.util.runtime.IVariableArguments;
import org.xml.sax.InputSource;

/**
 * WQObservationFilter
 * 
 * Conf: (one of TimeserieConstants.TYPE_*) denotes the type of the model
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
   * @see org.kalypso.ogc.sensor.filter.AbstractObservationFilter#initFilter(java.lang.String,
   *      org.kalypso.ogc.sensor.IObservation)
   */
  public void initFilter( final String conf, final IObservation obs )
  {
    super.initFilter( conf, obs );

    // (one of TimeserieConstants.TYPE_*) denotes the type of the model
    final String type = conf;

    final IAxis[] axes = obs.getAxisList();
    m_axes = new IAxis[axes.length + 1];
    for( int i = 0; i < axes.length; i++ )
      m_axes[i] = axes[i];

    m_dateAxis = ObservationUtilities.findAxisByType( axes,
        TimeserieConstants.TYPE_DATE );

    if( TimeserieConstants.TYPE_RUNOFF.equals( type ) )
    {
      m_srcAxis = ObservationUtilities.findAxisByType( axes,
          TimeserieConstants.TYPE_RUNOFF );
      m_destAxis = new DefaultAxis( "W", TimeserieConstants.TYPE_WATERLEVEL,
          "cm", Double.class, m_axes.length - 1, false );
      m_axes[m_axes.length - 1] = m_destAxis;
    }
    else if( TimeserieConstants.TYPE_WATERLEVEL.equals( type ) )
    {
      m_srcAxis = ObservationUtilities.findAxisByType( axes,
          TimeserieConstants.TYPE_WATERLEVEL );
      m_destAxis = new DefaultAxis( "Q", TimeserieConstants.TYPE_RUNOFF, "m^3",
          Double.class, m_axes.length - 1, false );
      m_axes[m_axes.length - 1] = m_destAxis;
    }
    else
      throw new IllegalArgumentException(
          "Type is not supported. Must one of W_AVAILABLE or Q_AVAILABE." );
  }

  /**
   * @see org.kalypso.ogc.sensor.filter.AbstractObservationFilter#getAxisList()
   */
  public IAxis[] getAxisList( )
  {
    return m_axes;
  }

  /**
   * @see org.kalypso.ogc.sensor.filter.AbstractObservationFilter#getValues(org.kalypso.util.runtime.IVariableArguments)
   */
  public ITuppleModel getValues( IVariableArguments args )
      throws SensorException
  {
    final String wechmann = getMetadataList().getProperty(
        TimeserieConstants.MD_WQ );
    final WechmannGroup group;
    try
    {
      group = WechmannFactory.parse( new InputSource( new StringReader(
          wechmann ) ) );
    }
    catch( WechmannException e )
    {
      throw new SensorException( e );
    }

    return new WQTuppleModel( super.getValues( args ), m_axes, m_dateAxis,
        m_srcAxis, m_destAxis, group );
  }

  /**
   * @see org.kalypso.ogc.sensor.filter.AbstractObservationFilter#setValues(org.kalypso.ogc.sensor.ITuppleModel)
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