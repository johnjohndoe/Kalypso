package org.kalypso.ogc.sensor.timeseries;

import java.util.Date;
import java.util.HashMap;
import java.util.Map;

import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.impl.DefaultAxis;
import org.kalypso.ogc.sensor.timeseries.wq.WechmannException;
import org.kalypso.ogc.sensor.timeseries.wq.WechmannFunction;
import org.kalypso.ogc.sensor.timeseries.wq.WechmannGroup;
import org.kalypso.ogc.sensor.timeseries.wq.WechmannSet;

/**
 * @author schlienger
 */
public class WQTuppleModel implements ITuppleModel
{
  private final ITuppleModel m_model;

  private final IAxis[] m_axes;

  private final String m_type;

  private final IAxis m_srcAxis;

  private final IAxis m_destAxis;

  private final IAxis m_dateAxis;

  private final Map m_values = new HashMap();

  private final WechmannGroup m_wsets;

  /**
   * Creates a <code>WQTuppleModel</code> that can generate either W or Q on
   * the fly. It needs an existing model from whitch the values of the given
   * type are fetched.
   * <p>
   * If it bases on a TimeserieConstants.TYPE_RUNOFF it can generate
   * TYPE_WATERLEVEL values and vice versa.
   * 
   * @param model
   *          base model delivering values of the given type
   * @param type
   *          (one of TimeserieConstants.TYPE_*) denotes the type of the model
   * @param wsets
   *          parameters used to perform the conversion
   */
  public WQTuppleModel( final ITuppleModel model, final String type,
      final WechmannGroup wsets )
  {
    m_type = type;
    m_model = model;
    m_wsets = wsets;

    final IAxis[] axes = model.getAxisList();
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
   * @param type
   * @return true if the model is of the given type
   */
  public boolean isType( final String type )
  {
    return m_type.equals( type );
  }

  /**
   * @see org.kalypso.ogc.sensor.ITuppleModel#getAxisList()
   */
  public IAxis[] getAxisList( )
  {
    return m_axes;
  }

  /**
   * @see org.kalypso.ogc.sensor.ITuppleModel#getCount()
   */
  public int getCount( ) throws SensorException
  {
    return m_model.getCount();
  }

  /**
   * @see org.kalypso.ogc.sensor.ITuppleModel#getElement(int,
   *      org.kalypso.ogc.sensor.IAxis)
   */
  public Object getElement( final int index, final IAxis axis )
      throws SensorException
  {
    if( axis.equals( m_destAxis ) )
    {
      final Integer objIndex = new Integer( index );
      
      if( !m_values.containsKey( objIndex ) )
      {
        Object value = null;
        
        final Date d = (Date) m_model.getElement( index, m_dateAxis );

        final WechmannSet set = m_wsets.getFor( d );

        if( axis.getType().equals( TimeserieConstants.TYPE_RUNOFF ) )
        {
          final double w = ((Number) m_model.getElement( index, m_srcAxis ))
              .doubleValue();
          try
          {
            value = new Double( WechmannFunction.computeQ( set.getForW( w ), w ) );
          }
          catch( WechmannException e )
          {
            value = new Double( Double.NaN );
          }
        }
        else if( axis.getType().equals( TimeserieConstants.TYPE_WATERLEVEL ) )
        {
          final double q = ((Number) m_model.getElement( index, m_srcAxis ))
              .doubleValue();
          try
          {
            value = new Double( WechmannFunction.computeW( set.getForQ( q ), q ) );
          }
          catch( WechmannException e )
          {
            value = new Double( Double.NaN );
          }
        }

        m_values.put( objIndex, value );
        
        return value;
      }

      return m_values.get( objIndex );
    }

    return m_model.getElement( index, axis );
  }

  /**
   * @see org.kalypso.ogc.sensor.ITuppleModel#setElement(int, java.lang.Object,
   *      org.kalypso.ogc.sensor.IAxis)
   */
  public void setElement( final int index, final Object element,
      final IAxis axis ) throws SensorException
  {
    if( axis.equals( m_destAxis ) )
    {
      final Date d = (Date) m_model.getElement( index, m_dateAxis );

      final WechmannSet set = m_wsets.getFor( d );

      if( axis.getType().equals( TimeserieConstants.TYPE_RUNOFF ) )
      {
        final double q = ((Number) element).doubleValue();
        double w;
        try
        {
          w = WechmannFunction.computeW( set.getForQ( q ), q );
        }
        catch( WechmannException e )
        {
          w = Double.NaN;
        }

        m_model.setElement( index, new Double( w ), m_srcAxis );
      }
      else if( axis.getType().equals( TimeserieConstants.TYPE_WATERLEVEL ) )
      {
        final double w = ((Number) element).doubleValue();
        double q;
        try
        {
          q = WechmannFunction.computeQ( set.getForW( w ), w );
        }
        catch( WechmannException e )
        {
          q = Double.NaN;
        }

        m_model.setElement( index, new Double( q ), m_srcAxis );
      }

      m_values.put( new Integer(index), element );
    }

    m_model.setElement( index, element, axis );
  }

  /**
   * @see org.kalypso.ogc.sensor.ITuppleModel#indexOf(java.lang.Object,
   *      org.kalypso.ogc.sensor.IAxis)
   */
  public int indexOf( final Object element, final IAxis axis )
      throws SensorException
  {
    if( axis.equals( m_destAxis ) )
      return -1; // TODO: check if ok, always returning -1 here. Should be ok, since indexOf only makes sensor for key axes

    return m_model.indexOf( element, axis );
  }
}