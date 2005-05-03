package org.kalypso.dcadapter;

import java.util.Date;
import java.util.HashMap;
import java.util.Map;

import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IAxisRange;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.impl.DefaultAxisRange;
import org.kalypso.ogc.sensor.status.KalypsoStatusUtils;

import com.bce.datacenter.db.timeseries.TimeserieTupple;

/**
 * DataCenterTuppleModel
 * 
 * @author marc
 */
public class DataCenterTuppleModel implements ITuppleModel
{
  private final TimeserieTupple[] m_tupples;

  private final IAxis[] m_axes;

  private final Map m_axesPos;
  
  public DataCenterTuppleModel( final TimeserieTupple[] tupples, IAxis[] axes )
  {
    m_tupples = tupples;
    m_axes = axes;

    m_axesPos = new HashMap( axes.length );
    for( int i = 0; i < axes.length; i++ )
      m_axesPos.put( axes[i], new Integer( i ) );
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
    return m_tupples.length;
  }

  /**
   * @see org.kalypso.ogc.sensor.ITuppleModel#getRangeFor(org.kalypso.ogc.sensor.IAxis)
   */
  public IAxisRange getRangeFor( IAxis axis ) throws SensorException
  {
    if( m_tupples.length == 0 )
      return null;
    
    switch( getPositionFor( axis ) )
    {
      case 0:
        return new DefaultAxisRange( m_tupples[0].getDate(), m_tupples[m_tupples.length - 1 ].getDate() );
      default:
        throw new SensorException( "Axis " + axis + " not supported for method getRangeFor() " );
    }
  }

  /**
   * @see org.kalypso.ogc.sensor.ITuppleModel#getElement(int,
   *      org.kalypso.ogc.sensor.IAxis)
   */
  public Object getElement( int index, IAxis axis ) throws SensorException
  {
    switch( getPositionFor( axis ) )
    {
      case 0:
        return m_tupples[index].getDate();
      case 1:
        return m_tupples[index].getValue();
      case 2:
        return m_tupples[index].getStatus();
      default:
        throw new SensorException( "Invalid axis position. Must be 0, 1 or 2." );
    }
  }

  /**
   * @see org.kalypso.ogc.sensor.ITuppleModel#setElement(int, java.lang.Object,
   *      org.kalypso.ogc.sensor.IAxis)
   */
  public void setElement( int index, Object element, IAxis axis )
      throws SensorException
  {
    switch( getPositionFor( axis ) )
    {
      case 0:
        m_tupples[index].setDate( (Date) element );
      case 1:
        m_tupples[index].setValue( (Double) element );
      case 2:
        m_tupples[index].setStatus( "x" ); // TODO
      default:
        throw new SensorException( "Invalid axis position. Must be 0, 1 or 2." );
    }
  }

  /**
   * @see org.kalypso.ogc.sensor.ITuppleModel#indexOf(java.lang.Object,
   *      org.kalypso.ogc.sensor.IAxis)
   */
  public int indexOf( Object element, IAxis axis ) throws SensorException
  {
    if( ((Integer) m_axesPos.get( axis )).intValue() == 0 )
    {
      for( int i = 0; i < m_tupples.length; i++ )
        if( m_tupples[i].getDate().equals( element ) )
          return i;
    }
    
    return -1;
  }

  /**
   * Creates tupples according to the model
   * 
   * @param model
   * @return tupples
   * @throws SensorException
   */
  public static TimeserieTupple[] toTupples( final ITuppleModel model ) throws SensorException
  {
    final IAxis[] axes = model.getAxisList();
    
    final IAxis dateAxis = ObservationUtilities.findAxisByClass( axes, Date.class );
    final IAxis valueAxis = KalypsoStatusUtils.findAxisByClass( axes, Double.class, true );
    
    final TimeserieTupple[] tupples = new TimeserieTupple[ model.getCount()];
    
    for( int i = 0; i < model.getCount(); i++ )
    {
      tupples[i].setDate( (Date) model.getElement(i, dateAxis) );
      tupples[i].setValue( (Double) model.getElement(i, valueAxis) );
      // TODO tupples[i].setStatus( (Double) model.getElement(i, valueAxis) );
    }
    
    return tupples;
  }

  /**
   * @see org.kalypso.ogc.sensor.ITuppleModel#getPositionFor(org.kalypso.ogc.sensor.IAxis)
   */
  public int getPositionFor( IAxis axis ) throws SensorException
  {
    if( m_axesPos.containsKey( axis ) )
      return ((Integer) m_axesPos.get( axis )).intValue();

    throw new SensorException( "The axis " + axis
        + " is not part of this model" );
  }
}