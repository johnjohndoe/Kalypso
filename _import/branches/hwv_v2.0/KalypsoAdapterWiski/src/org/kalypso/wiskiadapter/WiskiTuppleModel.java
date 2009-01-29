package org.kalypso.wiskiadapter;

import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

import org.kalypso.commons.conversion.units.IValueConverter;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.impl.AbstractTuppleModel;
import org.kalypso.ogc.sensor.timeseries.TimeserieConstants;

/**
 * WiskiTuppleModel
 * 
 * @author schlienger
 */
public class WiskiTuppleModel extends AbstractTuppleModel implements IWiskiConstants
{
  private final List m_data;

  private final Double[] m_values;

  private final Integer[] m_kalypsoStati;

  private final IValueConverter m_vc;

  private final WiskiTimeConverter m_timeConverter;

  /**
   * @param axes
   * @param data
   *          the underlying wiski data
   * @param conv
   *          a value converter (for instance when having different units)
   */
  public WiskiTuppleModel( final IAxis[] axes, final LinkedList data, final IValueConverter conv,
      final WiskiTimeConverter timeConverter )
  {
    super( axes );

    m_vc = conv;

    m_data = filter777( data, axes[1].getType() );
    m_timeConverter = timeConverter;
    m_values = new Double[m_data.size()];
    m_kalypsoStati = new Integer[m_data.size()];

    for( int i = 0; i < axes.length; i++ )
      mapAxisToPos( axes[i], i );
  }

  /**
   * Filters the -777 Values from the given list of data values.<br>
   * <ul>
   * <li>Missing entries from start or end of the list are removed</li>
   * <li>For rain or snow (N, ?): Missing entries in the middle (holes) are set to 0.0 and staus set to warned.</li>
   * <li>Other datatypes: Missing entries in the middle (holes) are linearly interpolated by the surrounding valid
   * values; status is set to warned.</li>
   * </ul>
   */
  private static List filter777( final LinkedList data, final String axisType )
  {
    final LinkedList filteredData = WiskiUtils.trimMissing( data );

    if( axisType == TimeserieConstants.TYPE_RAINFALL )
      WiskiUtils.fillMissing( filteredData, new Double( 0.0 ) );
    else
      WiskiUtils.interpolateMissing( filteredData );

    return filteredData;
  }

  /**
   * @see org.kalypso.ogc.sensor.ITuppleModel#getCount()
   */
  public int getCount( )
  {
    return m_data.size();
  }

  /**
   * @see org.kalypso.ogc.sensor.ITuppleModel#getElement(int, org.kalypso.ogc.sensor.IAxis)
   */
  public Object getElement( final int index, final IAxis axis ) throws SensorException
  {
    switch( getPositionFor( axis ) )
    {
      case 0:
	    return m_timeConverter.wiskiToKalypso( (Date) ((HashMap) m_data.get( index )).get( WISKI_DATA_AXIS_TIMESTAMP ) );
      case 1:
        return getValue( index );
      case 2:
        return getKalypsoStatus( index );
      default:
        throw new SensorException( "Position von Axis " + axis + " ist ungültig" );
    }
  }

  private Double getValue( final int index )
  {
    if( m_values[index] == null )
    {
      double value = ((Number) ((HashMap) m_data.get( index )).get( WISKI_DATA_AXIS_VALUE )).doubleValue();

      if( m_vc != null )
        value = m_vc.convert( value );

      m_values[index] = new Double( value );
    }

    return m_values[index];
  }

  private Integer getKalypsoStatus( final int index )
  {
    if( m_kalypsoStati[index] == null )
    {
      final String status = (String) ((HashMap) m_data.get( index )).get( WISKI_DATA_AXIS_QUALITY );
      m_kalypsoStati[index] = WiskiUtils.wiskiStatus2Kalypso( status );

    }

    return m_kalypsoStati[index];
  }

  private void setValue( final int index, final Double value )
  {
    m_values[index] = value;

    double v = value.doubleValue();

    if( m_vc != null )
      v = m_vc.reverse( v );

    ((HashMap) m_data.get( index )).put( WISKI_DATA_AXIS_VALUE, new Double( v ) );
  }

  /**
   * @see org.kalypso.ogc.sensor.ITuppleModel#setElement(int, java.lang.Object, org.kalypso.ogc.sensor.IAxis)
   */
  public void setElement( final int index, final Object element, final IAxis axis ) throws SensorException
  {
    switch( getPositionFor( axis ) )
    {
      case 0:
        throw new SensorException( "Kann Datum nicht setzen" );
      case 1:
        setValue( index, (Double) element );
      case 2:
        m_kalypsoStati[index] = (Integer) element;
      default:
        throw new SensorException( "Position von Achse " + axis + " ist ungültig" );
    }
  }

  /**
   * @see org.kalypso.ogc.sensor.ITuppleModel#indexOf(java.lang.Object, org.kalypso.ogc.sensor.IAxis)
   */
  public int indexOf( final Object element, final IAxis axis ) throws SensorException
  {
    if( getPositionFor( axis ) == 0 )
    {
      final Date date = (Date) element;

      for( final Iterator it = m_data.iterator(); it.hasNext(); )
      {
        final HashMap map = (HashMap) it.next();

        final Date wiskiDate = m_timeConverter.wiskiToKalypso( (Date) map.get( WISKI_DATA_AXIS_TIMESTAMP ) );
        if( date.equals( wiskiDate ) )
          return m_data.indexOf( map );
      }
    }

    return -1;
  }
}
