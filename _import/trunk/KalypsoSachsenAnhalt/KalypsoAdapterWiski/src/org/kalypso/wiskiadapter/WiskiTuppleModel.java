package org.kalypso.wiskiadapter;

import java.sql.Timestamp;
import java.util.Date;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;

import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.impl.AbstractTuppleModel;
import org.kalypso.util.conversion.units.IValueConverter;

/**
 * WiskiTuppleModel
 * 
 * @author schlienger
 */
public class WiskiTuppleModel extends AbstractTuppleModel
{
  private final List m_data;

  private final Double[] m_values;

  private final Integer[] m_kalypsoStati;

  private final IValueConverter m_vc;

  public WiskiTuppleModel( final IAxis[] axes, final LinkedList data,
      final IValueConverter conv )
  {
    super( axes );

    m_vc = conv;

    m_data = data;
    m_values = new Double[m_data.size()];
    m_kalypsoStati = new Integer[m_data.size()];

    for( int i = 0; i < axes.length; i++ )
      mapAxisToPos( axes[i], i );
  }

  /**
   * @see org.kalypso.ogc.sensor.ITuppleModel#getCount()
   */
  public int getCount( ) throws SensorException
  {
    return m_data.size();
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
        return Timestamp.valueOf( (String) ((HashMap) m_data.get( index ))
            .get( "timestamp" ) );
      case 1:
        return getValue( index );
      case 2:
        return getKalypsoStatus( index );
      default:
        throw new SensorException( "Position von Axis " + axis
            + " ist ungültig" );
    }
  }

  private Double getValue( int index )
  {
    if( m_values[index] == null )
    {
      double value = Double
          .parseDouble( (String) ((HashMap) m_data.get( index ))
              .get( "tsc_value0" ) );

      if( m_vc != null )
        value = m_vc.convert( value );

      m_values[index] = new Double( value );
    }

    return m_values[index];
  }

  private Integer getKalypsoStatus( int index )
  {
    if( m_kalypsoStati[index] == null )
      m_kalypsoStati[index] = WiskiUtils
          .wiskiStatus2Kalypso( (String) ((HashMap) m_data.get( index ))
              .get( "QUALITY" ) );

    return m_kalypsoStati[index];
  }

  private void setValue( int index, Double value )
  {
    m_values[index] = value;

    double v = value.doubleValue();

    if( m_vc != null )
      v = m_vc.reverse( v );

    ((HashMap) m_data.get( index )).put( "tsc_value0", new Double( v ) );
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
      case 0: // TODO: Darf das Datum überhaupt geändert werden?
        ((HashMap) m_data.get( index )).put( "timestamp", new Timestamp( ((Date) element).getTime() ) );
      case 1:
        setValue( index, (Double) element );
      case 2:
        m_kalypsoStati[index] = (Integer) element;
      default:
        throw new SensorException( "Position von Achse " + axis
            + " ist ungültig" );
    }
  }

  /**
   * @see org.kalypso.ogc.sensor.ITuppleModel#indexOf(java.lang.Object,
   *      org.kalypso.ogc.sensor.IAxis)
   */
  public int indexOf( Object element, IAxis axis ) throws SensorException
  {
    // TODO Auto-generated method stub
    return 0;
  }

}
