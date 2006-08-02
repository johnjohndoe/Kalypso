package org.kalypso.wiskiadapter;

import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.TimeZone;

import org.kalypso.commons.conversion.units.IValueConverter;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.impl.AbstractTuppleModel;

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

  private final TimeZone m_tzWiski;

  /** flag indicating whether to convert the dates or not */
  private final boolean m_needsConversion;
  /** [can be null] when dateOffset is also null, no conversion takes place */
  private Calendar m_wiskiBegin = null;
  /** [can be null] when beginDate is also null, no conversion takes place */
  private int m_wiskiOffset = 0;
  /** the calendar field for which the dateOffset will be used */
  private int m_wiskiDateOffsetField = 0;
  /** used for date conversions between wiski and kalypso */
  private final Calendar m_cal;

  /**
   * @param axes
   * @param data
   *          the underlying wiski data
   * @param conv
   *          a value converter (for instance when having different units)
   * @param tzSrc
   *          the wiski timezone
   */
  public WiskiTuppleModel( final IAxis[] axes, final LinkedList data, final IValueConverter conv, final TimeZone tzSrc, final TsInfoItem tsinfo )
  {
    super( axes );

    m_vc = conv;

    m_tzWiski = tzSrc;

    m_data = data;
    m_values = new Double[m_data.size()];
    m_kalypsoStati = new Integer[m_data.size()];

    for( int i = 0; i < axes.length; i++ )
      mapAxisToPos( axes[i], i );

    m_needsConversion = WiskiUtils.isConversionNeeded( tsinfo );

    if( m_needsConversion )
    {
      // init a calendar for the begin date, it will be used to fetch the date fields
      m_wiskiBegin = Calendar.getInstance( m_tzWiski );
      m_wiskiBegin.setTime( tsinfo.getWiskiBegin() );
      // offset is directly adapted to take care of kalypso conventions
      m_wiskiOffset = tsinfo.getWiskiOffset().intValue();
      m_wiskiDateOffsetField = WiskiUtils.getConversionCalendarField( tsinfo.getWiskiTimeLevel() );
    }

    m_cal = Calendar.getInstance( m_tzWiski );
  }

  /**
   * @see org.kalypso.ogc.sensor.ITuppleModel#getCount()
   */
  public int getCount() throws SensorException
  {
    return m_data.size();
  }

  /**
   * @see org.kalypso.ogc.sensor.ITuppleModel#getElement(int, org.kalypso.ogc.sensor.IAxis)
   */
  public Object getElement( int index, IAxis axis ) throws SensorException
  {
    switch( getPositionFor( axis ) )
    {
      case 0:
        Date dWiski = (Date)( (HashMap)m_data.get( index ) ).get( "timestamp" );

        // 2006-06-06 Tageswert Problematik, Begin und Offset berücksichtigen
        if( m_needsConversion )
          dWiski = wiskiToKalypso( dWiski );

        return dWiski;

      case 1:
        return getValue( index );

      case 2:
        return getKalypsoStatus( index );

      default:
        throw new SensorException( "Position von Axis " + axis + " ist ungültig" );
    }
  }

  /**
   * In WISKI werden Tageswerte nicht richtig abgebildet (die Uhrzeit ist immer 00:00:00). Für die vollständige
   * Berücksichtigung der Anfangswerte und Offset, wurden zwei Zusatzfelder implementiert:
   * <ul>
   * <li>tsinfo_begin_of als timestamp, dessen Time-Anteil den Beginn der Integrationszeit des Tageswertes Beschreibt
   * (z.B. 07:30 oder ähnlich)
   * <li>tsinfo_offset_of als long, welcher beschreibt, ob die Quellwerte eines Tageswertes zum Datum x vom Tag x bis
   * x+1 einfliessen (offset 0) oder z.B. vom Tag x-1 bis zum Tag x (offset -1).
   * </ul>
   * 
   * Diese Methode passt das Datum so an, dass es die vollständige Information beinhaltet
   * 
   * @return das Datum nachdem der Tagesanfang und -Offset berücksichtigt wurde
   */
  private Date wiskiToKalypso( final Date d )
  {
    m_cal.setTime( d );

    // Begin-Zeit setzen (Hour, Minute, Second)
    m_cal.set( Calendar.HOUR, m_wiskiBegin.get( Calendar.HOUR ) );
    m_cal.set( Calendar.MINUTE, m_wiskiBegin.get( Calendar.MINUTE ) );
    m_cal.set( Calendar.SECOND, m_wiskiBegin.get( Calendar.SECOND ) );

    // Offset berücksichtigen
    m_cal.add( m_wiskiDateOffsetField, m_wiskiOffset );

    return m_cal.getTime();
  }

  private Double getValue( int index )
  {
    if( m_values[index] == null )
    {
      double value = ( (Number)( (HashMap)m_data.get( index ) ).get( "tsc_value0" ) ).doubleValue();

      if( m_vc != null )
        value = m_vc.convert( value );

      m_values[index] = new Double( value );
    }

    return m_values[index];
  }

  private Integer getKalypsoStatus( int index )
  {
    if( m_kalypsoStati[index] == null )
    {
      final String status = (String)( (HashMap)m_data.get( index ) ).get( "QUALITY" );

      //      if( !status.equals("U") )
      //        System.out.println(status);

      m_kalypsoStati[index] = WiskiUtils.wiskiStatus2Kalypso( status );

    }

    return m_kalypsoStati[index];
  }

  private void setValue( int index, Double value )
  {
    m_values[index] = value;

    double v = value.doubleValue();

    if( m_vc != null )
      v = m_vc.reverse( v );

    ( (HashMap)m_data.get( index ) ).put( "tsc_value0", new Double( v ) );
  }

  /**
   * @see org.kalypso.ogc.sensor.ITuppleModel#setElement(int, java.lang.Object, org.kalypso.ogc.sensor.IAxis)
   */
  public void setElement( int index, Object element, IAxis axis ) throws SensorException
  {
    switch( getPositionFor( axis ) )
    {
      case 0:
        throw new SensorException( "Kann Datum nicht setzen" );
      case 1:
        setValue( index, (Double)element );
      case 2:
        m_kalypsoStati[index] = (Integer)element;
      default:
        throw new SensorException( "Position von Achse " + axis + " ist ungültig" );
    }
  }

  /**
   * @see org.kalypso.ogc.sensor.ITuppleModel#indexOf(java.lang.Object, org.kalypso.ogc.sensor.IAxis)
   */
  public int indexOf( Object element, IAxis axis ) throws SensorException
  {
    if( getPositionFor( axis ) == 0 )
    {
      final Date date = (Date)element;

      for( final Iterator it = m_data.iterator(); it.hasNext(); )
      {
        final HashMap map = (HashMap)it.next();
        if( date.equals( map.get( "timestamp" ) ) )
          return m_data.indexOf( map );
      }
    }

    return -1;
  }
}
