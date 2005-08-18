package org.kalypso.psiadapter.repository;

import java.util.Arrays;
import java.util.Calendar;
import java.util.Date;
import java.util.NoSuchElementException;

import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.impl.AbstractTuppleModel;
import org.kalypso.ogc.sensor.status.KalypsoStatusUtils;
import org.kalypso.ogc.sensor.timeseries.TimeserieConstants;
import org.kalypso.psiadapter.PSICompactFactory;
import org.kalypso.psiadapter.util.ArchiveDataDateComparator;
import org.kalypso.commons.conversion.units.IValueConverter;

import de.psi.go.lhwz.PSICompact;
import de.psi.go.lhwz.PSICompact.ArchiveData;

/**
 * @author schlienger
 */
public class PSICompactTuppleModel extends AbstractTuppleModel
{
  private final ArchiveData[] m_data;

  private final Double[] m_values;

  private final Integer[] m_kalypsoStati;

  private final IValueConverter m_vc;

  /**
   * Constructor with ArchiveData[]
   * 
   * @param axes
   *          list of axes (0: date, 1:value, 2:status)
   */
  public PSICompactTuppleModel( final ArchiveData[] data, final IAxis[] axes )
  {
    this( data, axes, null );
  }

  /**
   * Constructor with ArchiveData[] and a value converter
   * 
   * @param axes
   *          list of axes (0: date, 1:value, 2:status)
   * @param vc
   *          optional value converter when units are different between kalypso and psi
   */
  public PSICompactTuppleModel( final ArchiveData[] data, final IAxis[] axes, final IValueConverter vc )
  {
    super( axes );

    m_data = data;
    m_vc = vc;

    m_values = new Double[m_data.length];
    m_kalypsoStati = new Integer[m_data.length];

    for( int i = 0; i < axes.length; i++ )
      mapAxisToPos( axes[i], i );
  }

  /**
   * Create a new model based on an existing one.
   * 
   * @return TuppleModel
   * 
   * @throws NoSuchElementException
   *           when axis was not found
   */
  public static PSICompactTuppleModel copyModel( final ITuppleModel model, final IValueConverter vc )
      throws NoSuchElementException, SensorException
  {
    final IAxis[] axes = model.getAxisList();

    final IAxis dateAxis = ObservationUtilities.findAxisByClass( axes, Date.class );
    final IAxis valueAxis = KalypsoStatusUtils.findAxisByClass( axes, Number.class, true );
    final IAxis statusAxis = KalypsoStatusUtils.findStatusAxisFor( axes, valueAxis );

    final ArchiveData[] data = constructData( model, dateAxis, valueAxis, vc );

    return new PSICompactTuppleModel( data, new IAxis[]
    {
        dateAxis,
        valueAxis,
        statusAxis } );
  }

  /**
   * Helper that creates ArchiveData[] having a ITuppleModel
   * 
   * @param vc
   *          optional converter
   * @return ArchiveData[]
   */
  private final static ArchiveData[] constructData( final ITuppleModel model, final IAxis dateAxis,
      final IAxis valueAxis, final IValueConverter vc ) throws SensorException
  {
    final ArchiveData[] data = new ArchiveData[model.getCount()];

    final Calendar cal = PSICompactFactory.getCalendarForPSICompact();
    
    for( int i = 0; i < data.length; i++ )
    {
      Date date = (Date)model.getElement( i, dateAxis );
      int status = PSICompact.STATUS_AUTO;
      double value = ( (Number)model.getElement( i, valueAxis ) ).doubleValue();

      // convert the value if necessary
      if( vc != null )
        value = vc.reverse( value );
      
      // use calendar to fit timezone
      cal.setTime( date );
      
      data[i] = new ArchiveData( cal.getTime(), status, value );
    }

    return data;
  }

  public ArchiveData[] getData()
  {
    return m_data;
  }

  private Double getValue( int index )
  {
    if( m_values[index] == null )
    {
      double value = m_data[index].getValue();

      if( m_vc != null )
        value = m_vc.convert( value );

      m_values[index] = new Double( value );
    }

    return m_values[index];
  }

  private Integer getKalypsoStatus( int index )
  {
    if( m_kalypsoStati[index] == null )
      m_kalypsoStati[index] = PSICompactRepositoryFactory.psiStatusToMask( m_data[index].getStatus() );

    return m_kalypsoStati[index];
  }

  private void setValue( int index, Double value )
  {
    m_values[index] = value;

    double v = value.doubleValue();

    if( m_vc != null )
      v = m_vc.reverse( v );

    m_data[index].setValue( v );
  }

  /**
   * @see org.kalypso.ogc.sensor.ITuppleModel#getCount()
   */
  public int getCount()
  {
    return m_data.length;
  }

  /**
   * @see org.kalypso.ogc.sensor.ITuppleModel#indexOf(java.lang.Object, org.kalypso.ogc.sensor.IAxis)
   */
  public int indexOf( final Object element, final IAxis axis )
  {
    // we can only find the index of a date
    if( !axis.getType().equals( TimeserieConstants.TYPE_DATE ) )
      return -1;

    // TRICKY: wir gehen davon aus dass m_data sortiert ist! Sollte eigentlich der Fall
    // sein da es sich um eine Zeitreihe handelt.
    return Arrays.binarySearch( m_data, element, new ArchiveDataDateComparator() );
  }

  /**
   * @see org.kalypso.ogc.sensor.ITuppleModel#getElement(int, org.kalypso.ogc.sensor.IAxis)
   */
  public Object getElement( int index, IAxis axis ) throws SensorException
  {
    switch( getPositionFor( axis ) )
    {
    case 0:
      return m_data[index].getTimestamp();
    case 1:
      return getValue( index );
    case 2:
      return getKalypsoStatus( index );
    default:
      throw new SensorException( "Position von Axis " + axis + " ist ungültig" );
    }
  }

  /**
   * @see org.kalypso.ogc.sensor.ITuppleModel#setElement(int, java.lang.Object, org.kalypso.ogc.sensor.IAxis)
   */
  public void setElement( int index, Object element, IAxis axis ) throws SensorException
  {
    switch( getPositionFor( axis ) )
    {
    case 0: // TODO: darf das Datum überhaupt geändert werden???
      m_data[index].setTimestamp( (Date)element );
    case 1:
      setValue( index, (Double)element );
    case 2:
      m_kalypsoStati[index] = (Integer)element;
    default:
      throw new SensorException( "Position von Achse " + axis + " ist ungültig" );
    }
  }
}