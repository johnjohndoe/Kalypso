package org.kalypso.psiadapter.repository;

import java.util.Arrays;
import java.util.Date;
import java.util.NoSuchElementException;

import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.status.KalypsoStatusUtils;
import org.kalypso.psiadapter.util.ArchiveDataDateComparator;

import de.psi.go.lhwz.PSICompact.ArchiveData;

/**
 * Adapter von ArchiveData für ITupple.
 * 
 * @author schlienger
 */
public class PSICompactTuppleModel implements ITuppleModel
{
  private final ArchiveData[] m_data;

  private final Double[] m_values;

//  private final String[] m_psiStati;

  private final Integer[] m_kalypsoStati;

  private final IAxis[] m_axes;

  /**
   * Constructor with ArchiveData[]
   */
  public PSICompactTuppleModel( final ArchiveData[] data, final IAxis[] axes )
  {
    m_axes = axes;
    m_data = data;
    
    m_values = new Double[m_data.length];
//    m_psiStati = new String[m_data.length];
    m_kalypsoStati = new Integer[m_data.length];
  }

  /**
   * Create a new model based on an existing one.
   * 
   * @throws NoSuchElementException when axis was not found
   * @throws SensorException
   */
  public static PSICompactTuppleModel copyModel( final ITuppleModel model ) throws NoSuchElementException, SensorException
  {
    final IAxis[] axes = model.getAxisList();
    
    final IAxis dateAxis = ObservationUtilities.findAxisByClass( axes, Date.class )[0];
    final IAxis valueAxis = ObservationUtilities.findAxisByClass( axes, Number.class )[0];
    final IAxis statusAxis = KalypsoStatusUtils.findStatusAxis( axes );
  
    final ArchiveData[] data = constructData( model, dateAxis, valueAxis, statusAxis );
    
    return new PSICompactTuppleModel( data, new IAxis[] { dateAxis, valueAxis, statusAxis } );
  }

  /**
   * Helper that creates ArchiveData[] having a ITuppleModel
   * 
   * @throws SensorException
   */
  private final static ArchiveData[] constructData( final ITuppleModel model, final IAxis dateAxis, final IAxis valueAxis, final IAxis statusAxis ) throws SensorException
  {
    final ArchiveData[] data = new ArchiveData[model.getCount()];

    for( int i = 0; i < data.length; i++ )
    {
      data[i] = new ArchiveData( (Date)model.getElement( i, dateAxis ),
              PSICompactFactory.maskToPsiStatus( ((Number)model.getElement( i, statusAxis )).intValue() )
              , ( (Number)model.getElement( i, valueAxis ) )
              .doubleValue() );
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
      m_values[index] = new Double( m_data[index].getValue() );

    return m_values[index];
  }

//  private String getStatus( int index )
//  {
//    if( m_psiStati[index] == null )
//      m_psiStati[index] = PSICompactFactory.statusToString( m_data[index].getStatus() );
//
//    return m_psiStati[index];
//  }
//
  private Integer getKalypsoStatus( int index )
  {
    if( m_kalypsoStati[index] == null )
      m_kalypsoStati[index] = PSICompactFactory.psiStatusToMask( m_data[index].getStatus() );

    return m_kalypsoStati[index];
  }
//
//  private void setStatus( int index, String status )
//  {
//    m_psiStati[index] = status;
//
//    m_data[index].setStatus( Integer.valueOf( status ).intValue() );
//  }

  private void setValue( int index, Double value )
  {
    m_values[index] = value;

    m_data[index].setValue( value.doubleValue() );
  }

  /**
   * @see org.kalypso.ogc.sensor.ITuppleModel#getCount()
   */
  public int getCount()
  {
    return m_data.length;
  }

  /**
   * @see org.kalypso.ogc.sensor.ITuppleModel#indexOf(java.lang.Object,
   *      org.kalypso.ogc.sensor.IAxis)
   */
  public int indexOf( Object element, IAxis axis )
  {
    // wir gehen davon aus dass m_data sortiert ist! Sollte eigentlich der Fall
    // sein da es sich um eine Zeitreihe handelt.

    return Arrays.binarySearch( m_data, element, new ArchiveDataDateComparator() );
  }

  /**
   * @see org.kalypso.ogc.sensor.ITuppleModel#getAxisList()
   */
  public IAxis[] getAxisList()
  {
    return m_axes;
  }

  /**
   * @see org.kalypso.ogc.sensor.ITuppleModel#getElement(int, org.kalypso.ogc.sensor.IAxis)
   */
  public Object getElement( int index, IAxis axis )
  {
    switch( axis.getPosition() )
    {
    case 0:
      return m_data[index].getTimestamp();
    case 1:
      return getValue( index );
//    case 2:
//      return getStatus( index );
    case 2:
      return getKalypsoStatus( index );
    default:
      throw new IllegalArgumentException( "Position von Axis " + axis + " ist ungültig" );
    }
  }

  /**
   * @see org.kalypso.ogc.sensor.ITuppleModel#setElement(int, java.lang.Object, org.kalypso.ogc.sensor.IAxis)
   */
  public void setElement( int index, Object element, IAxis axis )
  {
    switch( axis.getPosition() )
    {
    case 0:
      m_data[index].setTimestamp( (Date)element );
    case 1:
      setValue( index, (Double)element );
//    case 2:
//      setStatus( index, (String)element );
    case 2:
      m_kalypsoStati[index] = (Integer)element;
    default:
      throw new IllegalArgumentException( "Position von Achse " + axis + " ist ungültig" );
    }
  }
}