package org.kalypso.psiadapter;

import java.util.Arrays;
import java.util.Date;

import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.psiadapter.util.ArchiveDataDateComparator;
import org.kalypso.util.status.MaskedNumber;

import de.psi.go.lhwz.PSICompact.ArchiveData;

/**
 * Adapter von ArchiveData für ITupple.
 * 
 * @author schlienger
 */
public class PSICompactTuppleModel implements ITuppleModel
{
  private final ArchiveData[] m_data;
  private final MaskedNumber[] m_values;
  private final String[] m_statuses;

  /**
   * Constructor with ArchiveData[]
   */
  public PSICompactTuppleModel( final ArchiveData[] data )
  {
    m_data = data;
    m_values = new MaskedNumber[m_data.length];
    m_statuses = new String[m_data.length];
  }
  
  /**
   * Constructor with another ITuppleModel
   */
  public PSICompactTuppleModel( final ITuppleModel model )
  {
    this( constructData( model ) );
  }
  
  /**
   * Helper that creates ArchiveData[] having a ITuppleModel
   */
  private final static ArchiveData[] constructData( ITuppleModel model )
  {
    ArchiveData[] data = new ArchiveData[ model.getCount() ];
    
    for( int i = 0; i < data.length; i++ )
    {
      data[i] = new ArchiveData( (Date)model.getElement(i, PSICompactAxis.TYPE_DATE),
          PSICompactFactory.statusTranslate( model.getElement(i, PSICompactAxis.TYPE_STATUS).toString() ),
          ((Double)model.getElement(i, PSICompactAxis.TYPE_VALUE)).doubleValue() );
    }
    
    return data;
  }

  public ArchiveData[] getData()
  {
    return m_data;
  }

  private MaskedNumber getValue( int index )
  {
    if( m_values[ index ] == null )
      m_values[ index ] = new MaskedNumber( m_data[index].getValue() );

    return m_values[ index ];
  }

  private String getStatus( int index )
  {
    if( m_statuses[ index ]== null )
      m_statuses[ index ] = PSICompactFactory.statusToString( m_data[index].getStatus() );

    return m_statuses[ index ];
  }

  private void setStatus( int index, String status )
  {
    m_statuses[ index ] = status;
    
    m_data[index].setStatus( Integer.valueOf( status ).intValue() );
  }

  private void setValue( int index, MaskedNumber value )
  {
    m_values[ index ] = value;
    
    m_data[index].setValue( value.doubleValue() );
  }
  
  /**
   * @see org.kalypso.ogc.sensor.ITuppleModel#getElement(int, int)
   */
  public Object getElement( int index, int position )
  {
    switch( position )
    {
    case 0:
      return m_data[index].getTimestamp();
    case 1:
      return getValue(index);
    case 2:
      return getStatus(index);
    default:
      throw new IllegalArgumentException( "Position " + position + " ist ungültig" );
    }
  }

  /**
   * @see org.kalypso.ogc.sensor.ITuppleModel#setElement(int, java.lang.Object, int)
   */
  public void setElement( int index, Object element, int position )
  {
    switch( position )
    {
    case 0:
      m_data[index].setTimestamp( (Date)element );
    case 1:
      setValue( index, (MaskedNumber)element );
    case 2:
      setStatus( index, (String)element );
    default:
      throw new IllegalArgumentException( "Position " + position + " ist ungültig" );
    }
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
  public int indexOf( Object element, IAxis axis )
  {
    // wir gehen davon aus dass m_data sortiert ist! Sollte eigentlich der Fall
    // sein da es sich um eine Zeitreihe handelt.
    
    return Arrays.binarySearch( m_data, element, new ArchiveDataDateComparator() );
  } 
}
