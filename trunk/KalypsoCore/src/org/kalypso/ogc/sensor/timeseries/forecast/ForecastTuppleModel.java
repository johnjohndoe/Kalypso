package org.kalypso.ogc.sensor.timeseries.forecast;

import java.util.Date;

import org.kalypso.java.util.DateUtilities;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.impl.SimpleTuppleModel;

/**
 * MergeTuppleModel
 * 
 * @author schlienger
 */
public class ForecastTuppleModel implements ITuppleModel
{
  private SimpleTuppleModel m_model;

  /**
   * Constructor
   * 
   * @param models
   * 
   * @throws SensorException
   */
  public ForecastTuppleModel( final ITuppleModel[] models ) throws SensorException
  {
    Date lastDate = DateUtilities.getMinimum();
    
    m_model = new SimpleTuppleModel( models[0].getAxisList() );

    for( int i = 0; i < models.length; i++ )
    {
      final IAxis[] axes = models[i].getAxisList();
      final IAxis[] dateAxes = ObservationUtilities.findAxisByClass( axes, Date.class );
      if( dateAxes.length == 0 )
        throw new IllegalArgumentException("no date axis");
      
      final IAxis dateAxis = dateAxes[0];
      
      for( int rowIx = 0; rowIx < models[i].getCount(); rowIx++ )
      {
        final Date date = (Date) models[i].getElement( rowIx, dateAxis );

        if( date.compareTo( lastDate ) > 0 )
        {
          final Object[] tupple = new Object[ axes.length ];
          
          for( int colIx = 0; colIx < axes.length; colIx++ )
            tupple[ axes[colIx].getPosition() ] = models[i].getElement( rowIx, axes[colIx]);
          
          m_model.addTupple( tupple );
        }
 
        lastDate = date;
      }
    }
  }

  /**
   * @see org.kalypso.ogc.sensor.ITuppleModel#getAxisList()
   */
  public IAxis[] getAxisList( )
  {
    return m_model.getAxisList();
  }

  /**
   * @see org.kalypso.ogc.sensor.ITuppleModel#getCount()
   */
  public int getCount( ) throws SensorException
  {
    return m_model.getCount();
  }

  /**
   * @see org.kalypso.ogc.sensor.ITuppleModel#getElement(int, org.kalypso.ogc.sensor.IAxis)
   */
  public Object getElement( int index, IAxis axis ) throws SensorException
  {
    return m_model.getElement( index, axis );
  }

  /**
   * @see org.kalypso.ogc.sensor.ITuppleModel#setElement(int, java.lang.Object, org.kalypso.ogc.sensor.IAxis)
   */
  public void setElement( int index, Object element, IAxis axis )
      throws SensorException
  {
    m_model.setElement( index, element, axis );
  }

  /**
   * @see org.kalypso.ogc.sensor.ITuppleModel#indexOf(java.lang.Object, org.kalypso.ogc.sensor.IAxis)
   */
  public int indexOf( Object element, IAxis axis ) throws SensorException
  {
    return m_model.indexOf( element, axis );
  }
}
