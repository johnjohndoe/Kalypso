package org.kalypso.ogc.sensor.filter.filters;

import java.util.HashMap;
import java.util.Map;

import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.status.KalypsoStatusUtils;
import org.kalypso.util.runtime.IVariableArguments;

/**
 * DataHoleFilter sets the status of elements that equals the marker-value.
 * TODO: This filter only works if there already is a status axis. It does not
 * add one on its own.
 * 
 * @author schlienger
 */
public class DataHoleFilter extends AbstractObservationFilter
{
  private final Double m_value;

  private final Integer m_status;

  private final Double m_replace;

  private final Map m_map = new HashMap();

  public DataHoleFilter( final double value, final int status,
      final Double replace )
  {
    m_value = new Double( value );
    m_status = new Integer( status );
    m_replace = replace;
  }

  /**
   * @see org.kalypso.ogc.sensor.filter.filters.AbstractObservationFilter#getValues(org.kalypso.util.runtime.IVariableArguments)
   */
  public ITuppleModel getValues( final IVariableArguments args )
      throws SensorException
  {
    final ITuppleModel values = super.getValues( args );
    final IAxis[] valueAxes = ObservationUtilities.findAxisByClass( values
        .getAxisList(), Number.class, true );

    for( int index = 0; index < values.getCount(); index++ )
    {
      for( int ia = 0; ia < valueAxes.length; ia++ )
      {
        if( m_value.equals( values.getElement( index, valueAxes[ia] ) ) )
        {
          final IAxis sa = getStatusAxisFor( valueAxes[ia], values.getAxisList() );
          if( sa != null )
          {
            // update the status
            values.setElement( index, m_status, sa );
            
            // and replace value
            if( m_replace != null )
              values.setElement( index, m_replace, valueAxes[ia]);
          }
        }
      }
    }

    return values;
  }

  /**
   * Helper
   * 
   * @param axis
   * @param axes
   * @return status axis or null if none
   */
  private IAxis getStatusAxisFor( final IAxis axis, final IAxis[] axes )
  {
    if( m_map.containsKey( axis ) )
      return (IAxis) m_map.get( axis );

    final String label = KalypsoStatusUtils.getStatusAxisLabelFor( axis );
    final IAxis statusAxis = ObservationUtilities.findAxisByNameNoEx( axes,
        label );

    m_map.put( axis, statusAxis );
    
    return statusAxis;
  }
}