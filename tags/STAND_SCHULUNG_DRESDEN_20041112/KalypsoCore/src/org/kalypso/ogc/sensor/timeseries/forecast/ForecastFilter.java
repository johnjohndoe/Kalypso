package org.kalypso.ogc.sensor.timeseries.forecast;

import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.filter.filters.AbstractObservationFilter;
import org.kalypso.util.runtime.IVariableArguments;

/**
 * MergeFilter
 * 
 * @author schlienger
 */
public class ForecastFilter extends AbstractObservationFilter
{
  private IObservation[] m_obsArray = null;
  
  /**
   * @see org.kalypso.ogc.sensor.filter.filters.AbstractObservationFilter#initFilter(java.lang.Object, org.kalypso.ogc.sensor.IObservation)
   */
  public void initFilter( Object conf, IObservation obs )
      throws SensorException
  {
    super.initFilter( conf, obs );
    
    m_obsArray = (IObservation[]) conf;
  }
  
  /**
   * @see org.kalypso.ogc.sensor.filter.filters.AbstractObservationFilter#getValues(org.kalypso.util.runtime.IVariableArguments)
   */
  public ITuppleModel getValues( IVariableArguments args )
      throws SensorException
  {
    final ITuppleModel models[] = new ITuppleModel[m_obsArray.length];
    
    for( int i = 0; i < models.length; i++ )
      models[i] = m_obsArray[i].getValues( args );
    
    return new ForecastTuppleModel( models );
  }
}
