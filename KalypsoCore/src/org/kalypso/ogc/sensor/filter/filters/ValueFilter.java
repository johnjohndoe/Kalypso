package org.kalypso.ogc.sensor.filter.filters;

import java.util.List;

import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.filter.filters.valuecomp.IValueComp;
import org.kalypso.util.runtime.IVariableArguments;

/**
 * ValueFilter
 * 
 * @author schlienger
 */
public class ValueFilter extends AbstractObservationFilter
{
  private IValueComp[] m_comps;

  /**
   * @see org.kalypso.ogc.sensor.filter.filters.AbstractObservationFilter#initFilter(java.lang.Object,
   *      org.kalypso.ogc.sensor.IObservation)
   */
  public void initFilter( Object conf, IObservation obs ) throws SensorException
  {
    super.initFilter( conf, obs );
    
    m_comps = (IValueComp[]) ((List)conf).toArray( new IValueComp[0]);
  }
  
  /**
   * @see org.kalypso.ogc.sensor.filter.filters.AbstractObservationFilter#getValues(org.kalypso.util.runtime.IVariableArguments)
   */
  public ITuppleModel getValues( IVariableArguments args ) throws SensorException
  {
    ITuppleModel values = super.getValues( args );
    
    for( int i = 0; i < values.getCount(); i++ )
    {
      
    }
    
    
    return values;
  }
}