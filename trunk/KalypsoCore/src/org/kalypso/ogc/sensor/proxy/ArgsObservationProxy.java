package org.kalypso.ogc.sensor.proxy;

import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.impl.AbstractObservationDecorator;
import org.kalypso.util.runtime.IVariableArguments;

/**
 * ArgsObservationProxy
 * 
 * @author schlienger
 */
public class ArgsObservationProxy extends AbstractObservationDecorator implements IObservationProxy
{
  private IVariableArguments m_args;

  public ArgsObservationProxy( final IVariableArguments args, final IObservation obs )
  {
    super( obs );
    
    m_args = args;
  }
  
  /**
   * @see org.kalypso.ogc.sensor.IObservation#getValues(org.kalypso.util.runtime.IVariableArguments)
   */
  public ITuppleModel getValues( IVariableArguments args )
      throws SensorException
  {
    if( args == null )
      args = m_args;
    
    return m_obs.getValues( args );
  }
}
