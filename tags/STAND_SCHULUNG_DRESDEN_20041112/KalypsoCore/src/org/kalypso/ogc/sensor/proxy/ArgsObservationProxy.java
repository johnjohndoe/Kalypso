package org.kalypso.ogc.sensor.proxy;

import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.MetadataList;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.util.runtime.IVariableArguments;
import org.kalypso.util.xml.xlink.IXlink;

/**
 * ArgsObservationProxy
 * 
 * @author schlienger
 */
public class ArgsObservationProxy implements IObservationProxy
{
  private IObservation m_obs;
  private IVariableArguments m_args;

  public ArgsObservationProxy( final IVariableArguments args, final IObservation obs )
  {
    m_obs = obs;
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
  
  public boolean equals( Object obj )
  {
    return m_obs.equals( obj );
  }
  
  public IAxis[] getAxisList( )
  {
    return m_obs.getAxisList();
  }

  public String getIdentifier( )
  {
    return m_obs.getIdentifier();
  }

  public MetadataList getMetadataList( )
  {
    return m_obs.getMetadataList();
  }

  public String getName( )
  {
    return m_obs.getName();
  }

  public IXlink getTarget( )
  {
    return m_obs.getTarget();
  }

  public int hashCode( )
  {
    return m_obs.hashCode();
  }

  public boolean isEditable( )
  {
    return m_obs.isEditable();
  }

  public void setValues( ITuppleModel values ) throws SensorException
  {
    m_obs.setValues( values );
  }

  public String toString( )
  {
    return m_obs.toString();
  }
}
