package org.kalypso.ogc.sensor.impl;

import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.IObservationListener;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.MetadataList;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.util.runtime.IVariableArguments;
import org.kalypso.util.xml.xlink.IXlink;

/**
 * AbstractObservationDecorator decorates an IObservation. Decorates all the
 * methods of IObservation and delegates the calls to the underlying
 * observation.
 * <p>
 * This class is used in filter and proxy as a base class due to its common
 * functionality.
 * 
 * @author schlienger
 */
public class AbstractObservationDecorator implements IObservation
{
  protected final IObservation m_obs;

  /**
   * Constructor with base observation
   * 
   * @param obs
   */
  public AbstractObservationDecorator( final IObservation obs )
  {
    m_obs = obs;
  }

  public void addListener( IObservationListener listener )
  {
    m_obs.addListener( listener );
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

  public ITuppleModel getValues( IVariableArguments args )
      throws SensorException
  {
    return m_obs.getValues( args );
  }

  public int hashCode( )
  {
    return m_obs.hashCode();
  }

  public boolean isEditable( )
  {
    return m_obs.isEditable();
  }

  public void removeListener( IObservationListener listener )
  {
    m_obs.removeListener( listener );
  }

  public void setValues( ITuppleModel values ) throws SensorException
  {
    m_obs.setValues( values );
  }

  public String toString( )
  {
    return m_obs.toString();
  }

  public String getHref( )
  {
    return m_obs.getHref();
  }
}