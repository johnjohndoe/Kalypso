package org.kalypso.ogc.sensor;

/**
 * IObservationEventProvider
 * 
 * @author schlienger
 */
public interface IObservationEventProvider
{
  public void addListener( final IObservationListener listener );
  
  public void removeListener( final IObservationListener listener );
}
