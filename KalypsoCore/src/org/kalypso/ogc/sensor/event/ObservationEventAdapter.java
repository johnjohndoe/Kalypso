package org.kalypso.ogc.sensor.event;

import java.util.ArrayList;
import java.util.List;

import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.IObservationEventProvider;
import org.kalypso.ogc.sensor.IObservationListener;

/**
 * ObservationEventAdapter
 * 
 * @author schlienger
 */
public class ObservationEventAdapter implements IObservationEventProvider
{
  private final List m_listeners = new ArrayList();
  private final IObservation m_obs;

  public ObservationEventAdapter( final IObservation obs )
  {
    m_obs = obs;
  }
  
  /**
   * @see org.kalypso.ogc.sensor.IObservationEventProvider#addListener(org.kalypso.ogc.sensor.IObservationListener)
   */
  public void addListener( final IObservationListener listener )
  {
    m_listeners.add( listener );
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservationEventProvider#removeListener(org.kalypso.ogc.sensor.IObservationListener)
   */
  public void removeListener( final IObservationListener listener )
  {
    m_listeners.remove( listener );
  }

  /**
   * Fires obs changed event
   */
  public void fireChangedEvent( )
  {
    final Object[] listeners = m_listeners.toArray();
    for( int i = 0; i < listeners.length; i++ )
    {
      final IObservationListener listener = (IObservationListener) listeners[i];
      listener.observationChanged( m_obs );
    }
  }
}
