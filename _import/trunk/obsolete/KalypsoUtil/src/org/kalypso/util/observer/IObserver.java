package org.kalypso.util.observer;

/**
 * Das Observer-Interface für die ObserverRegistry. Alle Listener auf
 * der Registry müssen dieses Interface implementieren.
 * 
 * @author belger
 */
public interface IObserver
{
  public void onPublisherChanged( final Object publisher, final Object hint );
}
