package org.kalypso.util.observer;

/**
 * Das Observer-Interface f�r die ObserverRegistry. Alle Listener auf
 * der Registry m�ssen dieses Interface implementieren.
 * 
 * @author belger
 */
public interface IObserver
{
  public void onPublisherChanged( final Object publisher, final Object hint );
}
