package org.kalypso.ogc.sensor.template;

import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.request.IRequest;

/**
 * @author belger
 */
public interface IObsProvider
{
  public IObservation getObservation();

  public IRequest getArguments();

  public void addListener( final IObsProviderListener l );

  public void removeListener( final IObsProviderListener l );

  public void dispose();

  public boolean isLoading();

  /** Clones this object, that is returns a provider of the same observation */
  public IObsProvider copy();
}
