package org.kalypso.ogc.sensor.proxy;

import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.SensorException;

/**
 * IProxyFactory
 * 
 * @author schlienger
 */
public interface IProxyFactory
{
  public IObservation proxyObservation( final IObservation obs ) throws SensorException;
}