package org.kalypso.ogc.sensor.zml.values;

import org.kalypso.ogc.sensor.SensorException;


/**
 * Used for zml values loading.
 * 
 * @author schlienger
 */
public interface IZmlValuesLoader
{
  public void setModel( ZmlTuppleModel model );
  public IZmlValuesProvider load( ) throws SensorException;
}
