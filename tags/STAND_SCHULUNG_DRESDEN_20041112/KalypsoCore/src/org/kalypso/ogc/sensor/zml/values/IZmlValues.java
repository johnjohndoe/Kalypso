package org.kalypso.ogc.sensor.zml.values;

import org.kalypso.ogc.sensor.SensorException;

/**
 * @author schlienger
 */
public interface IZmlValues
{
  public Object getElement( final int index ) throws SensorException;
  
  public void setElement( final int index, final Object element ) throws SensorException;
  
  public int getCount() throws SensorException;
  
  public int indexOf( final Object element ) throws SensorException;
}
