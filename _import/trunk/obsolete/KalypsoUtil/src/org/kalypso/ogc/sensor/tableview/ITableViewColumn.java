package org.kalypso.ogc.sensor.tableview;

import org.kalypso.ogc.sensor.IObservationProvider;

/**
 * A column of a tableview over IObservation.
 * 
 * @author schlienger
 */
public interface ITableViewColumn extends IObservationProvider
{
  public String getName();
  
  public boolean isEditable();
  
  public int getWidth();
  public void setWidth( final int width );
}
