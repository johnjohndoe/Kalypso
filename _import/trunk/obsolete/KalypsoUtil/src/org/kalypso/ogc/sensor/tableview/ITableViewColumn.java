package org.kalypso.ogc.sensor.tableview;

import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;

/**
 * A column of a tableview over IObservation.
 * 
 * @author schlienger
 */
public interface ITableViewColumn
{
  public String getName();
  
  public boolean isEditable();
  
  public int getWidth();
  public void setWidth( final int width );
  
  public IObservation getObservation();
  
  public IAxis getAxis();
}
