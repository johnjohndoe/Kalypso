package org.kalypso.ogc.sensor.tableview;

import org.kalypso.util.runtime.IVariableArguments;

/**
 * A column of a tableview over IObservation.
 * 
 * @author schlienger
 */
public interface ITableViewColumn extends ITableObservationProvider
{
  public String getName();

  public boolean isEditable();

  public int getWidth();

  public void setWidth( final int width );
  
  public void setArguments( IVariableArguments args );
  public IVariableArguments getArguments();
  
  /**
   * Clean all possible resources before object is thrown away
   */
  public void dispose();
}