package org.kalypso.ogc.sensor.tableview;

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
}