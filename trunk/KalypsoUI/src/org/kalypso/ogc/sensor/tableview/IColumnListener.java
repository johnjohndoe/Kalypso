package org.kalypso.ogc.sensor.tableview;


/**
 * A simple listener for column events like load finished.
 * 
 * @author schlienger
 */
public interface IColumnListener
{
  /**
   * The given column is loaded
   */
  public void columnLoaded( final ITableViewColumn column );
}
