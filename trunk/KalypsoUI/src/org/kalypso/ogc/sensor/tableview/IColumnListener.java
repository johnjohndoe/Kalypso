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
   * 
   * @param column
   */
  public void columnLoaded( final ITableViewColumn column );
}
