package org.kalypso.ogc.sensor.tableview;

import org.kalypso.ogc.sensor.IObservation;


/**
 * A column of a tableview
 * 
 * @author schlienger
 */
public interface ITableViewColumn
{
  public String getName( );

  public boolean isEditable( );

  public int getWidth( );

  /**
   * @return the name of the observation axis on which this column is based
   */
  public String getAxisName();
  
  /**
   * @return the observation on which this column is based
   */
  public IObservation getObservation();
  
  /**
   * Sets the column width
   * 
   * @param width
   */
  public void setWidth( final int width );

  /**
   * @return true when data hold in the observation hold by this column has
   *         changed
   */
  public boolean isDirty( );

  /**
   * Sets the dirty flag
   * @param b
   */
  public void setDirty( boolean b );
}