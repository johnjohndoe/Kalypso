package org.kalypso.ogc.sensor.tableview;

import org.kalypso.ogc.sensor.IAxis;
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
  public void setWidth( final int width );

  /**
   * @return true when data hold in the observation hold by this column has
   *         changed
   */
  public boolean isDirty( );
  public void setDirty( boolean dirty );
  
  /**
   * @return the class of the values in this column
   */
  public Class getColumnClass();
  
  /**
   * @return the value axis for which this column displays values
   */
  public IAxis getAxis();
  
  /**
   * @return the key axis of the underyling observation
   */
  public IAxis getKeyAxis();
  
  /**
   * @return the observation on which this column is based
   */
  public IObservation getObservation();
}