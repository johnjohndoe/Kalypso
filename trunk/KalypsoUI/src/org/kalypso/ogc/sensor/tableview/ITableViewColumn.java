package org.kalypso.ogc.sensor.tableview;

import org.kalypso.eclipse.ui.IViewable;
import org.kalypso.ogc.sensor.IAxis;


/**
 * A column of a tableview
 * 
 * @author schlienger
 */
public interface ITableViewColumn extends IViewable
{
  public String getName( );
  public void setName( final String name );

  public boolean isEditable( );

  public void setShown( boolean shown );
  
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
   * @return the observation theme on which this column is based
   */
  public ITableViewTheme getTheme();  
}