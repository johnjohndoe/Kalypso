package org.kalypso.ogc.sensor.tableview;

import org.kalypso.ogc.sensor.template.ITemplateEventProvider;


/**
 * A template that delivers ITableViewColumn information.
 * 
 * @author schlienger
 */
public interface ITableViewTemplate extends ITableViewRules, ITemplateEventProvider
{
  /**
   * Returns the list of columns. At the time this method is called, it might
   * be that not all columns are loaded. Clients should register to the template
   * as <code>ITemplateListener</code> and get the columns when <code>onTemplateLoaded()</code>
   * is called.
   * 
   * @return list of columns defined in this template
   */
  public ITableViewColumn[] getColumns();
  
  /**
   * Adds the given column
   * 
   * @param column
   */
  public void addColumn( ITableViewColumn column );
  
  /**
   * Remove given column
   * 
   * @param column
   */
  public void removeColumn( ITableViewColumn column );
  
  /**
   * Removes all column
   */
  public void removeAllColumns();

  /**
   * Clean all possible resources before object is thrown away
   */
  public void dispose();
}
