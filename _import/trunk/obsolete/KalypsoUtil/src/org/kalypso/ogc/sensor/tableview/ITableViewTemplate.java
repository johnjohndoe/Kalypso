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
   */
  public ITableViewColumn[] getColumns();
  public void addColumn( ITableViewColumn column );
  public void removeColumn( ITableViewColumn column );
  
  public void removeAllColumns();
}
