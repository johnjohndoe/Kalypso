package org.kalypso.ogc.sensor.tableview;

import org.kalypso.ogc.sensor.tableview.template.Rules;
import org.kalypso.ogc.sensor.template.ITemplateAdapter;


/**
 * A template that delivers ITableViewColumn information.
 * 
 * @author schlienger
 */
public interface ITableViewTemplate extends ITemplateAdapter
{
  /**
   * Returns the list of columns. At the time this method is called, it might
   * be that not all columns are loaded. Clients should register to the template
   * as <code>ITemplateListener</code> and get the columns when <code>onTemplateLoaded()</code>
   * is called.
   */
  public ITableViewColumn[] getColumns();
  
  public Rules getRules();
}
