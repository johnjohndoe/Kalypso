package org.kalypso.ogc.sensor.tableview;

import java.util.Collection;

import org.kalypso.ogc.sensor.template.ITemplateEventProvider;


/**
 * A template that delivers ITableViewColumn information.
 * 
 * @author schlienger
 */
public interface ITableViewTemplate extends ITemplateEventProvider
{
  /**
   * @return list of <code>ITableViewColumn</code>
   */
  public Collection getColumns();

  /**
   * @return list of <code>ITableViewTheme</code>
   */
  public Collection getThemes();

  /**
   * @return list of rules or null if not available
   */
  public ITableViewRules getRules();
  
  /**
   * Clean all possible resources before object is thrown away
   */
  public void dispose();
}
