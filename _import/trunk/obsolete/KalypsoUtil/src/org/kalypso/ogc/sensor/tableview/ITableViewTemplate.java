package org.kalypso.ogc.sensor.tableview;

import org.kalypso.ogc.sensor.tableview.template.Rules;


/**
 * A template that delivers ITableViewColumn information.
 * 
 * @author schlienger
 */
public interface ITableViewTemplate
{
  public ITableViewColumn[] getColumns();
  
  public Rules getRules();
}
