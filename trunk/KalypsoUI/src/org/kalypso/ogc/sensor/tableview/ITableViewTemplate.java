package org.kalypso.ogc.sensor.tableview;

import java.util.List;

import org.kalypso.ogc.sensor.template.ITemplateEventProvider;


/**
 * A template that delivers ITableViewColumn information.
 * 
 * @author schlienger
 */
public interface ITableViewTemplate extends ITableViewRules, ITemplateEventProvider
{
  /**
   * @return list of ITableViewColumn
   */
  public List getColumns();
  
//  /**
//   * Adds the given column
//   * 
//   * @param column
//   */
//  public void addColumn( ITableViewColumn column );
//  
//  /**
//   * Remove given column
//   * 
//   * @param column
//   */
//  public void removeColumn( ITableViewColumn column );
//  
//  /**
//   * Removes all column
//   */
//  public void removeAllColumns();

  /**
   * Clean all possible resources before object is thrown away
   */
  public void dispose();
}
