package org.kalypso.ogc.sensor.tableview;

import org.kalypso.ogc.sensor.tableview.rules.RenderingRule;

/**
 * @author schlienger
 */
public interface ITableViewRules
{
  public void addRule( RenderingRule rule );
  public void removeRule( RenderingRule rule );
  public RenderingRule[] findRules( int mask );
}
