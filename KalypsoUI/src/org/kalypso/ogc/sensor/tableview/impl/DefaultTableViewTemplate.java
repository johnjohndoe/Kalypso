package org.kalypso.ogc.sensor.tableview.impl;

import java.util.ArrayList;
import java.util.List;

import org.kalypso.ogc.sensor.tableview.ITableViewColumn;
import org.kalypso.ogc.sensor.tableview.ITableViewTemplate;
import org.kalypso.ogc.sensor.tableview.rules.RenderingRule;
import org.kalypso.ogc.sensor.tableview.rules.Rules;
import org.kalypso.ogc.sensor.template.AbstractTemplateEventProvider;
import org.kalypso.ogc.sensor.template.TemplateEvent;

/**
 * Default implementation of the <code>ITableViewColumn</code>.
 * 
 * @author schlienger
 */
public class DefaultTableViewTemplate extends AbstractTemplateEventProvider implements ITableViewTemplate
{
  private final Rules m_rules = new Rules();

  private final List m_columns = new ArrayList();

  /**
   * @see org.kalypso.ogc.sensor.tableview.ITableViewTemplate#getColumns()
   */
  public List getColumns()
  {
    return m_columns;
  }

  public void addColumn( ITableViewColumn column )
  {
    m_columns.add( column );
    
    fireTemplateChanged( new TemplateEvent( this, column, TemplateEvent.TYPE_ADD ) );
  }

  public void removeColumn( ITableViewColumn column )
  {
    m_columns.remove( column );
    
    fireTemplateChanged( new TemplateEvent( this, column, TemplateEvent.TYPE_REMOVE ) );
  }
  
  public void removeAllColumns()
  {
    fireTemplateChanged( new TemplateEvent( this, null, TemplateEvent.TYPE_REMOVE_ALL ) );
  }

  /**
   * @see org.kalypso.ogc.sensor.tableview.ITableViewTemplate#findRules(int)
   */
  public RenderingRule[] findRules( int mask )
  {
    return m_rules.findRules( mask );
  }

  /**
   * @see org.kalypso.ogc.sensor.tableview.ITableViewTemplate#addRule(org.kalypso.ogc.sensor.tableview.rules.RenderingRule)
   */
  public void addRule( RenderingRule rule )
  {
    m_rules.addRule( rule );
  }

  /**
   * @see org.kalypso.ogc.sensor.tableview.ITableViewTemplate#removeRule(org.kalypso.ogc.sensor.tableview.rules.RenderingRule)
   */
  public void removeRule( RenderingRule rule )
  {
    m_rules.removeRule( rule );
  }

  /**
   * @see org.kalypso.ogc.sensor.tableview.ITableViewTemplate#dispose()
   */
  public void dispose( )
  {
    m_columns.clear();
  }
}