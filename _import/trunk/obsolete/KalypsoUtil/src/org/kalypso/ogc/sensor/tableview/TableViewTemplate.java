package org.kalypso.ogc.sensor.tableview;

import java.util.ArrayList;
import java.util.List;

import org.kalypso.ogc.sensor.tableview.rules.RenderingRule;
import org.kalypso.ogc.sensor.tableview.rules.Rules;
import org.kalypso.ogc.sensor.template.AbstractTemplateEventProvider;
import org.kalypso.ogc.sensor.template.TemplateEvent;

/**
 * @author schlienger
 */
public class TableViewTemplate extends AbstractTemplateEventProvider implements ITableViewTemplate
{
  private final Rules m_rules = new Rules();

  private final List m_columns = new ArrayList();

  /**
   * @see org.kalypso.ogc.sensor.tableview.ITableViewTemplate#getColumns()
   */
  public ITableViewColumn[] getColumns()
  {
    return (ITableViewColumn[])m_columns.toArray( new ITableViewColumn[0] );
  }

  /**
   * @see org.kalypso.ogc.sensor.tableview.ITableViewTemplate#addColumn(org.kalypso.ogc.sensor.tableview.ITableViewColumn)
   */
  public void addColumn( ITableViewColumn column )
  {
    m_columns.add( column );
    
    fireTemplateChanged( new TemplateEvent( this, column, TemplateEvent.TYPE_ADD ) );
  }

  /**
   * @see org.kalypso.ogc.sensor.tableview.ITableViewTemplate#removeColumn(org.kalypso.ogc.sensor.tableview.ITableViewColumn)
   */
  public void removeColumn( ITableViewColumn column )
  {
    m_columns.remove( column );
    
    fireTemplateChanged( new TemplateEvent( this, column, TemplateEvent.TYPE_REMOVE ) );
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
   * @see org.kalypso.ogc.sensor.tableview.ITableViewTemplate#removeAllColumns()
   */
  public void removeAllColumns()
  {
    fireTemplateChanged( new TemplateEvent( this, null, TemplateEvent.TYPE_REMOVE_ALL ) );
  }
}