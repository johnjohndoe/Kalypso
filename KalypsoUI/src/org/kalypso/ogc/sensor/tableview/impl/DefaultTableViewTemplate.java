package org.kalypso.ogc.sensor.tableview.impl;

import java.util.Collection;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.Map;

import org.kalypso.ogc.sensor.tableview.ITableViewTemplate;
import org.kalypso.ogc.sensor.tableview.ITableViewTheme;
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

  private final Map m_themesMap = new Hashtable();

  /**
   * @see org.kalypso.ogc.sensor.tableview.ITableViewTemplate#getColumns()
   */
  public Collection getColumns()
  {
    return m_themesMap.values();
  }

  /**
   * Adds a theme.
   * 
   * @param theme
   */
  public void addTheme( ITableViewTheme theme )
  {
    m_themesMap.put( theme, theme.getColumns() );
    
    final Iterator it = theme.getColumns().iterator();
    while( it.hasNext() )
      fireTemplateChanged( new TemplateEvent( this, it.next(), TemplateEvent.TYPE_ADD) );
  }
  
  /**
   * Removes a theme.
   * 
   * @param theme
   */
  public void removeTheme( ITableViewTheme theme )
  {
    final Iterator it = theme.getColumns().iterator();
    while( it.hasNext() )
      fireTemplateChanged( new TemplateEvent( this, it.next(), TemplateEvent.TYPE_REMOVE ) );
    
    m_themesMap.remove( theme );
  }
  
  /**
   * Removes all the themes and fires event
   */
  public void removeAllThemes()
  {
    m_themesMap.clear();
    
    fireTemplateChanged( new TemplateEvent( this, null, TemplateEvent.TYPE_REMOVE_ALL) );
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
    final Iterator it = m_themesMap.keySet().iterator();
    while( it.hasNext() )
      ((ITableViewTheme) it.next()).dispose();
    
    m_themesMap.clear();
  }

  /**
   * @see org.kalypso.ogc.sensor.tableview.ITableViewTemplate#getThemes()
   */
  public Collection getThemes( )
  {
    return m_themesMap.keySet();
  }
}