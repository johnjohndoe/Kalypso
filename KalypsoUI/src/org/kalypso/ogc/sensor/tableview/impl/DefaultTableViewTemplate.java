package org.kalypso.ogc.sensor.tableview.impl;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.Map;

import org.kalypso.ogc.sensor.tableview.ITableViewRules;
import org.kalypso.ogc.sensor.tableview.ITableViewTemplate;
import org.kalypso.ogc.sensor.tableview.ITableViewTheme;
import org.kalypso.ogc.sensor.tableview.rules.RulesFactory;
import org.kalypso.ogc.sensor.template.AbstractTemplateEventProvider;
import org.kalypso.ogc.sensor.template.TemplateEvent;

/**
 * Default implementation of the <code>ITableViewColumn</code>.
 * 
 * @author schlienger
 */
public class DefaultTableViewTemplate extends AbstractTemplateEventProvider implements ITableViewTemplate
{
  private final ITableViewRules m_rules = RulesFactory.getDefaultRules();

  private final Map m_themesMap = new Hashtable();

  /**
   * @see org.kalypso.ogc.sensor.tableview.ITableViewTemplate#getColumns()
   */
  public Collection getColumns()
  {
    final Collection allCurves = new ArrayList();
    for( final Iterator iter = m_themesMap.values().iterator(); iter.hasNext(); )
    {
      final Collection curves = (Collection) iter.next();
      allCurves.addAll( curves );
    }
    
//    return m_themesMap.values();
    return allCurves;
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
    fireTemplateChanged( new TemplateEvent( this, theme, TemplateEvent.TYPE_REMOVE ) );
    
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

  /**
   * @see org.kalypso.ogc.sensor.tableview.ITableViewTemplate#getRules()
   */
  public ITableViewRules getRules( )
  {
    return m_rules;
  }
}