package org.kalypso.ogc.sensor.tableview.template;

import java.net.URL;
import java.util.Iterator;
import java.util.List;

import org.eclipse.core.runtime.jobs.ISchedulingRule;
import org.kalypso.eclipse.core.runtime.jobs.MutexSchedulingRule;
import org.kalypso.ogc.sensor.tableview.ITableViewColumn;
import org.kalypso.ogc.sensor.tableview.ITableViewTemplate;
import org.kalypso.ogc.sensor.tableview.impl.DefaultTableViewTemplate;
import org.kalypso.ogc.sensor.tableview.rules.RenderingRule;
import org.kalypso.ogc.sensor.template.ITemplateEventListener;
import org.kalypso.ogc.sensor.template.TemplateEvent;
import org.kalypso.template.obstableview.ObstableviewType;
import org.kalypso.template.obstableview.TypeRenderingRule;
import org.kalypso.template.obstableview.ObstableviewType.RulesType;

/**
 * A DefaultTableViewTemplate based on a XML template file. Performs the load
 * operation.
 * 
 * @author schlienger
 */
public class LinkedTableViewTemplate implements ITableViewTemplate
{
  private final DefaultTableViewTemplate m_template;

  private final ISchedulingRule m_rule = new MutexSchedulingRule();

  /**
   * Constructor
   * 
   * @param obsTableView
   * @param context
   */
  public LinkedTableViewTemplate( final ObstableviewType obsTableView,
      final URL context )
  {
    m_template = new DefaultTableViewTemplate();

    final List cols = obsTableView.getColumnpair();
    for( final Iterator it = cols.iterator(); it.hasNext(); )
    {
      ObstableviewType.ColumnpairType col = (ObstableviewType.ColumnpairType) it
          .next();

      new LinkedTableViewColumn( this, col, context );
    }

    final RulesType trules = obsTableView.getRules();
    if( trules != null )
    {
      for( final Iterator it = trules.getRenderingrule().iterator(); it
          .hasNext(); )
        m_template.addRule( RenderingRule
            .createRenderingRule( (TypeRenderingRule) it.next() ) );
    }
  }
  
  public void dispose()
  {
    m_template.dispose();
  }

  public void addColumn( final ITableViewColumn column )
  {
    m_template.addColumn( column );
  }

  public void addTemplateEventListener( ITemplateEventListener l )
  {
    m_template.addTemplateEventListener( l );
  }

  public void fireTemplateChanged( TemplateEvent evt )
  {
    m_template.fireTemplateChanged( evt );
  }

  public ITableViewColumn[] getColumns( )
  {
    return m_template.getColumns();
  }

  public void addRule( RenderingRule rule )
  {
    m_template.addRule( rule );
  }

  public RenderingRule[] findRules( int mask )
  {
    return m_template.findRules( mask );
  }

  public void removeRule( RenderingRule rule )
  {
    m_template.removeRule( rule );
  }

  public void removeColumn( ITableViewColumn column )
  {
    m_template.removeColumn( column );
  }

  public void removeTemplateEventListener( ITemplateEventListener l )
  {
    m_template.removeTemplateEventListener( l );
  }

  public void removeAllColumns( )
  {
    m_template.removeAllColumns();
  }

  public ISchedulingRule getSchedulingRule( )
  {
    return m_rule;
  }
}