package org.kalypso.ogc.sensor.template;

import java.util.Iterator;
import java.util.List;

import org.eclipse.core.resources.IProject;
import org.kalypso.java.util.Arrays;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.tableview.ITableViewColumn;
import org.kalypso.ogc.sensor.tableview.ITableViewTemplate;
import org.kalypso.ogc.sensor.tableview.TableViewTemplate;
import org.kalypso.ogc.sensor.tableview.rules.RenderingRule;
import org.kalypso.template.obstableview.ObstableviewType;
import org.kalypso.template.obstableview.TypeRenderingRule;
import org.kalypso.template.obstableview.ObstableviewType.RulesType;
import org.kalypso.util.link.ILinkResolverListener;
import org.kalypso.util.link.LinkEvent;
import org.kalypso.util.link.LinkResolver;

/**
 * A TableViewTemplate based on a XML template file. Performs the load
 * operation.
 * 
 * @author schlienger
 */
public class LinkedTableViewTemplate implements ITableViewTemplate, ILinkResolverListener
{
  private final TableViewTemplate m_template;

  private final IProject m_project;

  /**
   * Constructor
   */
  public LinkedTableViewTemplate( final ObstableviewType obsTableView, final IProject project )
  {
    m_project = project;

    m_template = new TableViewTemplate();

    final List cols = obsTableView.getColumnpair();
    for( Iterator it = cols.iterator(); it.hasNext(); )
    {
      ObstableviewType.ColumnpairType col = (ObstableviewType.ColumnpairType)it.next();

      m_template.addColumn( new LinkedTableViewColumn( col ) );
    }

    final RulesType trules = obsTableView.getRules();
    if( trules != null )
    {
      for( Iterator it = trules.getRenderingrule().iterator(); it.hasNext(); )
        m_template.addRule( RenderingRule.createRenderingRule( (TypeRenderingRule)it.next() ) );
    }

    // resolve the links!
    new LinkResolver( (LinkedTableViewColumn[])Arrays.castArray( m_template.getColumns(),
        new LinkedTableViewColumn[0] ), IObservation.class, project, this );
  }

  public void addColumn( ITableViewColumn column )
  {
    // resolve link curve before adding column!
    if( column instanceof LinkedTableViewColumn )
      new LinkResolver( new LinkedTableViewColumn[]
      { (LinkedTableViewColumn)column }, IObservation.class, m_project, this );
  }

  public void addTemplateEventListener( ITemplateEventListener l )
  {
    m_template.addTemplateEventListener( l );
  }

  public boolean equals( Object obj )
  {
    return m_template.equals( obj );
  }

  public void fireTemplateChanged( TemplateEvent evt )
  {
    m_template.fireTemplateChanged( evt );
  }

  public ITableViewColumn[] getColumns()
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

  public int hashCode()
  {
    return m_template.hashCode();
  }

  public void removeColumn( ITableViewColumn column )
  {
    m_template.removeColumn( column );
  }

  public void removeTemplateEventListener( ITemplateEventListener l )
  {
    m_template.removeTemplateEventListener( l );
  }

  public String toString()
  {
    return m_template.toString();
  }

  /**
   * @see org.kalypso.util.link.ILinkResolverListener#onLinkResolved(org.kalypso.util.link.LinkEvent)
   */
  public void onLinkResolved( LinkEvent evt )
  {
    // now that link is resolved, we can add the column!
    m_template.addColumn( (ITableViewColumn)evt.getLink() );
  }

  public void removeAllColumns()
  {
    m_template.removeAllColumns();
  }
}