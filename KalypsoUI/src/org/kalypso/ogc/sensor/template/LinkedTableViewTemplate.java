package org.kalypso.ogc.sensor.template;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.eclipse.core.resources.IProject;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.tableview.ITableViewColumn;
import org.kalypso.ogc.sensor.tableview.ITableViewTemplate;
import org.kalypso.ogc.sensor.tableview.impl.TableViewTemplate;
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

  private int m_toBeResolved = 0;
  private final List m_resolved = new ArrayList();
  
  private boolean m_useResolver = true;

  /**
   * Constructor
   */
  public LinkedTableViewTemplate( final ObstableviewType obsTableView, final IProject project )
  {
    m_project = project;

    m_template = new TableViewTemplate();

    final List cols = obsTableView.getColumnpair();

    // the list of columns, will be added to template once links are resolved
    final List linkedColumns = new ArrayList( cols.size() );

    for( final Iterator it = cols.iterator(); it.hasNext(); )
    {
      ObstableviewType.ColumnpairType col = (ObstableviewType.ColumnpairType)it.next();

      if( m_useResolver )
        linkedColumns.add( new LinkedTableViewColumn( col ) );
      else
        addColumn( new LinkedTableViewColumn( col ) );
    }

    final RulesType trules = obsTableView.getRules();
    if( trules != null )
    {
      for( final Iterator it = trules.getRenderingrule().iterator(); it.hasNext(); )
        m_template.addRule( RenderingRule.createRenderingRule( (TypeRenderingRule)it.next() ) );
    }

    m_toBeResolved = linkedColumns.size();
    
    if( m_useResolver )
    {
      // resolve the links!
      new LinkResolver( (LinkedTableViewColumn[])linkedColumns.toArray( new LinkedTableViewColumn[0] ),
          IObservation.class, project, this );
    }
  }

  public void addColumn( final ITableViewColumn column )
  {
    // resolve link curve before adding column!
    if( column instanceof LinkedTableViewColumn && m_useResolver )
    {
      m_toBeResolved++;
      
      new LinkResolver( new LinkedTableViewColumn[]
      { (LinkedTableViewColumn)column }, IObservation.class, m_project, this );
    }
    else
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

  public void removeColumn( ITableViewColumn column )
  {
    m_template.removeColumn( column );
  }

  public void removeTemplateEventListener( ITemplateEventListener l )
  {
    m_template.removeTemplateEventListener( l );
  }

  /**
   * Adds columns to base template as soon as they are all resolved!
   * 
   * @see org.kalypso.util.link.ILinkResolverListener#onLinkResolved(org.kalypso.util.link.LinkEvent)
   */
  public void onLinkResolved( LinkEvent evt )
  {
    m_resolved.add( evt.getLink() );
    
    m_toBeResolved--;
    
    System.out.println( "onLinkResolved  toBeResolved= " + m_toBeResolved );
    
    if( m_toBeResolved == 0 )
    {
      System.out.println( "... all resolved..." );
      
      for( final Iterator it = m_resolved.iterator(); it.hasNext(); )
      {
        ITableViewColumn col = (ITableViewColumn)it.next();

        // now that link is resolved, we can add the column!
        m_template.addColumn( col );
        
        System.out.println( "... added column: " + col.getName() );
      }
      
      m_resolved.clear();
    }
  }

  public void removeAllColumns()
  {
    m_template.removeAllColumns();
  }

  public void setUseResolver( final boolean useResolver )
  {
    m_useResolver = useResolver;
  }
}