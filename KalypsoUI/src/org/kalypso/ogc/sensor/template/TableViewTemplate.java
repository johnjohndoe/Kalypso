package org.kalypso.ogc.sensor.template;

import java.io.InputStream;
import java.util.Iterator;
import java.util.List;
import java.util.Vector;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.IProgressMonitor;
import org.kalypso.ogc.sensor.tableview.ITableViewColumn;
import org.kalypso.ogc.sensor.tableview.ITableViewTemplate;
import org.kalypso.ogc.sensor.tableview.template.RenderingRule;
import org.kalypso.ogc.sensor.tableview.template.Rules;
import org.kalypso.template.obstableview.ObjectFactory;
import org.kalypso.template.obstableview.ObstableviewType;
import org.kalypso.template.obstableview.TypeRenderingRule;
import org.kalypso.template.obstableview.ObstableviewType.RulesType;

/**
 * A TableViewTemplate based on a XML template file. Performs the load
 * operation.
 * 
 * @author schlienger
 */
public class TableViewTemplate implements ITableViewTemplate
{
  private final static ObjectFactory m_baseFactory = new ObjectFactory();

  private ObstableviewType m_baseTemplate;

  private final IFile m_file;

  private final IProgressMonitor m_monitor;

  private final List m_columns = new Vector();

  private List m_baseColumns = null;

  private final List m_listeners = new Vector();

  /**
   * Constructor
   */
  public TableViewTemplate( final IFile file, final IProgressMonitor monitor )
  {
    m_file = file;
    m_monitor = monitor;

    loadColumns();
  }

  /**
   * Helper
   */
  private ObstableviewType getBaseTemplate()
  {
    if( m_baseTemplate == null )
    {
      try
      {
        InputStream ins = m_file.getContents();

        m_baseTemplate = (ObstableviewType)m_baseFactory.createUnmarshaller().unmarshal( ins );

        ins.close();
      }
      catch( Exception e )
      {
        // TODO: handling
        throw new RuntimeException( e );
      }
    }

    return m_baseTemplate;
  }

  /**
   * Loads all columns from the base template.
   */
  private void loadColumns()
  {
    m_baseColumns = getBaseTemplate().getColumnpair();

    m_monitor.beginTask( "Tabelllenvorlage laden", m_baseColumns.size() );

    for( Iterator it = m_baseColumns.iterator(); it.hasNext(); )
    {
      ObstableviewType.ColumnpairType col = (ObstableviewType.ColumnpairType)it.next();

      // creates a columnpair, starts the job for retrieving the IObservation
      new ColumnPair( col, m_file.getProject(), this );
    }
  }

  /**
   * Liefert den zugeordnete Project
   */
  protected IFile getFile()
  {
    return m_file;
  }

  /**
   * Returns the rendering rules (packed into a Rules object)
   */
  public Rules getRules()
  {
    final RulesType trules = getBaseTemplate().getRules();
    if( trules == null )
      return new Rules();

    List xrules = trules.getRenderingrule();

    RenderingRule[] rules = new RenderingRule[xrules.size()];

    for( int i = 0; i < rules.length; i++ )
      rules[i] = RenderingRule.createRenderingRule( (TypeRenderingRule)xrules.get( i ) );

    return new Rules( rules );
  }

  /**
   *  
   */
  protected void columnLoaded( final ColumnPair col )
  {
    m_columns.add( col );

    m_monitor.worked( 1 );

    if( m_columns.size() == m_baseColumns.size() )
      fireTemplateLoaded();
  }

  /**
   * @see org.kalypso.ogc.sensor.template.ITemplateAdapter#addListener(org.kalypso.ogc.sensor.template.ITemplateListener)
   */
  public void addListener( ITemplateListener l )
  {
    m_listeners.add( l );
  }

  /**
   * @see org.kalypso.ogc.sensor.template.ITemplateAdapter#removeListener(org.kalypso.ogc.sensor.template.ITemplateListener)
   */
  public void removeListener( ITemplateListener l )
  {
    m_listeners.remove( l );
  }

  /**
   * @see org.kalypso.ogc.sensor.tableview.ITableViewTemplate#fireTemplateLoaded()
   */
  public void fireTemplateLoaded()
  {
    for( Iterator it = m_listeners.iterator(); it.hasNext(); )
    {
      ITemplateListener l = (ITemplateListener)it.next();
      l.onTemplateLoaded();
    }
  }

  /**
   * @see org.kalypso.ogc.sensor.tableview.ITableViewTemplate#getColumns()
   */
  public ITableViewColumn[] getColumns()
  {
    return (ITableViewColumn[])m_columns.toArray( new ITableViewColumn[0] );
  }
}