package org.kalypso.ogc.sensor.template;

import java.io.InputStream;
import java.util.Iterator;
import java.util.List;

import org.eclipse.core.resources.IFile;
import org.kalypso.ogc.sensor.tableview.ITableViewColumn;
import org.kalypso.ogc.sensor.tableview.ITableViewTemplate;
import org.kalypso.ogc.sensor.tableview.template.RenderingRule;
import org.kalypso.ogc.sensor.tableview.template.Rules;
import org.kalypso.template.obstableview.ObjectFactory;
import org.kalypso.template.obstableview.ObstableviewType;
import org.kalypso.template.obstableview.TypeRenderingRule;
import org.kalypso.template.obstableview.ObstableviewType.RulesType;

/**
 * A TableViewTemplate that uses a template file.
 * 
 * @author schlienger
 */
public class TableViewTemplate implements ITableViewTemplate
{
  private final static ObjectFactory m_baseFactory = new ObjectFactory();

  private ObstableviewType m_baseTemplate;

  private ColumnPair[] m_columns = null;

  protected IFile m_file;

  /**
   * Constructor
   */
  public TableViewTemplate( final IFile file )
  {
    m_file = file;
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
   * @see org.kalypso.ogc.sensor.tableview.ITableViewTemplate#getColumns()
   */
  public ITableViewColumn[] getColumns()
  {
    if( m_columns == null )
    {
      List cols = getBaseTemplate().getColumnpair();
      
      m_columns = new ColumnPair[ cols.size() ];
    
      int i = 0;
      for( Iterator it = cols.iterator(); it.hasNext(); )
      {
        ObstableviewType.ColumnpairType col = (ObstableviewType.ColumnpairType)it.next();
        
        m_columns[i++] = new ColumnPair( this, col );
      }
    }
    
    return m_columns;
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
    
    RenderingRule[] rules = new RenderingRule[ xrules.size() ];
    
    for( int i = 0; i < rules.length; i++ )
      rules[i] = RenderingRule.createRenderingRule( (TypeRenderingRule)xrules.get(i) );
    
    return new Rules( rules );
  }
}
