package org.kalypso.ogc.sensor.template;

import java.io.InputStream;
import java.util.Iterator;
import java.util.List;

import org.eclipse.core.resources.IFile;
import org.kalypso.template.obstableview.ObjectFactory;
import org.kalypso.template.obstableview.ObstableviewType;

/**
 * A TableViewTemplate that uses a template file.
 * 
 * @author schlienger
 */
public class TableViewTemplate
{
  private final static ObjectFactory m_baseFactory = new ObjectFactory();

  private ObstableviewType m_baseTemplate;

  private TableViewColumn[] m_columns = null;

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
        e.printStackTrace();
      }
    }

    return m_baseTemplate;
  }
  
  /**
   * @see org.kalypso.ogc.sensor.tableview.ITableViewTemplate#getColumns()
   */
  public TableViewColumn[] getColumns()
  {
    if( m_columns == null )
    {
      List cols = getBaseTemplate().getColumn();
      
      m_columns = new TableViewColumn[ cols.size() ];
    
      int i = 0;
      for( Iterator it = cols.iterator(); it.hasNext(); )
      {
        ObstableviewType.ColumnType col = (ObstableviewType.ColumnType)it.next();
        
        m_columns[i++] = new TableViewColumn( this, col );
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
}
