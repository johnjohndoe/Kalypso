package org.kalypso.ogc.sensor.template;

import java.io.InputStream;
import java.util.Iterator;
import java.util.List;

import javax.xml.bind.JAXBException;

import org.kalypso.ogc.sensor.IObservationProvider;
import org.kalypso.plugin.KalypsoGisPlugin;
import org.kalypso.template.obstableview.ObjectFactory;
import org.kalypso.template.obstableview.ObstableviewType;
import org.kalypso.template.obstableview.ObstableviewType.ColumnType;
import org.kalypso.util.factory.FactoryException;
import org.kalypso.util.xml.xlink.XLinkException;
import org.kalypso.util.xml.xlink.resolver.IResolver;

/**
 * A TableViewTemplate
 * 
 * @author schlienger
 */
public class TableViewTemplate
{
  private final static ObjectFactory m_baseFactory = new ObjectFactory();

  private final InputStream m_input;

  private ObstableviewType m_baseTemplate;

  private Column[] m_columns = null;

  /**
   * Constructor
   */
  public TableViewTemplate( final InputStream in )
  {
    m_input = in;
  }

  /**
   * Helper, parses the xml
   */
  private ObstableviewType getBaseTemplate()
  {
    if( m_baseTemplate == null )
    {
      try
      {
        m_baseTemplate = (ObstableviewType)m_baseFactory.createUnmarshaller().unmarshal( m_input );
      }
      catch( JAXBException e )
      {
        e.printStackTrace();
      }
    }

    return m_baseTemplate;
  }
  
  /**
   * Returns the columns defined in this template
   */
  public Column[] getColumnsList()
  {
    if( m_columns == null )
    {
      List cols = getBaseTemplate().getColumn();
      
      m_columns = new Column[ cols.size() ];
    
      int i = 0;
      for( Iterator it = cols.iterator(); it.hasNext(); )
      {
        ObstableviewType.ColumnType col = (ObstableviewType.ColumnType)it.next();
        
        m_columns[i++] = new Column( col );
      }
    }
    
    return m_columns;
  }
  
  /**
   * Wrapper over ColumnType from jaxb output.
   * 
   * @author schlienger
   */
  public static class Column
  {
    private final ColumnType m_col;
    
    private final TemplateXLink m_xlink;

    public Column( final ObstableviewType.ColumnType col )
    {
      m_col = col;
      m_xlink = new TemplateXLink( col );
    }
    
    public int getWidth()
    {
      return m_col.getWidth();
    }
    
    public boolean isEditable()
    {
      return m_col.isEditable();
    }
    
    public void setEditable( boolean value )
    {
      m_col.setEditable( value );
    }
    
    public void setWidth( int value )
    {
      m_col.setWidth( value );
    }
    
    /**
     * Delivers the IObservationProvider with which the clients can display the
     * linked IObservation.
     */
    public IObservationProvider getProvider()
    {
      try
      {
        IResolver res = KalypsoGisPlugin.getDefault().getResolver( m_col.getLinktype() );
        
        return (IObservationProvider)res.resolve( m_xlink );
      }
      catch( FactoryException e )
      {
        // TODO handle
        e.printStackTrace();
      }
      catch( XLinkException e )
      {
        // TODO handle
        e.printStackTrace();
      }
      
      return null;
    }
  }
}
