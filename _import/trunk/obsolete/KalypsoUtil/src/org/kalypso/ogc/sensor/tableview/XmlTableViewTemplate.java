package org.kalypso.ogc.sensor.tableview;

import java.util.Iterator;
import java.util.List;

import org.kalypso.template.obstableview.ObstableviewType;
import org.kalypso.template.obstableview.ObstableviewType.ColumnType;

/**
 * A TableView template based on a xml-File which validates against the obstableview.xsd schema.
 * 
 * @author schlienger
 */
public class XmlTableViewTemplate implements ITableViewTemplate
{
  private final ObstableviewType m_templ;

  public XmlTableViewTemplate( final ObstableviewType templ )
  {
    m_templ = templ;
  }

  /**
   * @see org.kalypso.ogc.sensor.tableview.ITableViewTemplate#getColumns()
   */
  public ITableViewColumn[] getColumns()
  {
    List cols = m_templ.getColumn();
    
    ITableViewColumn[] tvcols = new ITableViewColumn[ cols.size() ];
    
    int i = 0;
    for( Iterator it = cols.iterator(); it.hasNext(); )
    {
      ObstableviewType.ColumnType col = (ColumnType)it.next();
      
      
      //tvcols[i++] = new DefaultTableViewColumn(  );
    }
    
    return tvcols;
  }
}
