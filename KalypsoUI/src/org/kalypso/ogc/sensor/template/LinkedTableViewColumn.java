package org.kalypso.ogc.sensor.template;

import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.tableview.ITableViewColumn;
import org.kalypso.ogc.sensor.tableview.impl.TableViewColumn;
import org.kalypso.template.obstableview.ObstableviewType;
import org.kalypso.util.link.ObjectLink;
import org.kalypso.util.xml.xlink.IXlink;
import org.kalypso.util.xml.xlink.JAXBXLink;

/**
 * An implementation of the ITableViewColumn for column's specifications fetched
 * from a template file.
 * 
 * @author schlienger
 */
public class LinkedTableViewColumn extends ObjectLink implements ITableViewColumn
{
  private final String m_name;

  private final boolean m_isEditable;

  private final int m_width;

  private final String m_sharedAxisName;

  private final String m_valueAxisName;

  private TableViewColumn m_column;

  public LinkedTableViewColumn( final ObstableviewType.ColumnpairType col )
  {
    this( col.getValueAxis(), col.getLinktype(), new JAXBXLink(col), col.isEditable(), col.getWidth(), col.getSharedAxis(), col.getValueAxis() );
  }
  
  public LinkedTableViewColumn( final String name, final String linkType, final IXlink xlink,
      final boolean isEditable, final int width, final String sharedAxisName,
      final String valueAxisName )
  {
    super( linkType, xlink );

    m_name = name;
    m_isEditable = isEditable;
    m_width = width;
    m_sharedAxisName = sharedAxisName;
    m_valueAxisName = valueAxisName;
  }

  /**
   * @see org.kalypso.util.link.ObjectLink#linkResolved(java.lang.Object)
   */
  public void linkResolved( Object object )
  {
    super.linkResolved( object );

    m_column = new TableViewColumn( m_name, (IObservation)getLinkedObject(), m_isEditable, m_width,
        m_sharedAxisName, m_valueAxisName );
  }

  public boolean equals( Object obj )
  {
    return m_column.equals( obj );
  }

  public String getName()
  {
    return m_column.getName();
  }

  public IObservation getObservation()
  {
    return m_column.getObservation();
  }

  public IAxis getSharedAxis()
  {
    return m_column.getSharedAxis();
  }

  public IAxis getValueAxis()
  {
    return m_column.getValueAxis();
  }

  public int getWidth()
  {
    return m_column.getWidth();
  }

  public boolean isEditable()
  {
    return m_column.isEditable();
  }

  public void setWidth( int width )
  {
    m_column.setWidth( width );
  }
}