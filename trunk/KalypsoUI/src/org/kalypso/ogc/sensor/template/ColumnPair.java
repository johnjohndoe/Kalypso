package org.kalypso.ogc.sensor.template;

import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.IObservationProvider;
import org.kalypso.ogc.sensor.tableview.ITableViewColumn;
import org.kalypso.template.obstableview.ObstableviewType;
import org.kalypso.template.obstableview.ObstableviewType.ColumnpairType;
import org.kalypso.util.pool.BorrowObjectJob;
import org.kalypso.util.pool.IPoolListener;
import org.kalypso.util.pool.PoolableObjectType;
import org.kalypso.util.pool.ResourcePool;

/**
 * An implementation of the ITableViewColumn for column's specifications fetched
 * from a template file.
 * 
 * @author schlienger
 */
public class ColumnPair implements ITableViewColumn
{
  private static final Object DUMMY_OBJECT = new Object();
  
  private final TableViewTemplate m_template;

  private PoolableObjectType m_key = null;
  
  private IObservationProvider m_provider = null;

  private final ColumnpairType m_col;

  public ColumnPair( final TableViewTemplate template, final ObstableviewType.ColumnpairType col )
  {
    m_template = template;
    m_col = col;
  }

  public String getLinktype()
  {
    return m_col.getLinktype();
  }

  /**
   * @see org.kalypso.ogc.sensor.tableview.ITableViewColumn#isEditable()
   */
  public boolean isEditable()
  {
    return m_col.isEditable();
  }

  /**
   * @see org.kalypso.ogc.sensor.tableview.ITableViewColumn#getWidth()
   */
  public int getWidth()
  {
    return m_col.getWidth();
  }

  /**
   * @see org.kalypso.ogc.sensor.tableview.ITableViewColumn#setWidth(int)
   */
  public void setWidth( int width )
  {
    m_col.setWidth( width );
  }

  /**
   * Returns the key associated with this column
   */
  public PoolableObjectType getKey()
  {
    if( m_key == null )
      m_key = new PoolableObjectType( m_col.getLinktype(), m_col.getHref(), m_template.m_file.getProject() );

    return m_key;
  }

  /**
   * Schedule the job for borrowing the object that 'lies under' this column
   */
  public void startBorrowObjectJob( ResourcePool pool, IPoolListener listener )
  {
    new BorrowObjectJob( "Load Column for Table", pool, listener, getKey(), DUMMY_OBJECT ).schedule();
  }

  /**
   * Specifies whether this column in invalid or not
   */
  public boolean isInvalid( Object oldObject )
  {
    return oldObject == DUMMY_OBJECT || m_provider == oldObject;
  }

  /**
   * @see org.kalypso.ogc.sensor.tableview.ITableViewColumn#getName()
   */
  public String getName()
  {
    return null;
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservationProvider#getObservation()
   */
  public IObservation getObservation()
  {
    return m_provider.getObservation();
  }

  public void setProvider( IObservationProvider p )
  {
    m_provider= p;
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservationProvider#getSharedAxis()
   */
  public IAxis getSharedAxis()
  {
    return m_provider.getSharedAxis();
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservationProvider#getValueAxis()
   */
  public IAxis getValueAxis()
  {
    return m_provider.getValueAxis();
  }
}
