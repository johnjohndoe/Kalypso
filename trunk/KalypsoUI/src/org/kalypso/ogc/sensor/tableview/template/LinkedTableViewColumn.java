package org.kalypso.ogc.sensor.tableview.template;

import java.net.URL;

import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.jobs.Job;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.tableview.ITableViewColumn;
import org.kalypso.ogc.sensor.tableview.impl.DefaultTableViewColumn;
import org.kalypso.template.obstableview.ObstableviewType;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.util.pool.BorrowObjectJob;
import org.kalypso.util.pool.IPoolListener;
import org.kalypso.util.pool.IPoolableObjectType;
import org.kalypso.util.pool.PoolableObjectType;
import org.kalypso.util.pool.ResourcePool;
import org.kalypso.util.runtime.IVariableArguments;
import org.kalypso.util.xml.xlink.IXlink;
import org.kalypso.util.xml.xlink.JAXBXLink;

/**
 * An implementation of the ITableViewColumn for column's specifications fetched
 * from a template file.
 * 
 * @author schlienger
 */
public class LinkedTableViewColumn implements ITableViewColumn, IPoolListener
{
  private final static Object DUMMY_OBJECT = new Object();

  private final String m_name;

  private final boolean m_isEditable;

  private final int m_width;

  private final String m_sharedAxisName;

  private final String m_valueAxisName;

  private DefaultTableViewColumn m_column;

  private IVariableArguments m_args = null;

  private final ResourcePool m_pool;

  private final LinkedTableViewTemplate m_template;

  private final PoolableObjectType m_key;

  public LinkedTableViewColumn( final LinkedTableViewTemplate template, final ObstableviewType.ColumnpairType col, final URL context )
  {
    this( template, col.getValueAxis(), col.getLinktype(), new JAXBXLink( col ), col
        .isEditable(), col.getWidth(), col.getSharedAxis(), col.getValueAxis(), context );
  }

  public LinkedTableViewColumn( final LinkedTableViewTemplate template, final String name, final String linkType,
      final IXlink xlink, final boolean isEditable, final int width,
      final String sharedAxisName, final String valueAxisName, final URL context )
  {
    m_template = template;
    m_name = name;
    m_isEditable = isEditable;
    m_width = width;
    m_sharedAxisName = sharedAxisName;
    m_valueAxisName = valueAxisName;

    m_key = new PoolableObjectType( linkType, xlink
        .getHRef(), context );
    m_pool = KalypsoGisPlugin.getDefault().getPool( IObservation.class );

    final Job job = new BorrowObjectJob( "Link auslösen für Observation",
        m_pool, this, m_key, DUMMY_OBJECT );
    job.setRule( m_template.getSchedulingRule() );
    job.schedule();
  }

  public String getName( )
  {
    return m_column.getName();
  }

  public IObservation getObservation( )
  {
    return m_column.getObservation();
  }

  public IAxis getSharedAxis( )
  {
    return m_column.getSharedAxis();
  }

  public IAxis getValueAxis( )
  {
    return m_column.getValueAxis();
  }

  public int getWidth( )
  {
    return m_column.getWidth();
  }

  public boolean isEditable( )
  {
    return m_column.isEditable();
  }

  public void setWidth( int width )
  {
    m_column.setWidth( width );
  }

  public IVariableArguments getArguments( )
  {
    return m_column.getArguments();
  }

  public void setArguments( IVariableArguments args )
  {
    if( m_column != null )
      m_column.setArguments( args );
    else
      m_args = args;
  }

  /**
   * @see org.kalypso.util.pool.IPoolListener#onObjectInvalid(org.kalypso.util.pool.ResourcePool,
   *      org.kalypso.util.pool.IPoolableObjectType, java.lang.Object, boolean)
   */
  public void onObjectInvalid( ResourcePool source, IPoolableObjectType key,
      Object oldObject, boolean bCannotReload ) throws Exception
  {
    if( oldObject == DUMMY_OBJECT || m_column.getObservation() == oldObject )
    {
      IObservation obs = (IObservation) m_pool.getObject( key,
          new NullProgressMonitor() );

      m_column = new DefaultTableViewColumn( m_name, obs, m_isEditable, m_width,
          m_sharedAxisName, m_valueAxisName, m_args );
      
      m_template.addColumn( this );
    }
  }

  /**
   * @see org.kalypso.ogc.sensor.tableview.ITableViewColumn#dispose()
   */
  public void dispose( )
  {
    m_pool.removePoolListener( this );
    m_pool.releaseKey( m_key );
    m_column.dispose();
  }

  /**
   * @see org.kalypso.ogc.sensor.tableview.ITableViewColumn#isDirty()
   */
  public boolean isDirty( )
  {
    return m_column.isDirty();
  }
 
  /**
   * Sets the dirty flag. Call this method when you know the data in the observation
   * has changed.
   * 
   * @param dirty
   */
  public void setDirty( final boolean dirty )
  {
    m_column.setDirty( dirty );
  }
}