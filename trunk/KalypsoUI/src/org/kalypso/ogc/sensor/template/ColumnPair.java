package org.kalypso.ogc.sensor.template;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.jobs.Job;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.IObservationProvider;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.tableview.ITableViewColumn;
import org.kalypso.plugin.KalypsoGisPlugin;
import org.kalypso.template.obstableview.ObstableviewType;
import org.kalypso.template.obstableview.ObstableviewType.ColumnpairType;
import org.kalypso.util.pool.BorrowObjectJob;
import org.kalypso.util.pool.IPoolListener;
import org.kalypso.util.pool.IPoolableObjectType;
import org.kalypso.util.pool.PoolableObjectType;
import org.kalypso.util.pool.ResourcePool;

/**
 * An implementation of the ITableViewColumn for column's specifications fetched
 * from a template file.
 * 
 * @author schlienger
 */
public class ColumnPair implements ITableViewColumn, IPoolListener
{
  private final static Object DUMMY_OBJECT = new Object();

  private final ResourcePool m_pool = KalypsoGisPlugin.getDefault().getPool(
      IObservationProvider.class );

  private final ColumnpairType m_col;
  private final PoolableObjectType m_key;

  private IObservation m_obs = null;

  private final TableViewTemplate m_template;

  public ColumnPair( final ObstableviewType.ColumnpairType col, final IProject project, final TableViewTemplate template )
  {
    m_col = col;
    m_template = template;

    // load the associated observation
    m_key = new PoolableObjectType( m_col.getLinktype(), m_col.getHref(), project );

    Job job = new BorrowObjectJob( "Daten für Tabelle laden", m_pool, this, m_key, DUMMY_OBJECT );
    job.schedule();
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
   * @see org.kalypso.ogc.sensor.tableview.ITableViewColumn#getName()
   */
  public String getName()
  {
    return m_col.getValueAxis();
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservationProvider#getObservation()
   */
  public IObservation getObservation()
  {
    return m_obs;
  }

  /**
   * @see org.kalypso.ogc.sensor.tableview.ITableObservationProvider#getSharedAxis()
   */
  public IAxis getSharedAxis()
  {
    return ObservationUtilities.findAxis( m_obs, m_col.getSharedAxis() );
  }

  /**
   * @see org.kalypso.ogc.sensor.tableview.ITableObservationProvider#getValueAxis()
   */
  public IAxis getValueAxis()
  {
    return ObservationUtilities.findAxis( m_obs, m_col.getValueAxis() );
  }

  /**
   * @see org.kalypso.util.pool.IPoolListener#onObjectInvalid(org.kalypso.util.pool.ResourcePool,
   *      org.kalypso.util.pool.IPoolableObjectType, java.lang.Object, boolean)
   */
  public void onObjectInvalid( final ResourcePool source, final IPoolableObjectType key,
      final Object oldObject, final boolean bCannotReload ) throws Exception
  {
      if( oldObject == DUMMY_OBJECT || m_obs == oldObject )
      {
        m_obs = (IObservation)m_pool.getObject( m_key, new NullProgressMonitor() );
        
        m_template.columnLoaded( this );
      }
  }
}
