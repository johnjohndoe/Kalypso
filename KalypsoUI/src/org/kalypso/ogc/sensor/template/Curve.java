package org.kalypso.ogc.sensor.template;

import java.util.Iterator;
import java.util.List;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.jobs.Job;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.diagview.DefaultMapping;
import org.kalypso.ogc.sensor.diagview.IAxisMapping;
import org.kalypso.ogc.sensor.diagview.ICurve;
import org.kalypso.plugin.KalypsoGisPlugin;
import org.kalypso.template.obsdiagview.ObsdiagviewType;
import org.kalypso.template.obsdiagview.TypeAxisMapping;
import org.kalypso.template.obsdiagview.ObsdiagviewType.CurveType;
import org.kalypso.util.pool.BorrowObjectJob;
import org.kalypso.util.pool.IPoolListener;
import org.kalypso.util.pool.IPoolableObjectType;
import org.kalypso.util.pool.PoolableObjectType;
import org.kalypso.util.pool.ResourcePool;

/**
 * @author schlienger
 */
public class Curve implements ICurve, IPoolListener
{
  private final static Object DUMMY_OBJECT = new Object();

  private final ResourcePool m_pool = KalypsoGisPlugin.getDefault().getPool(
      IObservation.class );

  private IObservation m_obs = null;

  private final CurveType m_baseCurve;

  private final DiagramViewTemplate m_template;

  private PoolableObjectType m_key;

  public Curve( final ObsdiagviewType.CurveType baseCurve, final IProject project,
      final DiagramViewTemplate template )
  {
    m_baseCurve = baseCurve;
    m_template = template;
    
    // load the associated observation
    m_key = new PoolableObjectType( m_baseCurve.getLinktype(), m_baseCurve.getHref(), project );

    Job job = new BorrowObjectJob( "Daten für Diagramm laden", m_pool, this, m_key, DUMMY_OBJECT );
    job.schedule();
  }
  
  /**
   * @see org.kalypso.ogc.sensor.diagview.ICurve#getName()
   */
  public String getName()
  {
    return m_baseCurve.getName();
  }

  /**
   * @see org.kalypso.ogc.sensor.diagview.ICurve#getMappings()
   */
  public IAxisMapping[] getMappings()
  {
    List basem = m_baseCurve.getMapping();
    IAxisMapping[] mapping = new IAxisMapping[basem.size() ];

    int i = 0;
    for( Iterator it = basem.iterator(); it.hasNext(); )
    {
      TypeAxisMapping am = (TypeAxisMapping)it.next();
      
      mapping[i++] = new DefaultMapping( ObservationUtilities.findAxis(m_obs, am.getObservationAxis()), m_template.getDiagramAxis( am.getDiagramAxis() ) );
    }
    
    return mapping;
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservationProvider#getObservation()
   */
  public IObservation getObservation()
  {
    return m_obs;
  }

  /**
   * @see org.kalypso.util.pool.IPoolListener#onObjectInvalid(org.kalypso.util.pool.ResourcePool,
   *      org.kalypso.util.pool.IPoolableObjectType, java.lang.Object, boolean)
   */
  public void onObjectInvalid( ResourcePool source, IPoolableObjectType key, Object oldObject,
      boolean bCannotReload ) throws Exception
  {
    if( oldObject == DUMMY_OBJECT || m_obs == oldObject )
    {
      m_obs = (IObservation)m_pool.getObject( m_key, new NullProgressMonitor() );
      
      m_template.curveLoaded( this );
    }
  }
}