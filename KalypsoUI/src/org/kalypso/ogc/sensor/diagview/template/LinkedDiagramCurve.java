package org.kalypso.ogc.sensor.diagview.template;

import java.net.URL;
import java.util.Properties;

import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.jobs.Job;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.diagview.IAxisMapping;
import org.kalypso.ogc.sensor.diagview.IDiagramCurve;
import org.kalypso.ogc.sensor.diagview.IDiagramTemplate;
import org.kalypso.ogc.sensor.diagview.impl.DiagramCurve;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.util.pool.BorrowObjectJob;
import org.kalypso.util.pool.IPoolListener;
import org.kalypso.util.pool.IPoolableObjectType;
import org.kalypso.util.pool.PoolableObjectType;
import org.kalypso.util.pool.ResourcePool;
import org.kalypso.util.runtime.IVariableArguments;
import org.kalypso.util.xml.xlink.IXlink;

/**
 * @author schlienger
 */
public class LinkedDiagramCurve implements IDiagramCurve, IPoolListener
{
  private final static Object DUMMY_OBJECT = new Object();
  
  private final String m_name;

  private final IDiagramTemplate m_template;

  private final Properties m_mappings;

  private DiagramCurve m_curve = null;

  private IVariableArguments m_args = null;

  private final ResourcePool m_pool;

  public LinkedDiagramCurve( final String linkType, final IXlink xlink, final String name,
      final Properties mappings, final IDiagramTemplate template, final URL context )
  {
    m_name = name;
    m_mappings = mappings;
    m_template = template;
    
    final PoolableObjectType key = new PoolableObjectType( linkType, xlink.getHRef(), context );
    m_pool = KalypsoGisPlugin.getDefault().getPool( IObservation.class );
    
    final Job job = new BorrowObjectJob( "Link auslösen für Observation", m_pool, this, key, DUMMY_OBJECT );
    job.schedule();
  }

  public boolean equals( Object obj )
  {
    return m_curve.equals( obj );
  }

  public IAxisMapping[] getMappings()
  {
    return m_curve.getMappings();
  }

  public String getName()
  {
    return m_curve.getName();
  }

  public IVariableArguments getArguments()
  {
    return m_curve.getArguments();
  }

  public void setArguments( IVariableArguments args )
  {
    if( m_curve == null )
      m_args = args;
    else
      m_curve.setArguments( args );
  }

  public IObservation getObservation()
  {
    return m_curve.getObservation();
  }

  /**
   * @see org.kalypso.util.pool.IPoolListener#onObjectInvalid(org.kalypso.util.pool.ResourcePool, org.kalypso.util.pool.IPoolableObjectType, java.lang.Object, boolean)
   */
  public void onObjectInvalid( ResourcePool source, IPoolableObjectType key, Object oldObject, boolean bCannotReload ) throws Exception
  {
    if( oldObject == DUMMY_OBJECT || m_curve.getObservation() == oldObject )
    {
      IObservation obs = (IObservation) m_pool.getObject( key, new NullProgressMonitor() );      
      
      m_curve = new DiagramCurve( m_name, obs, m_mappings, m_template, m_args );
      
      m_template.addCurve( this );
    }
  }
}