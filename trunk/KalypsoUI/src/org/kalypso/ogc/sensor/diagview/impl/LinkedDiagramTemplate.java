package org.kalypso.ogc.sensor.diagview.impl;

import java.net.URL;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.List;
import java.util.Properties;

import org.eclipse.core.runtime.IStatus;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.template.obsdiagview.ObjectFactory;
import org.kalypso.template.obsdiagview.ObsdiagviewType;
import org.kalypso.template.obsdiagview.TypeAxis;
import org.kalypso.template.obsdiagview.TypeAxisMapping;
import org.kalypso.template.obsdiagview.TypeCurve;
import org.kalypso.template.obsdiagview.TypeObservation;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.util.pool.IPoolListener;
import org.kalypso.util.pool.IPoolableObjectType;
import org.kalypso.util.pool.PoolableObjectType;
import org.kalypso.util.pool.ResourcePool;

/**
 * A DiagramTemplate that is based on a XML file.
 * 
 * @author schlienger
 */
public class LinkedDiagramTemplate extends DefaultDiagramTemplate implements IPoolListener
{
  private static ObjectFactory m_objectFactory;
  private final ResourcePool m_pool;
  private final Hashtable m_keys;

  /**
   * Constructor
   * 
   * @param obsDiagView
   * @param context
   */
  public LinkedDiagramTemplate( final ObsdiagviewType obsDiagView,
      final URL context )
  {
    super( obsDiagView.getTitle(), obsDiagView.getLegend() == null ? ""
        : obsDiagView.getLegend().getTitle(),
        obsDiagView.getLegend() == null ? false : obsDiagView.getLegend()
            .isVisible() );

    m_pool = KalypsoGisPlugin.getDefault().getPool( );
    m_keys = new Hashtable();

    final List list = obsDiagView.getObservation();
    for( final Iterator it = list.iterator(); it.hasNext(); )
    {
      final TypeObservation tobs = (TypeObservation) it.next();

      final PoolableObjectType key = new PoolableObjectType( tobs.getLinktype(), tobs
          .getHref(), context );
      m_pool.addPoolListener( this, key );
      m_keys.put( key, tobs );
    }
    
    for( final Iterator it = obsDiagView.getAxis().iterator(); it.hasNext(); )
    {
      final TypeAxis baseAxis = (TypeAxis) it.next();

      addAxis( new DiagramAxis( baseAxis ) );
    }
  }
  
  /**
   * @see org.kalypso.ogc.sensor.diagview.IDiagramTemplate#dispose()
   */
  public void dispose( )
  {
    super.dispose();

    m_pool.removePoolListener( this );
  }
  
  /**
   * @see org.kalypso.util.pool.IPoolListener#objectLoaded(org.kalypso.util.pool.IPoolableObjectType, java.lang.Object, org.eclipse.core.runtime.IStatus)
   */
  public void objectLoaded( IPoolableObjectType key, Object newValue, IStatus status )
  {
    if( status.isOK() )
    {
      final TypeObservation tobs = (TypeObservation) m_keys.get( key );

      final List curves = tobs.getCurve();
      for( Iterator it = curves.iterator(); it.hasNext(); )
      {
        final TypeCurve curve = (TypeCurve) it.next();

        final Properties mappings = new Properties();
        
        final List list = curve.getMapping();
        for( final Iterator itm = list.iterator(); itm.hasNext(); )
        {
          final TypeAxisMapping mapping = (TypeAxisMapping) itm.next();
          
          mappings.setProperty( mapping.getObservationAxis(), mapping.getDiagramAxis() );
        }

        addCurve( new DiagramCurve( curve.getName(), (IObservation) newValue, mappings, this ) );
      }
    }    
  }

  /**
   * @see org.kalypso.util.pool.IPoolListener#objectInvalid(org.kalypso.util.pool.IPoolableObjectType, java.lang.Object)
   */
  public void objectInvalid( IPoolableObjectType key, Object oldValue )
  {
    // TODO Auto-generated method stub
  }
}