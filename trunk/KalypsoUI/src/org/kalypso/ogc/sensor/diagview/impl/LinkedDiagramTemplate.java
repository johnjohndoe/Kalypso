package org.kalypso.ogc.sensor.diagview.impl;

import java.net.URL;
import java.util.ArrayList;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.List;
import java.util.Properties;
import java.util.TreeMap;

import org.eclipse.core.runtime.IStatus;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.diagview.IDiagramCurve;
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
  private final ResourcePool m_pool;
  private final TreeMap m_key2curves;
  private final Hashtable m_curve2key;

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
    m_key2curves = new TreeMap( m_pool.getKeyComparator() );
    m_curve2key = new Hashtable();

    for( final Iterator it = obsDiagView.getAxis().iterator(); it.hasNext(); )
    {
      final TypeAxis baseAxis = (TypeAxis) it.next();

      addAxis( new DiagramAxis( baseAxis ) );
    }
    
    final List list = obsDiagView.getObservation();
    for( final Iterator it = list.iterator(); it.hasNext(); )
    {
      final TypeObservation tobs = (TypeObservation) it.next();

      final List curves = new ArrayList();
      final List tcurves = tobs.getCurve();
      for( final Iterator itcurves = tcurves.iterator(); itcurves.hasNext(); )
      {
        final TypeCurve tcurve = (TypeCurve) itcurves.next();

        final Properties mappings = new Properties();
        
        final List tmaps = tcurve.getMapping();
        for( final Iterator itm = tmaps.iterator(); itm.hasNext(); )
        {
          final TypeAxisMapping mapping = (TypeAxisMapping) itm.next();
          
          mappings.setProperty( mapping.getObservationAxis(), mapping.getDiagramAxis() );
        }

        // new curve, here no observation yet (null) but will be updated once loaded
        final DiagramCurve curve = new DiagramCurve( tcurve.getName(), null, mappings, this );
        curves.add( curve );
      }
  
      final PoolableObjectType key = new PoolableObjectType( tobs.getLinktype(), tobs
          .getHref(), context );

      addObservationTheme( key, curves );
    }
  }
  
  /**
   * Adds a list of curves as an observation them
   * 
   * @param key
   * @param curves
   */
  public void addObservationTheme( final PoolableObjectType key, final List curves )
  {
    // store key-tobs in map before adding to pool
    m_key2curves.put( key, curves );
    
    // store ref for curve to key, used in removeCurve( ... )
    final Iterator it = curves.iterator();
    while( it.hasNext() )
      m_curve2key.put( it.next(), key );
    
    // now add to pool
    m_pool.addPoolListener( this, key );
  }
  
  /**
   * @see org.kalypso.ogc.sensor.diagview.impl.DefaultDiagramTemplate#removeAllCurves()
   */
  public void removeAllCurves( )
  {
    super.removeAllCurves();
    
    m_key2curves.clear();
    m_curve2key.clear();
  }
  
  /**
   * @see org.kalypso.ogc.sensor.diagview.impl.DefaultDiagramTemplate#removeCurve(org.kalypso.ogc.sensor.diagview.IDiagramCurve)
   */
  public void removeCurve( final IDiagramCurve curve )
  {
    super.removeCurve( curve );
    
    final Object key = m_curve2key.get( curve );
    
    final ArrayList list = (ArrayList) m_key2curves.get( key );
    list.remove( curve );
    if( list.isEmpty() )
      m_key2curves.remove( key );
    
    m_curve2key.remove( curve );
  }
  
  /**
   * @see org.kalypso.ogc.sensor.diagview.IDiagramTemplate#dispose()
   */
  public void dispose( )
  {
    super.dispose();

    m_curve2key.clear();
    m_key2curves.clear();
    
    m_pool.removePoolListener( this );
  }
  
  /**
   * @see org.kalypso.util.pool.IPoolListener#objectLoaded(org.kalypso.util.pool.IPoolableObjectType, java.lang.Object, org.eclipse.core.runtime.IStatus)
   */
  public void objectLoaded( final IPoolableObjectType key, final Object newValue, final IStatus status )
  {
    if( status.isOK() )
    {
      final List curves = (List) m_key2curves.get( key );

      if( curves == null || curves.size() == 0 )
        return;
      
      for( final Iterator it = curves.iterator(); it.hasNext(); )
      {
        final DiagramCurve curve = (DiagramCurve) it.next();
        curve.setObservation( (IObservation) newValue );
      }
      
      for( final Iterator it = curves.iterator(); it.hasNext(); )
      {
        // now add the curve since observation has been loaded
        addCurve( (IDiagramCurve) it.next() );
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