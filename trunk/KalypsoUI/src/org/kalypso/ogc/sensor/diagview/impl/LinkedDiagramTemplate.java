package org.kalypso.ogc.sensor.diagview.impl;

import java.net.URL;
import java.util.Iterator;
import java.util.List;
import java.util.Properties;
import java.util.TreeMap;

import org.eclipse.core.runtime.IStatus;
import org.kalypso.ogc.sensor.IObservation;
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
public class LinkedDiagramTemplate extends ObservationDiagramTemplate implements
    IPoolListener
{
  private final ResourcePool m_pool;

  private final TreeMap m_key2themes;

  /**
   * Constructor
   * 
   * @param obsDiagView
   * @param context
   */
  public LinkedDiagramTemplate( final ObsdiagviewType obsDiagView,
      final URL context )
  {
    super();

    setTitle( obsDiagView.getTitle() );
    setLegendName( obsDiagView.getLegend() == null ? "" : obsDiagView
        .getLegend().getTitle() );
    setShowLegend( obsDiagView.getLegend() == null ? false : obsDiagView
        .getLegend().isVisible() );

    m_pool = KalypsoGisPlugin.getDefault().getPool();
    m_key2themes = new TreeMap( m_pool.getKeyComparator() );

    for( final Iterator it = obsDiagView.getAxis().iterator(); it.hasNext(); )
    {
      final TypeAxis baseAxis = (TypeAxis) it.next();

      addAxis( new DiagramAxis( baseAxis ) );
    }

    final List list = obsDiagView.getObservation();
    for( final Iterator it = list.iterator(); it.hasNext(); )
    {
      final TypeObservation tobs = (TypeObservation) it.next();
      final List tcurves = tobs.getCurve();

      // no observation yet, will be updated once loaded
      final DefaultDiagramTemplateTheme theme = new DefaultDiagramTemplateTheme( null );

      for( final Iterator itcurves = tcurves.iterator(); itcurves.hasNext(); )
      {
        final TypeCurve tcurve = (TypeCurve) itcurves.next();

        final Properties mappings = new Properties();

        final List tmaps = tcurve.getMapping();
        for( final Iterator itm = tmaps.iterator(); itm.hasNext(); )
        {
          final TypeAxisMapping mapping = (TypeAxisMapping) itm.next();

          mappings.setProperty( mapping.getObservationAxis(), mapping
              .getDiagramAxis() );
        }

        // create curve and add it to theme
        final DiagramCurve curve = new DiagramCurve( tcurve.getName(), theme,
            mappings, this );
        theme.addCurve( curve );
      }

      // create key according to observation link
      final PoolableObjectType key = new PoolableObjectType(
          tobs.getLinktype(), tobs.getHref(), context );

      startLoading( key, theme );
    }
  }

  /**
   * Starts the loading process for the given theme
   * 
   * @param key
   * @param theme
   */
  public void startLoading( final PoolableObjectType key,
      final DefaultDiagramTemplateTheme theme )
  {
    // store key-tobs in map before adding to pool
    m_key2themes.put( key, theme );

    // now add to pool
    m_pool.addPoolListener( this, key );
  }
  
  /**
   * @see org.kalypso.ogc.sensor.diagview.impl.DefaultDiagramTemplate#removeAllThemes()
   */
  public void removeAllThemes( )
  {
    m_key2themes.clear();

    super.removeAllThemes();
  }

  /**
   * @see org.kalypso.ogc.sensor.diagview.IDiagramTemplate#dispose()
   */
  public void dispose( )
  {
    m_key2themes.clear();
    m_pool.removePoolListener( this );

    super.dispose();
  }

  /**
   * @see org.kalypso.util.pool.IPoolListener#objectLoaded(org.kalypso.util.pool.IPoolableObjectType,
   *      java.lang.Object, org.eclipse.core.runtime.IStatus)
   */
  public void objectLoaded( final IPoolableObjectType key,
      final Object newValue, final IStatus status )
  {
    if( status.isOK() )
    {
      final DefaultDiagramTemplateTheme theme = (DefaultDiagramTemplateTheme) m_key2themes
          .get( key );

      if( theme == null )
        return;

      theme.setObservation( (IObservation) newValue );

      // now that theme is ready, add it
      addTheme( theme );
    }
  }

  /**
   * @see org.kalypso.util.pool.IPoolListener#objectInvalid(org.kalypso.util.pool.IPoolableObjectType,
   *      java.lang.Object)
   */
  public void objectInvalid( IPoolableObjectType key, Object oldValue )
  {
    // TODO Auto-generated method stub
  }
}