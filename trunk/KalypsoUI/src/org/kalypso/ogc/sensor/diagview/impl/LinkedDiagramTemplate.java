package org.kalypso.ogc.sensor.diagview.impl;

import java.net.URL;
import java.util.Iterator;
import java.util.List;
import java.util.TreeMap;

import org.eclipse.core.runtime.IStatus;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.IObservationListener;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.diagview.IDiagramTemplateTheme;
import org.kalypso.ogc.sensor.proxy.IProxyFactory;
import org.kalypso.ogc.sensor.template.TemplateEvent;
import org.kalypso.template.obsdiagview.ObsdiagviewType;
import org.kalypso.template.obsdiagview.TypeAxis;
import org.kalypso.template.obsdiagview.TypeObservation;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.util.pool.IPoolListener;
import org.kalypso.util.pool.IPoolableObjectType;
import org.kalypso.util.pool.PoolableObjectType;
import org.kalypso.util.pool.ResourcePool;
import org.kalypso.util.runtime.IVariableArguments;

/**
 * A DiagramTemplate that is based on a XML file.
 * 
 * @author schlienger
 */
public class LinkedDiagramTemplate extends ObservationDiagramTemplate implements
    IPoolListener, IObservationListener
{
  private final ResourcePool m_pool;

  private final TreeMap m_key2themes;

  private IProxyFactory m_factory;

  /**
   * Constructor
   */
  public LinkedDiagramTemplate( )
  {
    super();

    m_pool = KalypsoGisPlugin.getDefault().getPool();
    m_key2themes = new TreeMap( m_pool.getKeyComparator() );
  }

  /**
   * Sets the base template and loads the curves.
   * 
   * @param obsDiagView
   * @param context
   */
  public void setBaseTemplate( final ObsdiagviewType obsDiagView,
      final URL context )
  {
    removeAllThemes();

    setTitle( obsDiagView.getTitle() );
    setLegendName( obsDiagView.getLegend() == null ? "" : obsDiagView
        .getLegend().getTitle() );
    setShowLegend( obsDiagView.getLegend() == null ? false : obsDiagView
        .getLegend().isVisible() );

    // axes spec is optional
    if( obsDiagView.getAxis() != null )
    {
      for( final Iterator it = obsDiagView.getAxis().iterator(); it.hasNext(); )
      {
        final TypeAxis baseAxis = (TypeAxis) it.next();

        addAxis( new DiagramAxis( baseAxis ) );
      }
    }

    final List list = obsDiagView.getObservation();
    for( final Iterator it = list.iterator(); it.hasNext(); )
    {
      final TypeObservation tobs = (TypeObservation) it.next();

      final IDiagramTemplateTheme theme;

      // tricky: if no curves specified in the xml, we won't use a
      // LinkedDiagramTemplate but the default one. Once the observation
      // will be loaded, all possible curves will be created according to the
      // observation
      if( tobs.getCurve().size() == 0 )
        theme = new DefaultDiagramTemplateTheme(); // fake theme
      else
      {
        // no observation yet, will be updated once loaded
        // curves will be taken from the xml
        theme = new LinkedDiagramTemplateTheme( this, tobs );
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
      final IDiagramTemplateTheme theme )
  {
    // store key-tobs in map before adding to pool
    m_key2themes.put( key, theme );

    // now add to pool
    m_pool.addPoolListener( this, key );
  }

  /**
   * Convenienve method for adding an observation to this template.
   * 
   * @param themeName
   *          used as part of the curve name if not null
   * @param context
   * @param href
   * @param linktype
   * @param ignoreExceptions
   * @param args
   */
  public void addObservation( final String themeName, final URL context,
      final String href, final String linktype, final boolean ignoreExceptions,
      final IVariableArguments args )
  {
    // create key according to observation link
    final PoolableObjectType key = new PoolableObjectType( linktype, href,
        context, ignoreExceptions );

    // fake theme because it won't be added directly to this template
    final DefaultDiagramTemplateTheme fakeTheme = new DefaultDiagramTemplateTheme(
        themeName );
    fakeTheme.setArguments( args );

    // use load mechanism
    startLoading( key, fakeTheme );
  }

  /**
   * @see org.kalypso.ogc.sensor.diagview.impl.DefaultDiagramTemplate#removeAllThemes()
   */
  public void removeAllThemes( )
  {
    m_key2themes.clear();

    // TODO: auch für einzelne Themen???
    m_pool.removePoolListener( this );

    clearObsListener();

    super.removeAllThemes();
  }

  /**
   * Removes this from the listeners for the observations of this' themes
   */
  private void clearObsListener( )
  {
    for( final Iterator it = getThemes().iterator(); it.hasNext(); )
    {
      final IDiagramTemplateTheme theme = (IDiagramTemplateTheme) it.next();
      theme.getObservation().removeListener( this );
    }
  }

  /**
   * @see org.kalypso.ogc.sensor.diagview.impl.DefaultDiagramTemplate#removeTheme(org.kalypso.ogc.sensor.diagview.IDiagramTemplateTheme)
   */
  public void removeTheme( IDiagramTemplateTheme theme )
  {
    if( m_key2themes.containsValue( theme ) )
    {
      final Iterator it = m_key2themes.keySet().iterator();
      while( it.hasNext() )
      {
        Object key = it.next();

        if( m_key2themes.get( key ) == theme )
        {
          m_key2themes.remove( key );
          break;
        }
      }
    }

    theme.getObservation().removeListener( this );

    super.removeTheme( theme );
  }

  /**
   * @see org.kalypso.ogc.sensor.diagview.IDiagramTemplate#dispose()
   */
  public void dispose( )
  {
    m_key2themes.clear();
    m_pool.removePoolListener( this );

    clearObsListener();

    super.dispose();
  }

  /**
   * @see org.kalypso.util.pool.IPoolListener#objectLoaded(org.kalypso.util.pool.IPoolableObjectType,
   *      java.lang.Object, org.eclipse.core.runtime.IStatus)
   */
  public void objectLoaded( final IPoolableObjectType key,
      final Object newValue, final IStatus status )
  {
    if( status.isOK() && newValue != null )
    {
      final DefaultDiagramTemplateTheme theme = (DefaultDiagramTemplateTheme) m_key2themes
          .get( key );

      if( theme != null )
      {
        IObservation obs = (IObservation) newValue;
        
        if( m_factory != null )
        {
          try
          {
            obs = m_factory.proxyObservation( obs );
          }
          catch( SensorException e )
          {
            e.printStackTrace();
          }
        }
        
        theme.setObservation( obs );

        obs.addListener( this );

        // tricky: fake theme if no curves
        if( theme.getCurves().size() == 0 )
          addObservation( obs, theme.getArguments() );
        else
          addTheme( theme );
      }
      else
        m_key2themes.remove( key );
    }
  }

  /**
   * @see org.kalypso.util.pool.IPoolListener#objectInvalid(org.kalypso.util.pool.IPoolableObjectType,
   *      java.lang.Object)
   */
  public void objectInvalid( IPoolableObjectType key, Object oldValue )
  {
    final DefaultDiagramTemplateTheme theme = (DefaultDiagramTemplateTheme) m_key2themes
        .get( key );

    if( theme == null )
      return;

    removeTheme( theme );
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservationListener#observationChanged(org.kalypso.ogc.sensor.IObservation)
   */
  public void observationChanged( final IObservation obs )
  {
    final IDiagramTemplateTheme theme = findTheme( obs );

    if( theme != null )
      fireTemplateChanged( new TemplateEvent( theme, TemplateEvent.TYPE_REFRESH ) );
  }

  /**
   * Finds a theme for the given observation.
   * 
   * @param obs
   * @return theme or null if not found
   */
  public IDiagramTemplateTheme findTheme( final IObservation obs )
  {
    final Iterator it = getThemes().iterator();
    while( it.hasNext() )
    {
      final IDiagramTemplateTheme theme = (IDiagramTemplateTheme) it.next();

      if( theme.getObservation().equals( obs ) )
        return theme;
    }

    return null;
  }

  /**
   * Sets the proxy factory to use when observations are resolved. Clients
   * can set a custom factory so that observations can be extended by
   * functionality provided in the proxy-observation.
   * <p>
   * By default, the factory for this class is null. At every time, you can 
   * reset the factory to null, in that case no factory will be used.
   * 
   * @param factory [null allowed]
   */
  public void setProxyFactory( final IProxyFactory factory )
  {
    m_factory = factory;
  }
}