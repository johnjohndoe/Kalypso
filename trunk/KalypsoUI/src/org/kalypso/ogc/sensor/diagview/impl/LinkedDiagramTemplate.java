package org.kalypso.ogc.sensor.diagview.impl;

import java.net.URL;
import java.util.Iterator;
import java.util.List;
import java.util.TreeMap;

import org.eclipse.core.runtime.IStatus;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.diagview.IDiagramTemplateTheme;
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
    IPoolListener
{
  private final ResourcePool m_pool;

  private final TreeMap m_key2themes;

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

    for( final Iterator it = obsDiagView.getAxis().iterator(); it.hasNext(); )
    {
      final TypeAxis baseAxis = (TypeAxis) it.next();

      addAxis( new DiagramAxis( baseAxis ) );
    }

    final List list = obsDiagView.getObservation();
    for( final Iterator it = list.iterator(); it.hasNext(); )
    {
      final TypeObservation tobs = (TypeObservation) it.next();

      // no observation yet, will be updated once loaded
      final LinkedDiagramTemplateTheme theme = new LinkedDiagramTemplateTheme( this, tobs );

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
   * Convenienve method for adding an observation to this template.
   *
   * TODO: use themeName as name for the curve of the observation...
   *  
   * @param themeName
   * @param context
   * @param href
   * @param linktype
   * @param args
   */
  public void addObservation( final String themeName, final URL context, final String href, final String linktype, final IVariableArguments args )
  {
    // create key according to observation link
    final PoolableObjectType key = new PoolableObjectType( linktype, href, context );

    // fake theme because it won't be added directly to this template
    final DefaultDiagramTemplateTheme fakeTheme = new DefaultDiagramTemplateTheme();
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

    super.removeAllThemes();
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
    
    super.removeTheme( theme );
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

      final IObservation obs = (IObservation) newValue;

      theme.setObservation( obs );
      
      // tricky: fake theme if no curves
      if( theme.getCurves().size() == 0 )
        addObservation( obs, theme.getArguments() );
      else
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