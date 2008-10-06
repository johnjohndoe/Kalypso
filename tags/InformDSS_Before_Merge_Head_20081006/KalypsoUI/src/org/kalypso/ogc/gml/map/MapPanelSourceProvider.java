/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 * 
 *  and
 * 
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 * 
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 * 
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 * 
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 * 
 *  Contact:
 * 
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 * 
 *  ---------------------------------------------------------------------------*/
package org.kalypso.ogc.gml.map;

import java.util.Collection;
import java.util.HashSet;
import java.util.Map;
import java.util.TreeMap;

import org.eclipse.core.commands.contexts.Context;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.ISchedulingRule;
import org.eclipse.ui.AbstractSourceProvider;
import org.eclipse.ui.contexts.IContextActivation;
import org.eclipse.ui.contexts.IContextService;
import org.eclipse.ui.handlers.IHandlerService;
import org.eclipse.ui.menus.IMenuService;
import org.eclipse.ui.progress.UIJob;
import org.eclipse.ui.services.IEvaluationService;
import org.eclipse.ui.services.IServiceLocator;
import org.eclipse.ui.services.IServiceWithSources;
import org.kalypso.contribs.eclipse.core.runtime.jobs.MutexRule;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.map.listeners.IMapPanelListener;
import org.kalypso.ogc.gml.map.listeners.MapPanelAdapter;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.mapmodel.IMapModellListener;
import org.kalypso.ogc.gml.mapmodel.MapModellAdapter;

/**
 * Manages context and sources corresponding to the map.<br>
 * As soon as the manager is created on a mapPanel, the map-context is activated and registered with the given
 * serviceLocator. Also, the mapPanel is provides as source via the evaluation context.<br>
 * Additional: the activeTheme is always registered as some kind of dynamic context. This should be changed; handlers,
 * that are at the moment registered against this context should register against the mapContext, and test the active
 * theme via the corresponding property test (org.kalypso.ui.activeThemeQName).
 * 
 * @author Stefan Kurzbach
 * @author Gernot Belger
 */
public class MapPanelSourceProvider extends AbstractSourceProvider
{
  public static final String MAP_CONTEXT = "org.kalypso.ogc.gml.map.context"; //$NON-NLS-1$

  public static final String ACTIVE_MAPPANEL_NAME = "activeMapPanel";

  private static final String[] PROVIDED_SOURCE_NAMES = new String[] { ACTIVE_MAPPANEL_NAME };

  protected final IMapModellListener m_mapModellListener = new MapModellAdapter()
  {
    /**
     * @see org.kalypso.ogc.gml.mapmodel.MapModellAdapter#themeActivated(org.kalypso.ogc.gml.mapmodel.IMapModell,
     *      org.kalypso.ogc.gml.IKalypsoTheme, org.kalypso.ogc.gml.IKalypsoTheme)
     */
    @Override
    public void themeActivated( final IMapModell source, final IKalypsoTheme previouslyActive, final IKalypsoTheme nowActive )
    {
      activateThemeContext( nowActive );
    }

    /**
     * @see org.kalypso.ogc.gml.mapmodel.MapModellAdapter#themeContextChanged(org.kalypso.ogc.gml.mapmodel.IMapModell,
     *      org.kalypso.ogc.gml.IKalypsoTheme)
     */
    @Override
    public void themeContextChanged( final IMapModell source, final IKalypsoTheme theme )
    {
      activateThemeContext( theme );
    }
  };

  private final IMapPanelListener m_mapPanelListener = new MapPanelAdapter()
  {
    /**
     * @see org.kalypso.ogc.gml.map.listeners.MapPanelAdapter#onMapModelChanged(org.kalypso.ogc.gml.map.MapPanel,
     *      org.kalypso.ogc.gml.mapmodel.IMapModell, org.kalypso.ogc.gml.mapmodel.IMapModell)
     */
    @Override
    public void onMapModelChanged( final IMapPanel source, final IMapModell oldModel, final IMapModell newModel )
    {
      if( oldModel != null )
        oldModel.removeMapModelListener( m_mapModellListener );
      if( newModel != null )
        newModel.addMapModelListener( m_mapModellListener );

      final IKalypsoTheme activeTheme = newModel == null ? null : newModel.getActiveTheme();
      activateThemeContext( activeTheme );
    }
  };

  /**
   * This collection contains all services, with which this provider has been registered. Used in order to correctly
   * unregister.
   */
  private final Collection<IServiceWithSources> m_registeredServices = new HashSet<IServiceWithSources>();

  /** Ensures, that the context are activated in the same order as the themes are activated. */
  private final ISchedulingRule m_muteRule = new MutexRule();

  private IMapPanel m_mapPanel;

  private final IContextService m_contextService;

  private IContextActivation m_mapPanelContext;

  private IContextActivation m_themeContext;

  /**
   * Creates a new MapPanelSourceProvider on the given service locator.
   */
  public MapPanelSourceProvider( final IServiceLocator serviceLocator )
  {
    this( serviceLocator, null );
  }

  /**
   * Creates a new MapPanelSourceProvider on the given MapPanel.<br>
   * Initialises it state with the given parameters.
   */
  public MapPanelSourceProvider( final IServiceLocator serviceLocator, final IMapPanel mapPanel )
  {
    m_mapPanel = mapPanel;

    m_contextService = (IContextService) registerServiceWithSources( serviceLocator, IContextService.class );
    registerServiceWithSources( serviceLocator, IEvaluationService.class );
    registerServiceWithSources( serviceLocator, IHandlerService.class );
    registerServiceWithSources( serviceLocator, IMenuService.class );

    m_mapPanelContext = m_contextService.activateContext( MAP_CONTEXT );

    m_mapPanel.addMapPanelListener( m_mapPanelListener );
    m_mapPanelListener.onMapModelChanged( m_mapPanel, null, m_mapPanel.getMapModell() );
  }

  private IServiceWithSources registerServiceWithSources( final IServiceLocator serviceLocator, final Class< ? extends IServiceWithSources> serviceClass )
  {
    final IServiceWithSources service = (IServiceWithSources) serviceLocator.getService( serviceClass );
    if( service == null )
      return null;

    service.addSourceProvider( this );

    m_registeredServices.add( service );

    return service;
  }

  /**
   * @see org.eclipse.ui.ISourceProvider#dispose()
   */
  public void dispose( )
  {
    m_mapPanel.removeMapPanelListener( m_mapPanelListener );
    m_mapPanelListener.onMapModelChanged( m_mapPanel, m_mapPanel.getMapModell(), null );

    m_mapPanel = null;

// for( final IServiceWithSources service : m_registeredServices )
// service.removeSourceProvider( this );

    if( m_mapPanelContext != null )
      m_contextService.deactivateContext( m_mapPanelContext );

    if( m_themeContext != null )
      m_contextService.deactivateContext( m_themeContext );
  }

  /**
   * @see org.eclipse.ui.ISourceProvider#getCurrentState()
   */
  public Map< ? , ? > getCurrentState( )
  {
    final Map<String, Object> currentState = new TreeMap<String, Object>();
    currentState.put( ACTIVE_MAPPANEL_NAME, m_mapPanel );
    return currentState;
  }

  /**
   * @see org.eclipse.ui.ISourceProvider#getProvidedSourceNames()
   */
  public String[] getProvidedSourceNames( )
  {
    return PROVIDED_SOURCE_NAMES;
  }

  protected void activateThemeContext( final IKalypsoTheme activeTheme )
  {
    final UIJob job = new UIJob( "Activate theme context job" )
    {
      @Override
      public IStatus runInUIThread( final IProgressMonitor monitor )
      {
        activateThemeContextSWT( activeTheme );
        return Status.OK_STATUS;
      }
    };
    job.setRule( m_muteRule );
    job.setSystem( true );
    job.schedule();
  }

  /**
   * Same as {@link #activateThemeContext(IKalypsoTheme)}, but MUST run in the swt thread.
   */
  protected void activateThemeContextSWT( final IKalypsoTheme activeTheme )
  {
    if( m_themeContext != null )
      m_contextService.deactivateContext( m_themeContext );

    m_themeContext = null;

    if( activeTheme != null )
    {
      final String contextId = activeTheme.getTypeContext();
      final Context context = m_contextService.getContext( contextId );
      if( !context.isDefined() )
      {
        final String parentId = MAP_CONTEXT;
        context.define( contextId, contextId, parentId );
      }

      m_themeContext = m_contextService.activateContext( contextId );

      // REMARK: in order to update the command contributions, we
      // activate/deactivate the current map-context.
      // This is better, as all items should be completely update()'d
      // ( instead of just refreshed via an UIElement updater.
      // This is for example necessary for the NewFeature-DropDown.
      // There is probably a better way to do this, which one?
      if( m_mapPanelContext != null )
      {
        m_contextService.deactivateContext( m_mapPanelContext );
        m_mapPanelContext = m_contextService.activateContext( MAP_CONTEXT );
      }
    }
  }

}
