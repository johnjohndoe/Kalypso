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

import java.util.Map;
import java.util.TreeMap;

import org.eclipse.ui.AbstractSourceProvider;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.contexts.IContextService;
import org.eclipse.ui.handlers.IHandlerService;
import org.eclipse.ui.internal.services.IEvaluationService;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.map.listeners.IMapPanelListener;
import org.kalypso.ogc.gml.map.listeners.MapPanelAdapter;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.mapmodel.IMapModellListener;
import org.kalypso.ogc.gml.mapmodel.MapModellAdapter;

/**
 * Provides notifications when the active map panel and its internal properties change.
 * 
 * @author kurzbach
 */
public class MapPanelSourceProvider extends AbstractSourceProvider
{

  public static final String ACTIVE_MAPPANEL_NAME = "activeMapPanel";

  public static final String ACTIVE_THEME_NAME = "activeTheme";

  private static final String[] PROVIDED_SOURCE_NAMES = new String[] { ACTIVE_MAPPANEL_NAME, ACTIVE_THEME_NAME };

  private static MapPanelSourceProvider m_instance = null;

  protected final IMapModellListener m_MapModellListener = new MapModellAdapter()
  {

    /**
     * @see org.kalypso.ogc.gml.mapmodel.MapModellAdapter#themeActivated(org.kalypso.ogc.gml.mapmodel.IMapModell,
     *      org.kalypso.ogc.gml.IKalypsoTheme, org.kalypso.ogc.gml.IKalypsoTheme)
     */
    @Override
    public void themeActivated( final IMapModell source, final IKalypsoTheme previouslyActive, final IKalypsoTheme nowActive )
    {
      checkActiveTheme();
    }
  };

  private final IMapPanelListener m_MapPanelListener = new MapPanelAdapter()
  {
    /**
     * @see org.kalypso.ogc.gml.map.listeners.MapPanelAdapter#onMapModelChanged(org.kalypso.ogc.gml.map.MapPanel,
     *      org.kalypso.ogc.gml.mapmodel.IMapModell, org.kalypso.ogc.gml.mapmodel.IMapModell)
     */
    @Override
    public void onMapModelChanged( final MapPanel source, final IMapModell oldModel, final IMapModell newModel )
    {
      if( oldModel != null )
        oldModel.removeMapModelListener( m_MapModellListener );
      if( newModel != null )
        newModel.addMapModelListener( m_MapModellListener );
      checkActiveTheme();
    }
  };

  private MapPanel m_activeMapPanel = null;

  private IKalypsoTheme m_activeTheme = null;

  /**
   * Creates a new MapPanelSourceProvider on the given MapPanel
   */
  public MapPanelSourceProvider( )
  {
    registerSourceProviders();
  }

  @SuppressWarnings("unchecked")
  protected final void checkActiveTheme( )
  {
    final Map currentState = getCurrentState();

    final Object newActiveTheme = currentState.get( ACTIVE_THEME_NAME );
    if( newActiveTheme != m_activeTheme )
    {
      m_activeTheme = (IKalypsoTheme) newActiveTheme;
      fireSourceChanged( 0, ACTIVE_THEME_NAME, m_activeTheme );
    }
  }

  /**
   * Call this method if some map panel thinks it is the active one
   */
  public void setActiveMapPanel( final MapPanel mapPanel )
  {
    if( m_activeMapPanel != mapPanel )
    {
      if( m_activeMapPanel != null )
      {
        m_MapPanelListener.onMapModelChanged( m_activeMapPanel, null, null );
        m_activeMapPanel.removeMapPanelListener( m_MapPanelListener );
      }

      m_activeMapPanel = mapPanel;

      if( m_activeMapPanel != null )
      {
        m_activeMapPanel.addMapPanelListener( m_MapPanelListener );
        m_MapPanelListener.onMapModelChanged( m_activeMapPanel, null, m_activeMapPanel.getMapModell() );
      }
      fireSourceChanged( 0, ACTIVE_MAPPANEL_NAME, m_activeMapPanel );
      checkActiveTheme();
    }
  }

  public void unsetActiveMapPanel( final MapPanel mapPanel )
  {
    if( m_activeMapPanel == mapPanel )
    {
      if( m_activeMapPanel != null )
      {
        m_MapPanelListener.onMapModelChanged( m_activeMapPanel, null, null );
        m_activeMapPanel.removeMapPanelListener( m_MapPanelListener );
      }
      m_activeMapPanel = null;
      fireSourceChanged( 0, ACTIVE_MAPPANEL_NAME, m_activeMapPanel );
      checkActiveTheme();
    }
  }

  /**
   * @see org.eclipse.ui.ISourceProvider#dispose()
   */
  public void dispose( )
  {
    unregisterSourceProviders();
    m_activeMapPanel = null;
    m_activeTheme = null;
  }

  /**
   * @see org.eclipse.ui.ISourceProvider#getCurrentState()
   */
  @SuppressWarnings("unchecked")
  public Map getCurrentState( )
  {
    final Map currentState = new TreeMap();
    currentState.put( ACTIVE_MAPPANEL_NAME, m_activeMapPanel );
    final IKalypsoTheme activeMapTheme = getActiveMapTheme( m_activeMapPanel );
    currentState.put( ACTIVE_THEME_NAME, activeMapTheme );
    return currentState;
  }

  private IKalypsoTheme getActiveMapTheme( final MapPanel activeMapPanel )
  {
    if( activeMapPanel == null )
      return null;
    final IMapModell mapModell = activeMapPanel.getMapModell();
    if( mapModell == null )
      return null;
    return mapModell.getActiveTheme();
  }

  /**
   * @see org.eclipse.ui.ISourceProvider#getProvidedSourceNames()
   */
  public String[] getProvidedSourceNames( )
  {
    return PROVIDED_SOURCE_NAMES;
  }

  public static MapPanelSourceProvider getInstance( )
  {
    if( m_instance == null )
      m_instance = new MapPanelSourceProvider();
    return m_instance;
  }

  @SuppressWarnings("restriction")
  private void registerSourceProviders( )
  {
    if( PlatformUI.isWorkbenchRunning() )
    {
      final IWorkbench workbench = PlatformUI.getWorkbench();
      final IEvaluationService evaluationService = (IEvaluationService) workbench.getService( IEvaluationService.class );
      if( evaluationService != null )
        evaluationService.addSourceProvider( this );

      final IContextService contextService = (IContextService) workbench.getService( IContextService.class );
      if( contextService != null )
        contextService.addSourceProvider( this );

      final IHandlerService handlerService = (IHandlerService) workbench.getService( IHandlerService.class );
      if( handlerService != null )
        handlerService.addSourceProvider( this );
    }
  }

  @SuppressWarnings("restriction")
  private void unregisterSourceProviders( )
  {
    final IWorkbench workbench = PlatformUI.getWorkbench();
    if( workbench != null && !workbench.isClosing() )
    {
      final IEvaluationService evaluationService = (IEvaluationService) workbench.getService( IEvaluationService.class );
      if( evaluationService != null )
        evaluationService.removeSourceProvider( this );

      final IContextService contextService = (IContextService) workbench.getService( IContextService.class );
      if( contextService != null )
        contextService.removeSourceProvider( this );

      final IHandlerService handlerService = (IHandlerService) workbench.getService( IHandlerService.class );
      if( handlerService != null )
        handlerService.removeSourceProvider( this );
    }
  }

}
