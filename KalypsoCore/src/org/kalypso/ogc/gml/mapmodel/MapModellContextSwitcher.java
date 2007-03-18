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
package org.kalypso.ogc.gml.mapmodel;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Logger;

import org.eclipse.core.commands.contexts.Context;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.contexts.IContextActivation;
import org.eclipse.ui.contexts.IContextService;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.IKalypsoThemeListener;
import org.kalypso.ogc.gml.KalypsoThemeEvent;
import org.kalypsodeegree.model.feature.event.ModellEvent;
import org.kalypsodeegree.model.feature.event.ModellEventListener;
import org.kalypsodeegree.model.feature.event.ModellEventProvider;

/**
 * @author Stefan Kurzbach
 */
public class MapModellContextSwitcher implements ModellEventListener, IKalypsoThemeListener
{

  private final class ContextSwitcherThread extends Thread
  {
    private IContextService m_contextService;

    private IContextActivation m_oldContext;

    private IKalypsoTheme m_theme;

    ContextSwitcherThread( final IContextService contextService )
    {
      m_contextService = contextService;
    }

    public void setTheme( final IKalypsoTheme theme )
    {
      m_theme = theme;
    }

    /**
     * @see java.lang.Thread#run()
     */
    @Override
    public void run( )
    {
      // TODO: it sometimes happens thatz the old context is not deactivated, howmay this happen?
      // concurrent access problems?
      
      if( m_oldContext != null )
      {
        logger.info( "Deactivating context: " + m_oldContext.getContextId() );
        m_contextService.deactivateContext( m_oldContext );
      }
      if( m_theme == null )
      {
        return;
      }
      else
      {
        final String contextId = m_theme.getContext();
        final Context context = m_contextService.getContext( contextId );
        if( !context.isDefined() )
        {
          context.define( contextId, contextId, "org.eclipse.ui.contexts.window" );
        }
        logger.info( "Activating context: " + contextId );
        m_oldContext = m_contextService.activateContext( contextId );
        logger.info( "Active contexts: " + Arrays.deepToString( m_contextService.getActiveContextIds().toArray() ) );
      }
      super.run();
    }

    public void dispose( )
    {
      setTheme( null );
      final Display display = PlatformUI.getWorkbench().getDisplay();
      if( !display.isDisposed() )
      {
        display.asyncExec( this );
      }
      m_contextService = null;
      m_oldContext = null;
    }
  }

  static final Logger logger = Logger.getLogger( MapModellContextSwitcher.class.getName() );

  private final Collection<IKalypsoTheme> m_themes = new ArrayList<IKalypsoTheme>();

  private Map<IContextService, ContextSwitcherThread> m_contextSwitcherThreads = new HashMap<IContextService, ContextSwitcherThread>();

  public MapModellContextSwitcher( )
  {
  }

  public void addContextService( final IContextService contextService )
  {
    if( contextService != null )
    {
      m_contextSwitcherThreads.put( contextService, new ContextSwitcherThread( contextService ) );
    }
  }

  public void removeContextService( final IContextService contextService )
  {
    final ContextSwitcherThread thread = m_contextSwitcherThreads.remove( contextService );
    if( thread != null )
    {
      thread.dispose();
    }
  }

  /**
   * @see org.kalypsodeegree.model.feature.event.ModellEventListener#onModellChange(org.kalypsodeegree.model.feature.event.ModellEvent)
   */
  public void onModellChange( final ModellEvent modellEvent )
  {
    if( modellEvent == null )
    {
      return;
    }
    final ModellEventProvider eventSource = modellEvent.getEventSource();
    IMapModell mapModell = null;
    if( eventSource instanceof IMapModell )
    {
      mapModell = (IMapModell) eventSource;
    }
    else
    {
      // ignore,this must be wrong
      return;
    }

    if( modellEvent.isType( ModellEvent.THEME_ACTIVATED ) )
    {
      final IKalypsoTheme activeTheme = mapModell.getActiveTheme();
      activateContextFor( activeTheme );
    }
    else if( modellEvent.isType( ModellEvent.THEME_ADDED ) )
    {
      for( IKalypsoTheme theme : mapModell.getAllThemes() )
      {
        if( !m_themes.contains( theme ) )
        {
          m_themes.add( theme );
          theme.addKalypsoThemeListener( this );
        }
      }
    }
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoThemeListener#kalypsoThemeChanged(org.kalypso.ogc.gml.KalypsoThemeEvent)
   */
  public void kalypsoThemeChanged( final KalypsoThemeEvent event )
  {
    if( event.isType( KalypsoThemeEvent.CONTEXT_CHANGED ) )
    {
      final IKalypsoTheme theme = event.getSource();
      final IMapModell mapModell = theme.getMapModell();
      if( mapModell != null && mapModell.getActiveTheme() == theme )
        activateContextFor( theme );
    }
  }

  private synchronized void activateContextFor( final IKalypsoTheme theme )
  {
    final Display display = PlatformUI.getWorkbench().getDisplay();
    if( display.isDisposed() )
      return;

    for( final ContextSwitcherThread thread : m_contextSwitcherThreads.values() )
    {
      thread.setTheme( theme );
      display.asyncExec( thread );
    }
  }

  public void dispose( )
  {
    activateContextFor( null );
    for( ContextSwitcherThread thread : m_contextSwitcherThreads.values() )
    {
      thread.dispose();
    }
    for( IKalypsoTheme theme : m_themes )
    {
      theme.removeKalypsoThemeListener( this );
    }
    m_themes.clear();
  }
}
