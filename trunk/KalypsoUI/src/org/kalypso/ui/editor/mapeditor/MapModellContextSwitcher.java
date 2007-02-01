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
package org.kalypso.ui.editor.mapeditor;

import org.eclipse.core.commands.contexts.Context;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.contexts.IContextActivation;
import org.eclipse.ui.contexts.IContextService;
import org.kalypso.ogc.gml.mapmodel.MapModell;
import org.kalypsodeegree.model.feature.event.ModellEvent;
import org.kalypsodeegree.model.feature.event.ModellEventListener;

/**
 * @author Stefan Kurzbach
 */
public class MapModellContextSwitcher implements ModellEventListener
{
  IContextActivation m_activeContextHandle;

  IContextService m_contextService;

  /**
   * @see org.kalypsodeegree.model.feature.event.ModellEventListener#onModellChange(org.kalypsodeegree.model.feature.event.ModellEvent)
   */
  public void onModellChange( final ModellEvent modellEvent )
  {
    PlatformUI.getWorkbench().getDisplay().syncExec( new Runnable()
    {
      public void run( )
      {
        if( modellEvent != null && modellEvent.isType( ModellEvent.THEME_ACTIVATED ) )
        {
          if( m_activeContextHandle != null )
          {
            m_activeContextHandle.getContextService().deactivateContext( m_activeContextHandle );
          }
          if( m_contextService == null )
          {
            m_contextService = (IContextService) PlatformUI.getWorkbench().getService( IContextService.class );
          }
          final MapModell map = (MapModell) modellEvent.getEventSource();
          final String themeName = map.getActiveTheme().getName();
          final Context context = m_contextService.getContext( themeName );
          if( !context.isDefined() )
          {
            context.define( themeName, themeName, "org.eclipse.ui.contexts.window" );
          }
          m_activeContextHandle = m_contextService.activateContext( context.getId() );
          // System.out.println( "active contexts: " + Arrays.deepToString(
          // m_contextService.getActiveContextIds().toArray() ) );
        }
      }
    } );
  }
}
