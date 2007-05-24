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
package org.kalypso.ui.editor.mapeditor.actiondelegates;

import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.IWorkbenchPartSite;
import org.kalypso.commons.command.ICommand;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypsodeegree.model.feature.event.ModellEventProvider;

/**
 * Decorates a part (normally the map editor or the map view) in order to access the map panel or post commands.
 * 
 * @author Gernot Belger
 */
public class WidgetActionPart implements ICommandTarget
{
  private final IWorkbenchPart m_part;

  public WidgetActionPart( final IWorkbenchPart part )
  {
    m_part = part;
  }

  public MapPanel getMapPanel( )
  {
    if( m_part == null )
      return null;

    return (MapPanel) m_part.getAdapter( MapPanel.class );
  }

  public ICommandTarget getCommandTarget( )
  {
    if( m_part == null )
      return null;

    if( m_part instanceof ICommandTarget )
      return (ICommandTarget) m_part;

    return (ICommandTarget) m_part.getAdapter( ICommandTarget.class );
  }

  /**
   * @see org.kalypso.commons.command.ICommandTarget#postCommand(org.kalypso.commons.command.ICommand,
   *      java.lang.Runnable)
   */
  public void postCommand( final ICommand command, final Runnable runnable )
  {
    final ICommandTarget commandTarget = getCommandTarget();
    if( commandTarget == null )
      return;

    commandTarget.postCommand( command, runnable );
  }

  public ModellEventProvider getModellEventProvider( )
  {
    if( m_part == null )
      return null;

    return (ModellEventProvider) m_part.getAdapter( ModellEventProvider.class );
  }

  public IWorkbenchPartSite getSite( )
  {
    if( m_part == null )
      return null;

    return m_part.getSite();
  }

  public IWorkbenchPart getPart( )
  {
    return m_part;
  }

  public CommandableWorkspace getCommandableWorkspace( )
  {
    if( m_part == null )
      return null;

    return (CommandableWorkspace) m_part.getAdapter( CommandableWorkspace.class );
  }

  public Control getControl( )
  {
    if( m_part == null )
      return null;

    return (Control) m_part.getAdapter( Control.class );
  }
}
