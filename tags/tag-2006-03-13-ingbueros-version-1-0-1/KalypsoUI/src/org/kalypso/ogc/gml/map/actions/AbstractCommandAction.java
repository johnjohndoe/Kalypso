/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
 
 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.ogc.gml.map.actions;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.resource.ImageDescriptor;
import org.kalypso.commons.command.ICommand;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.ogc.gml.map.MapPanel;

/**
 * @author belger
 */
public abstract class AbstractCommandAction extends Action
{
  private final MapPanel m_mapPanel;

  private final ICommandTarget m_commandTarget;

  public AbstractCommandAction( final ICommandTarget commandTarget, final MapPanel mapPanel, final String text,
      final ImageDescriptor imageDescriptor, final String tooltiptext )
  {
    super( text, AS_PUSH_BUTTON );

    setToolTipText( tooltiptext );
    setImageDescriptor( imageDescriptor );

    m_mapPanel = mapPanel;
    m_commandTarget = commandTarget;
  }

  protected abstract ICommand runInternal();

  /**
   * @see org.eclipse.jface.action.Action#run()
   */
  public final void run()
  {
    postCommand( runInternal(), null );
  }

  protected final MapPanel getMapPanel()
  {
    return m_mapPanel;
  }

  protected final void postCommand( final ICommand command, final Runnable runAfter )
  {
    m_commandTarget.postCommand( command, runAfter );
  }
}