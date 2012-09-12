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
package org.kalypso.kalypsomodel1d2d.ui.map.channeledit;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.jface.action.Action;
import org.eclipse.swt.widgets.Event;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.kalypso.ogc.gml.widgets.IWidget;
import org.kalypso.ui.views.map.MapView;

/**
 * @author Gernot Belger
 */
class SetWidgetAction extends Action
{
  private final IWidget m_widget;

  private final ChannelEditData m_data;

  public SetWidgetAction( final ChannelEditData data, final IWidget widget )
  {
    super( StringUtils.EMPTY, Action.AS_CHECK_BOX );

    m_data = data;
    m_widget = widget;
  }

  @Override
  public void runWithEvent( final Event event )
  {
    final IWidget delegate = m_data.getDelegate();

    if( delegate == m_widget )
      m_data.setDelegate( null );
    else
      m_data.setDelegate( m_widget );

    // REMARK: give focus to map pane now: the tools need key stroke (e.g. SPACE), but if the focus stays at the button,
    // space will deactive the tool again.
    final IWorkbenchWindow window = PlatformUI.getWorkbench().getActiveWorkbenchWindow();
    if( window == null )
      return;

    final IWorkbenchPage page = window.getActivePage();
    if( page == null )
      return;

    try
    {
      page.showView( MapView.ID, null, IWorkbenchPage.VIEW_ACTIVATE );
    }
    catch( final PartInitException e )
    {
      e.printStackTrace();
    }
  }

  public IWidget getDelegate( )
  {
    return m_widget;
  }
}