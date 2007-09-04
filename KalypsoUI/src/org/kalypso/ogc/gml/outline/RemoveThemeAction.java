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
package org.kalypso.ogc.gml.outline;

import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.swt.widgets.Event;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.command.RemoveThemeCommand;
import org.kalypso.ogc.gml.mapmodel.IMapModellView;

/**
 * @author Gernot Belger
 */
public class RemoveThemeAction extends MapModellViewActionDelegate
{
  /**
   * @see org.eclipse.ui.actions.ActionDelegate#runWithEvent(org.eclipse.jface.action.IAction,
   *      org.eclipse.swt.widgets.Event)
   */
  @Override
  public void runWithEvent( final IAction action, final Event event )
  {
    final IKalypsoTheme[] selectedThemes = MapModellViewActionDelegate.getSelectedThemes( getSelection() );
    for( final IKalypsoTheme theme : selectedThemes )
      removeElement( theme );
  }

  protected void removeElement( final IKalypsoTheme theme )
  {
    final IMapModellView view = getView();
    if( view != null )
      view.postCommand( new RemoveThemeCommand( theme.getMapModell(), theme ), null );
  }

  /**
   * @see org.eclipse.ui.IActionDelegate#selectionChanged(org.eclipse.jface.action.IAction,
   *      org.eclipse.jface.viewers.ISelection)
   */
  @Override
  public void selectionChanged( final IAction action, final ISelection selection )
  {
    super.selectionChanged( action, selection );

    final IKalypsoTheme[] selectedThemes = MapModellViewActionDelegate.getSelectedThemes( getSelection() );

    final boolean allDeleteable = determineDeleteable( selectedThemes );

    final boolean enabled = (selectedThemes.length > 0) && allDeleteable;
    action.setEnabled( enabled );
  }

  private boolean determineDeleteable( final IKalypsoTheme[] selectedThemes )
  {
    for( final IKalypsoTheme kalypsoTheme : selectedThemes )
      if( !kalypsoTheme.getProperty( IKalypsoTheme.PROPERTY_DELETEABLE ) )
        return false;

    return true;
  }
}