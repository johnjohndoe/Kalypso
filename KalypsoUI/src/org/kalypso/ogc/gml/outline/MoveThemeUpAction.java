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
import org.kalypso.commons.list.IListManipulator;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.mapmodel.IMapModellView;

/**
 * @author Stefan Kurzbach
 */
public class MoveThemeUpAction extends MapModellViewActionDelegate
{
  /**
   * @see org.eclipse.ui.actions.ActionDelegate#run(org.eclipse.jface.action.IAction)
   */
  @Override
  public void run( final IAction action )
  {
    IListManipulator listManipulator = getView();
    IKalypsoTheme selectedElement = getSelectedTheme();
    if( listManipulator != null && selectedElement != null )
    {
      listManipulator.moveElementUp( selectedElement );
    }
  }

  /**
   * @see org.kalypso.ogc.gml.outline.MapModellViewActionDelegate#selectionChanged(org.eclipse.jface.action.IAction,
   *      org.eclipse.jface.viewers.ISelection)
   */
  @Override
  public void selectionChanged( IAction action, ISelection selection )
  {
    super.selectionChanged( action, selection );
    final IKalypsoTheme selectedTheme = getSelectedTheme();
    boolean bEnable = false;
    final IMapModellView view = getView();
    if( selectedTheme != null && view != null )
    {
      final IMapModell mapModell = view.getMapPanel().getMapModell();
      if( mapModell != null )
      {
        final Object[] elements = mapModell.getAllThemes();
        bEnable = elements[0] != selectedTheme;
      }
    }
    action.setEnabled( bEnable );
  }
}
