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
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.swt.widgets.Event;
import org.kalypso.i18n.Messages;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.command.ChangeExtentCommand;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypso.ogc.gml.mapmodel.IMapModellView;
import org.kalypso.ogc.gml.mapmodel.MapModellHelper;
import org.kalypso.ui.editor.mapeditor.GisMapOutlinePage;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree_impl.tools.GeometryUtilities;

/**
 * @author Thomas Jung
 */
public class ZoomToSelectedLayer extends MapModellViewActionDelegate
{
  /**
   * @see org.eclipse.ui.actions.ActionDelegate#runWithEvent(org.eclipse.jface.action.IAction,
   *      org.eclipse.swt.widgets.Event)
   */
  @Override
  public void runWithEvent( final IAction action, final Event event )
  {
    final IKalypsoTheme[] selectedThemes = getSelectedThemes( getSelection() );
    final IMapPanel panel = getView().getMapPanel();

    if( panel != null && selectedThemes.length > 0 )
    {
      final GM_Envelope zoomBox = MapModellHelper.calculateExtent( selectedThemes, null );
      if( zoomBox == null )
      {
        MessageDialog.openWarning( event.display.getActiveShell(), action.getText(), Messages.getString( "org.kalypso.ogc.gml.outline.ZoomToSelectedLayer.0" ) );
        return;
      }

      final GM_Envelope wishBBox = GeometryUtilities.scaleEnvelope( zoomBox, 1.05 );

      final IMapModellView view = getView();
      final ChangeExtentCommand changeExtentCommand = new ChangeExtentCommand( panel, wishBBox );

      if( view instanceof GisMapOutlineView )
      {
        final GisMapOutlineView gisView = (GisMapOutlineView) view;
        gisView.postCommand( changeExtentCommand, null );
      }
      else if( view instanceof GisMapOutlinePage )
      {
        final GisMapOutlinePage page = (GisMapOutlinePage) view;
        page.postCommand( changeExtentCommand, null );
      }
      else
        panel.setBoundingBox( wishBBox );
    }
  }

  /**
   * @see org.kalypso.ogc.gml.outline.MapModellViewActionDelegate#selectionChanged(org.eclipse.jface.action.IAction,
   *      org.eclipse.jface.viewers.ISelection)
   */
  @Override
  public void selectionChanged( final IAction action, final ISelection selection )
  {
    super.selectionChanged( action, selection );

    action.setEnabled( getSelectedThemes( getSelection() ) != null );
  }

}
