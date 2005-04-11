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
package org.kalypso.ui.editor.mapeditor.actiondelegates;

import org.eclipse.jface.action.IAction;
import org.eclipse.ui.IEditorPart;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.map.widgets.EditFeatureWidget;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ui.editor.mapeditor.GisMapEditor;

/**
 * @author belger
 */
public class EditFeatureWidgetDelegate extends AbstractWidgetActionDelegate
{
  public EditFeatureWidgetDelegate()
  {
    super( new EditFeatureWidget( "edit feature", "" ) );
  }

  /**
   * @see org.kalypso.ui.editor.AbstractGisEditorActionDelegate#setActiveEditor(org.eclipse.jface.action.IAction,
   *      org.eclipse.ui.IEditorPart)
   */
  public void setActiveEditor( IAction action, IEditorPart targetEditor )
  {
    super.setActiveEditor( action, targetEditor );
    final EditFeatureWidget featureWidget = (EditFeatureWidget)getWidget();
    if( targetEditor != null )
      featureWidget.setShell( getEditor().getEditorSite().getShell() );
  }

  /**
   * @see org.eclipse.ui.IActionDelegate#run(org.eclipse.jface.action.IAction)
   */
  public void run( final IAction action )
  {
    final GisMapEditor editor = (GisMapEditor)getEditor();
    editor.getMapPanel().getWidgetManager().changeWidget( getWidget() );
  }

  /**
   * @see org.kalypso.ui.editor.mapeditor.actiondelegates.AbstractWidgetActionDelegate#refreshEnabled()
   */
  public void refreshEnabled()
  {
    boolean enabled = false;
    final GisMapEditor editor = (GisMapEditor)getEditor();
    IAction action = getAction();
    if( editor != null )
    {
      final MapPanel mapPanel = editor.getMapPanel();
      IMapModell mapModell = mapPanel.getMapModell();
      if( mapModell != null && mapModell.getThemeSize() > 0 )
        enabled = true;
    }
    action.setEnabled( enabled );
  }
}

