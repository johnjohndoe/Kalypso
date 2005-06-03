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
package org.kalypso.ui.editor.abstractobseditor.actions;

import org.eclipse.jface.dialogs.MessageDialog;
import org.kalypso.eclipse.jface.action.FullAction;
import org.kalypso.ogc.sensor.commands.RemoveThemeCommand;
import org.kalypso.ogc.sensor.template.ObsViewItem;
import org.kalypso.ui.ImageProvider;
import org.kalypso.ui.editor.abstractobseditor.ObservationEditorOutlinePage;

/**
 * RemoveThemeAction
 * 
 * @author schlienger
 */
public class RemoveThemeAction extends FullAction
{
  private final ObservationEditorOutlinePage m_page;

  public RemoveThemeAction( final ObservationEditorOutlinePage page )
  {
    super( "Thema entfernen", ImageProvider.IMAGE_MAPVIEW_OUTLINE_REMOVE,
        "Entfernt aktives Thema" );

    m_page = page;
  }

  /**
   * @see org.eclipse.jface.action.Action#run()
   */
  public void run()
  {
    final ObsViewItem item = m_page.getSelectedItem();

    if( item != null
        && MessageDialog.openConfirm( m_page.getSite().getShell(),
            "Zeitreihe entfernen", "Wollen Sie wirklich die Zeitreihe "
                + item.getName() + " entfernen" ) )

      m_page.getEditor().postCommand(
          new RemoveThemeCommand( m_page.getView(), item ), null );
  }
}