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
package org.kalypso.ui.editor.diagrameditor.actions;

import org.eclipse.jface.window.Window;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.contribs.eclipse.jface.action.FullAction;
import org.kalypso.ogc.sensor.diagview.DiagView;
import org.kalypso.ogc.sensor.template.ObsViewItem;
import org.kalypso.ui.ImageProvider;
import org.kalypso.ui.editor.abstractobseditor.ObservationEditorOutlinePage;

/**
 * Allows the user to set the axis-types that should be ignored when displaying observation items
 * 
 * @author schlienger
 */
public class EditDiagCurveAction extends FullAction
{
  private static final String TITLE = "Unsichtbare Achsen setzen";

  private final static String MSG = "Wählen Sie die Achsentypen die nicht dargestellt werden";

  private ObservationEditorOutlinePage m_page;

  public EditDiagCurveAction( final ObservationEditorOutlinePage page )
  {
    super( TITLE, ImageProvider.IMAGE_OBSVIEW_CURVE_PROPERTIES, "Erlaubt die Eigenschaften des Themas zu bearbeiten" );

    m_page = page;
  }

  public void run()
  {
    final Shell shell = m_page.getSite().getShell();

    final DiagView obsView = (DiagView)m_page.getView();
    final ObsViewItem[] items = m_page.getSelectedItems();

    final LineProperties lineProperties = determineLineProperties( items );

    final EditDiagCurveDialog dialog = new EditDiagCurveDialog( shell, lineProperties );
    if( dialog.open() == Window.OK )
    {
      final LineProperties result = dialog.getLineProperties();
      for( int i = 0; i < items.length; i++ )
      applyLineProperties( items[i], result );
      
      // TODO: update diagramm?
    }
  }

  private void applyLineProperties( ObsViewItem item, final LineProperties result )
  {
    

  //    item.setName(TITLE);
    
    // TODO Auto-generated method stub
    
  }

  /**
   * @param items
   * @return
   */
  private LineProperties determineLineProperties( ObsViewItem[] items )
  {
    // TODO Auto-generated method stub
    return null;
  }
}