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
package org.kalypso.ui.editor.obstableeditor.actions;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.kalypso.eclipse.jface.action.FullAction;
import org.kalypso.ogc.sensor.commands.RemoveThemeCommand;
import org.kalypso.ogc.sensor.tableview.TableViewTheme;
import org.kalypso.ui.ImageProvider;
import org.kalypso.ui.editor.obstableeditor.ObsTableOutlinePage;

/**
 * RemoveThemeAction
 * 
 * @author schlienger
 */
public class RemoveThemeAction extends FullAction implements
    ISelectionChangedListener
{
  private ObsTableOutlinePage m_page;

  public RemoveThemeAction( ObsTableOutlinePage page )
  {
    super( "Spalte entfernen", ImageProvider.IMAGE_MAPVIEW_OUTLINE_REMOVE,
        "Entfernt aktive Spalte" );

    m_page = page;

    m_page.addSelectionChangedListener( this );
  }

  public void dispose( )
  {
    m_page.removeSelectionChangedListener( this );
  }

  /**
   * @see org.eclipse.jface.action.Action#run()
   */
  public void run( )
  {
    final TableViewTheme selectedTheme = m_page.getSelectedTheme();

    if( selectedTheme != null
        && MessageDialog.openConfirm( m_page.getSite().getShell(),
            "Zeitreihe entfernen", "Wollen Sie wirklich die Zeitreihe "
                + selectedTheme.getName() + " entfernen" ) )
      
      m_page.getEditor().postCommand( new RemoveThemeCommand( m_page.getTemplate(), selectedTheme ), null );
  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionChangedListener#selectionChanged(org.eclipse.jface.viewers.SelectionChangedEvent)
   */
  public void selectionChanged( final SelectionChangedEvent event )
  {
    setEnabled( m_page.isThemeSelected() );
  }
}