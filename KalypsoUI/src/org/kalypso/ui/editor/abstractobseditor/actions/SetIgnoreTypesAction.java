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

import java.util.LinkedHashSet;
import java.util.Set;

import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.window.Window;
import org.eclipse.ui.dialogs.ListSelectionDialog;
import org.kalypso.contribs.eclipse.jface.action.FullAction;
import org.kalypso.ogc.sensor.template.ObsView;
import org.kalypso.ogc.sensor.template.ObsViewUtils;
import org.kalypso.ui.ImageProvider;
import org.kalypso.ui.editor.abstractobseditor.ObservationEditorOutlinePage;

/**
 * Allows the user to set the axis-types that should be ignored when displaying observation items
 * 
 * @author schlienger
 */
public class SetIgnoreTypesAction extends FullAction
{
  private static final String TITLE = "Unsichtbare Achsen setzen";

  private final static String MSG = "Wählen Sie die Achsentypen die nicht dargestellt werden";

  private final ObservationEditorOutlinePage m_page;

  public SetIgnoreTypesAction( final ObservationEditorOutlinePage page )
  {
    super( TITLE, ImageProvider.IMAGE_UTIL_FILTER, "Erlaubt die Deaktivierung von ausgewählten Achsentypen (z.B. Wasserstand)" );

    m_page = page;
  }

  @Override
  public void run( )
  {
    final ObsView obsView = m_page.getView();
    final Set<String> types = ObsViewUtils.retrieveAxisTypes( obsView.getItems(), true );

    final ListSelectionDialog dlg = new ListSelectionDialog( m_page.getSite().getShell(), types.toArray(), new ArrayContentProvider(), new LabelProvider(), MSG );
    dlg.setTitle( TITLE );
    dlg.setInitialSelections( obsView.getHiddenTypes().toArray() );

    if( dlg.open() == Window.OK )
    {
      final Object[] res = dlg.getResult();
      if( res == null )
        obsView.hideTypes( null );
      else
      {
        final Set<String> hideSet = new LinkedHashSet<String>();
        for( final Object string : res )
          hideSet.add( string.toString() );
        obsView.hideTypes( hideSet );
      }
    }
  }
}