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

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.KalypsoUserStyle;
import org.kalypso.ogc.gml.mapmodel.IMapModellView;
import org.kalypso.ui.editor.mapeditor.views.StyleEditorViewPart;

/**
 * @author belger
 */
public class OpenStyleDialogAction extends AbstractOutlineAction
{
  public OpenStyleDialogAction( final String text, final ImageDescriptor image, final String tooltipText,
      final IMapModellView outlineViewer )
  {
    super( text, image, tooltipText, outlineViewer, null );

    refresh();
  }

  /**
   * @see org.eclipse.jface.action.Action#run()
   */
  @Override
  public void run()
  {
    StyleEditorViewPart part = null;
    final IWorkbenchWindow window = PlatformUI.getWorkbench().getActiveWorkbenchWindow();
    Object o = ( (IStructuredSelection)getOutlineviewer().getSelection() ).getFirstElement();

    try
    {
      part = (StyleEditorViewPart)window.getActivePage().showView( "org.kalypso.ui.editor.mapeditor.views.styleeditor" );

      if( part != null )
        part.setSelectionChangedProvider( getOutlineviewer() );

      // if UserStyle selected path that on to styleeditor
      if( o instanceof ThemeStyleTreeObject )
      {
        final IKalypsoTheme theme = ( (ThemeStyleTreeObject)o ).getTheme();

        if( part != null && theme instanceof IKalypsoFeatureTheme )
        {
          KalypsoUserStyle kalypsoStyle = ( (ThemeStyleTreeObject)o ).getStyle();
          part.initStyleEditor( kalypsoStyle, (IKalypsoFeatureTheme)theme );
        }
        else
          part.initStyleEditor( null, null );
      }
      else if( o instanceof IKalypsoTheme )
        part.initStyleEditor( null, null );
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }
  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionChangedListener#selectionChanged(org.eclipse.jface.viewers.SelectionChangedEvent)
   */
  @Override
  public void selectionChanged( final SelectionChangedEvent event )
  {
    refresh();
  }

  @Override
  protected void refresh()
  {
    boolean bEnable = false;

    final IStructuredSelection s = (IStructuredSelection)getOutlineviewer().getSelection();

    if( s.getFirstElement() instanceof ThemeStyleTreeObject )
      bEnable = true;
    setEnabled( bEnable );
  }
}