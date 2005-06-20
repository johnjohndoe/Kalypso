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
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.internal.Workbench;
import org.kalypso.ogc.gml.KalypsoUserStyle;
import org.kalypso.ui.editor.mapeditor.views.StyleEditorViewPart;
import org.kalypso.util.list.IListManipulator;
import org.kalypsodeegree.model.feature.event.ModellEvent;

/**
 * @author belger
 */
public class RemoveRuleAction extends AbstractOutlineAction
{
  public RemoveRuleAction( final String text, final ImageDescriptor image, final String tooltipText,
      final GisMapOutlineViewer outlineViewer, final IListManipulator listManip )
  {
    super( text, image, tooltipText, outlineViewer, listManip );
    refresh();
  }

  /**
   * @see org.eclipse.jface.action.Action#run()
   */
  public void run()
  {
    Object o = ( (IStructuredSelection)getOutlineviewer().getSelection() ).getFirstElement();
    if( o instanceof RuleTreeObject )
    {
      RuleTreeObject obj = (RuleTreeObject)o;
      KalypsoUserStyle userStyle = obj.getStyle();
      userStyle.getFeatureTypeStyles()[0].removeRule( obj.getRule() );
      userStyle.fireModellEvent( new ModellEvent( userStyle, ModellEvent.STYLE_CHANGE ) );

      IWorkbenchWindow window = Workbench.getInstance().getActiveWorkbenchWindow();
      StyleEditorViewPart part;
      try
      {
        part = (StyleEditorViewPart)window.getActivePage().showView(
            "org.kalypso.ui.editor.mapeditor.views.styleeditor" );
        if( part != null )
        {
          part.setSelectionChangedProvider( getOutlineviewer() );
          part.initStyleEditor( userStyle, obj.getTheme() );
        }
      }
      catch( PartInitException e )
      {
        e.printStackTrace();
      }
    }
  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionChangedListener#selectionChanged(org.eclipse.jface.viewers.SelectionChangedEvent)
   */
  public void selectionChanged( final SelectionChangedEvent event )
  {
    refresh();
  }

  protected void refresh()
  {
    boolean bEnable = false;

    final IStructuredSelection s = (IStructuredSelection)getOutlineviewer().getSelection();

    if( s.getFirstElement() instanceof RuleTreeObject )
      bEnable = true;
    setEnabled( bEnable );
  }
}