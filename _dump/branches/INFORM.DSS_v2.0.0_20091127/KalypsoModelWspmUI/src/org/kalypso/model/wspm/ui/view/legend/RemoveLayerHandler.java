/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 * 
 *  and
 *  
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 * 
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 * 
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 * 
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 * 
 *  Contact:
 * 
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *   
 *  ---------------------------------------------------------------------------*/
package org.kalypso.model.wspm.ui.view.legend;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PlatformUI;
import org.kalypso.chart.ui.editor.ChartEditorTreeOutlinePage;
import org.kalypso.model.wspm.ui.i18n.Messages;
import org.kalypso.model.wspm.ui.view.chart.IProfilChartLayer;

public class RemoveLayerHandler extends AbstractHandler
{
  /**
   * @see org.eclipse.core.commands.IHandler#execute(org.eclipse.core.commands.ExecutionEvent)
   */
  public Object execute( ExecutionEvent event )
  {
    final IWorkbenchPage activePage = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage();
    final IViewPart view = activePage == null ? null : activePage.findView( "org.kalypso.model.wspm.ui.view.legend.LegendView" ); //$NON-NLS-1$
    final ChartEditorTreeOutlinePage legendView = view == null ? null : (ChartEditorTreeOutlinePage) view.getAdapter( ChartEditorTreeOutlinePage.class );

    if( legendView == null )
      return null;

    final ISelection selection = legendView.getSelection();
    if( selection == null || selection.isEmpty() )
    {
      return null;
    }
    if( selection instanceof IStructuredSelection )
    {
      final IProfilChartLayer layer = (IProfilChartLayer) ((IStructuredSelection) selection).getFirstElement();
      try
      {
        layer.removeYourself();
      }
      catch( final UnsupportedOperationException e )
      {
        MessageDialog.openError( view.getViewSite().getShell(), Messages.getString("org.kalypso.model.wspm.ui.view.legend.RemoveLayerHandler.1"), e.getLocalizedMessage() ); //$NON-NLS-1$
      }
    }
    return null;

  }
}
