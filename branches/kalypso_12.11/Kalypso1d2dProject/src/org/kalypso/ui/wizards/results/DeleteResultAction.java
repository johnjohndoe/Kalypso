/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
package org.kalypso.ui.wizards.results;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.commons.eclipse.core.runtime.PluginImageProvider;
import org.kalypso.contribs.eclipse.jface.viewers.ViewerUtilities;
import org.kalypso.kalypso1d2d.internal.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DUIImages;
import org.kalypso.kalypsomodel1d2d.conv.results.ResultMeta1d2dHelper;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IStepResultMeta;
import org.kalypso.kalypsosimulationmodel.core.resultmeta.IResultMeta;
import org.kalypso.ogc.gml.IKalypsoLayerModell;

/**
 * @author Gernot Belger
 */
public class DeleteResultAction extends Action
{
  private final ICommandTarget m_commandTarget;

  private final IKalypsoLayerModell m_modell;

  private final SelectResultWizardPage m_page;

  public DeleteResultAction( final SelectResultWizardPage page, final ICommandTarget commandTarget, final IKalypsoLayerModell modell )
  {
    m_page = page;
    m_commandTarget = commandTarget;
    m_modell = modell;

    setToolTipText( Messages.getString( "org.kalypso.ui.wizards.results.ResultManager1d2dWizardPage.0" ) ); //$NON-NLS-1$

    final PluginImageProvider imageProvider = KalypsoModel1D2DPlugin.getImageProvider();
    setImageDescriptor( imageProvider.getImageDescriptor( KalypsoModel1D2DUIImages.IMGKEY.DELETE ) );
  }

  @Override
  public void runWithEvent( final Event event )
  {
    final Shell shell = event.widget.getDisplay().getActiveShell();

    // FIXME: show real list of selected element -> if none selected, show user what to do
    if( !MessageDialog.openConfirm( shell, m_page.getWizard().getWindowTitle(), Messages.getString( "DeleteResultAction.0" ) ) ) //$NON-NLS-1$
      return;

    final TreeViewer treeViewer = m_page.getTreeViewer();
    final IResultMeta[] selectedResults = m_page.getSelectedResults();
    for( final IResultMeta resultMeta : selectedResults )
    {
      if( resultMeta instanceof IStepResultMeta )
      {
        /* handle result meta entries */
        final IStepResultMeta stepResult = (IStepResultMeta)resultMeta;
        ResultMeta1d2dHelper.removeResult( stepResult );

        /* handle map themes */
        if( m_modell != null && m_commandTarget != null )
          ResultMeta1d2dHelper.deleteResultThemeFromMap( stepResult, m_modell, m_commandTarget );

        /* handle tree */
        ViewerUtilities.refresh( treeViewer, true );
      }
    }
  }
}