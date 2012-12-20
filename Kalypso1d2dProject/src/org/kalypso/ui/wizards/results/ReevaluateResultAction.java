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
package org.kalypso.ui.wizards.results;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.viewers.CheckboxTreeViewer;
import org.eclipse.jface.wizard.IWizard;
import org.eclipse.jface.wizard.IWizardContainer;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.afgui.KalypsoAFGUIFrameworkPlugin;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.commons.eclipse.core.runtime.PluginImageProvider;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.contribs.eclipse.jface.viewers.ViewerUtilities;
import org.kalypso.core.status.StatusDialog;
import org.kalypso.kalypso1d2d.internal.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DUIImages;
import org.kalypso.kalypsomodel1d2d.ui.geolog.IGeoLog;
import org.kalypso.kalypsosimulationmodel.core.resultmeta.IResultMeta;
import org.kalypso.ogc.gml.IKalypsoLayerModell;

import de.renew.workflow.connector.cases.IScenarioDataProvider;

/**
 * @author Ilya Gershovich
 */
public class ReevaluateResultAction extends Action
{
  private final SelectResultWizardPage m_page;

  private final ICommandTarget m_commandTarget;

  private final IKalypsoLayerModell m_modell;

  private final IScenarioDataProvider m_modelProvider;

  private final IGeoLog m_geoLog;

  public ReevaluateResultAction( final SelectResultWizardPage page, final ICommandTarget commandTarget, final IKalypsoLayerModell modell, final IScenarioDataProvider modelProvider, final IGeoLog geoLog )
  {
    m_page = page;
    m_commandTarget = commandTarget;
    m_modell = modell;
    m_modelProvider = modelProvider;
    m_geoLog = geoLog;

    setToolTipText( Messages.getString( "org.kalypso.ui.wizards.results.ResultManager1d2dWizardPage.7" ) ); //$NON-NLS-1$

    final PluginImageProvider imageProvider = KalypsoModel1D2DPlugin.getImageProvider();
    setImageDescriptor( imageProvider.getImageDescriptor( KalypsoModel1D2DUIImages.IMGKEY.GO ) );
  }

  @Override
  public void runWithEvent( final Event event )
  {
    final Shell shell = event.widget.getDisplay().getActiveShell();

    reevaluateResults( shell );
    /* handle tree */
    final CheckboxTreeViewer treeViewer = m_page.getTreeViewer();
    ViewerUtilities.refresh( treeViewer, true );
  }

  protected void reevaluateResults( final Shell shell )
  {
    try
    {
      setEnabled( false );

      final IResultMeta[] selectedResults = m_page.getSelectedResults();

      final IContainer scenarioFolder = KalypsoAFGUIFrameworkPlugin.getActiveWorkContext().getCurrentCase().getFolder();

      final ICoreRunnableWithProgress calculationOperation = new ReevaluateResultOperation( selectedResults, scenarioFolder, m_commandTarget, m_modell, m_modelProvider, m_geoLog );

      final IWizard wizard = m_page.getWizard();
      final IWizardContainer container = wizard.getContainer();
      final IStatus status = RunnableContextHelper.execute( container, true, true, calculationOperation );
      if( !status.isOK() )
        StatusDialog.open( shell, status, wizard.getWindowTitle() );
    }
    finally
    {
      setEnabled( true );
    }
  }
}
