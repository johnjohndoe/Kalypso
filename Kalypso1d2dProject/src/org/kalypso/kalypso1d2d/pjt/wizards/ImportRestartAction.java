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
package org.kalypso.kalypso1d2d.pjt.wizards;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubMonitor;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.window.Window;
import org.eclipse.jface.wizard.IWizard;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.commons.command.EmptyCommand;
import org.kalypso.commons.eclipse.core.runtime.PluginImageProvider;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.jface.dialog.DialogSettingsUtils;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.contribs.eclipse.jface.viewers.ViewerUtilities;
import org.kalypso.contribs.eclipse.jface.wizard.WizardDialog2;
import org.kalypso.kalypso1d2d.internal.i18n.Messages;
import org.kalypso.kalypso1d2d.pjt.Kalypso1d2dProjectPlugin;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DUIImages;
import org.kalypso.kalypsomodel1d2d.imports.Restart1DImporter;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IScenarioResultMeta;
import org.kalypso.model.wspm.schema.IWspmDictionaryConstants;
import org.kalypso.ui.wizards.imports.wspmrestart.ImportWspmRestartWizard;

import de.renew.workflow.connector.cases.IScenarioDataProvider;

/**
 * Imports some external restart data into this model.
 * 
 * @author Gernot Belger
 */
public class ImportRestartAction extends Action
{
  private final RestartSelectWizardPage1 m_page;

  private final IFolder m_scenarioFolder;

  private final IScenarioDataProvider m_modelProvider;

  private final IScenarioResultMeta m_resultModel;

  public ImportRestartAction( final RestartSelectWizardPage1 page, final IFolder scenarioFolder, final IScenarioDataProvider modelProvider, final IScenarioResultMeta resultModel )
  {
    m_page = page;
    m_scenarioFolder = scenarioFolder;
    m_modelProvider = modelProvider;
    m_resultModel = resultModel;

    setText( Messages.getString( "org.kalypso.kalypso1d2d.pjt.wizards.RestartSelectWizardPage1.0" ) ); //$NON-NLS-1$

    final PluginImageProvider imageProvider = KalypsoModel1D2DPlugin.getImageProvider();
    setImageDescriptor( imageProvider.getImageDescriptor( KalypsoModel1D2DUIImages.IMGKEY.IMPORT_EXTERNAL_RESULT ) );
  }

  @Override
  public void runWithEvent( final Event event )
  {
    final Shell shell = event.widget.getDisplay().getActiveShell();
    final IWizard outerWizard = m_page.getWizard();

    // Open Import Wizard
    final ImportWspmRestartWizard importWizard = new ImportWspmRestartWizard();
    importWizard.setWindowTitle( Messages.getString( "org.kalypso.kalypso1d2d.pjt.wizards.RestartSelectWizardPage1.1" ) ); //$NON-NLS-1$

    DialogSettingsUtils.getDialogSettings( Kalypso1d2dProjectPlugin.getDefault(), Messages.getString( "org.kalypso.kalypso1d2d.pjt.wizards.RestartSelectWizardPage1.2" ) ); //$NON-NLS-1$
    importWizard.setDialogSettings( outerWizard.getDialogSettings() );
    importWizard.setNeedsProgressMonitor( true );

    // FIXME: a wizard from a wizard? add a second page instead...!

    final WizardDialog2 wizardDialog = new WizardDialog2( shell, importWizard );
    if( wizardDialog.open() != Window.OK )
      return;

    final TreeViewer treeViewer = m_page.getTreeViewer();
    final IScenarioDataProvider modelProvider = m_modelProvider;

    final Restart1DImporter restart1DImporter = new Restart1DImporter( m_resultModel, m_scenarioFolder );

    final ICoreRunnableWithProgress operation = new ICoreRunnableWithProgress()
    {
      @Override
      public IStatus execute( final IProgressMonitor monitor )
      {
        try
        {
          final SubMonitor progress = SubMonitor.convert( monitor, Messages.getString( "org.kalypso.kalypso1d2d.pjt.wizards.RestartSelectWizardPage1.3" ), 100 ); //$NON-NLS-1$

          final IFile lengthSectionFile = importWizard.getLengthSection();
          restart1DImporter.doImport( lengthSectionFile, IWspmDictionaryConstants.LS_COMPONENT_STATION, IWspmDictionaryConstants.LS_COMPONENT_WATERLEVEL, IWspmDictionaryConstants.LS_COMPONENT_VELOCITY, IWspmDictionaryConstants.LS_COMPONENT_TYPE, progress.newChild( 80 ) );

          final IScenarioDataProvider szenarioDataProvider = modelProvider;
          szenarioDataProvider.postCommand( IScenarioResultMeta.class.getName(), new EmptyCommand( "empty", false ) ); //$NON-NLS-1$
          modelProvider.saveModel( IScenarioResultMeta.class.getName(), progress.newChild( 20 ) );

          ViewerUtilities.refresh( treeViewer, true );

          return Status.OK_STATUS;
        }
        catch( final Exception e )
        {
          return StatusUtilities.statusFromThrowable( e );
        }
      }
    };

    final IStatus result = RunnableContextHelper.execute( outerWizard.getContainer(), true, false, operation );
    ErrorDialog.openError( shell, m_page.getTitle(), Messages.getString( "org.kalypso.kalypso1d2d.pjt.wizards.RestartSelectWizardPage1.5" ), result ); //$NON-NLS-1$
  }
}