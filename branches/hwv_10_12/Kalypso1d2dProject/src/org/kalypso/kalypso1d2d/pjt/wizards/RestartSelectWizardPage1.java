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
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.ViewerFilter;
import org.eclipse.jface.window.Window;
import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.kalypso.afgui.model.IModel;
import org.kalypso.afgui.scenarios.SzenarioDataProvider;
import org.kalypso.commons.command.EmptyCommand;
import org.kalypso.contribs.eclipse.core.runtime.PluginUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.contribs.eclipse.jface.viewers.ViewerUtilities;
import org.kalypso.contribs.eclipse.jface.wizard.WizardDialog2;
import org.kalypso.kalypso1d2d.pjt.Kalypso1d2dProjectPlugin;
import org.kalypso.kalypso1d2d.pjt.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.imports.Restart1DImporter;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IScenarioResultMeta;
import org.kalypso.model.wspm.schema.IWspmDictionaryConstants;
import org.kalypso.ui.wizards.imports.wspmrestart.ImportWspmRestartWizard;
import org.kalypso.ui.wizards.results.IThemeConstructionFactory;
import org.kalypso.ui.wizards.results.Result1d2dMetaComparator;
import org.kalypso.ui.wizards.results.SelectResultWizardPage;

import de.renew.workflow.connector.cases.ICaseDataProvider;

/**
 * @author Dejan Antanaskovic
 *
 */
public class RestartSelectWizardPage1 extends SelectResultWizardPage
{
  private final IFolder m_scenarioFolder;

  private final ICaseDataProvider<IModel> m_modelProvider;

  public RestartSelectWizardPage1( final String pageName, final String title, final ImageDescriptor titleImage, final ViewerFilter filter, final Result1d2dMetaComparator comparator, final IThemeConstructionFactory factory, final IFolder scenarioFolder, final ICaseDataProvider<IModel> modelProvider )
  {
    super( pageName, title, titleImage, filter, comparator, factory, null );

    m_scenarioFolder = scenarioFolder;
    m_modelProvider = modelProvider;
  }

  /**
   * @see org.kalypso.ui.wizards.results.SelectResultWizardPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  @Override
  public void createControl( final Composite parent )
  {
    // HACK: add an extra button 'Import'

    final Composite panel = new Composite( parent, SWT.BORDER );
    panel.setLayout( new GridLayout() );

    super.createControl( panel );

    getControl().setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );

    final Button importButton = new Button( panel, SWT.PUSH );
    importButton.setText( Messages.getString("org.kalypso.kalypso1d2d.pjt.wizards.RestartSelectWizardPage1.0") ); //$NON-NLS-1$
    importButton.addSelectionListener( new SelectionAdapter()
    {
      /**
       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
       */
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        handleImportPressed();
      }
    } );

    setControl( panel );
  }

  /**
   * @see org.eclipse.jface.wizard.WizardPage#getNextPage()
   */
  @Override
  public IWizardPage getNextPage( )
  {
    final IWizardPage nextPage = super.getNextPage();
    if( nextPage instanceof RestartSelectWizardPage2 )
      ((RestartSelectWizardPage2) nextPage).initializeResults( getSelectedResults() );
    return nextPage;
  }

  /**
   * @see org.eclipse.jface.wizard.WizardPage#canFlipToNextPage()
   */
  @Override
  public boolean canFlipToNextPage( )
  {
    return getSelectedResults().length > 0;
  }

  /** Import button was pressed: imports some external restart data into this model. */
  protected void handleImportPressed( )
  {
    // Open Import Wizard
    final ImportWspmRestartWizard importWizard = new ImportWspmRestartWizard();
    importWizard.setWindowTitle( Messages.getString("org.kalypso.kalypso1d2d.pjt.wizards.RestartSelectWizardPage1.1") ); //$NON-NLS-1$

    PluginUtilities.getDialogSettings( Kalypso1d2dProjectPlugin.getDefault(), Messages.getString("org.kalypso.kalypso1d2d.pjt.wizards.RestartSelectWizardPage1.2") ); //$NON-NLS-1$
    importWizard.setDialogSettings( getDialogSettings() );
    importWizard.setNeedsProgressMonitor( true );
    final WizardDialog2 wizardDialog = new WizardDialog2( getContainer().getShell(), importWizard );
    if( wizardDialog.open() != Window.OK )
      return;

    final TreeViewer treeViewer = getTreeViewer();
    final IFolder scenarioFolder = m_scenarioFolder;
    final ICaseDataProvider<IModel> modelProvider = m_modelProvider;
    final ICoreRunnableWithProgress operation = new ICoreRunnableWithProgress()
    {
      @Override
      public IStatus execute( final IProgressMonitor monitor )
      {
        try
        {
          final SubMonitor progress = SubMonitor.convert( monitor, Messages.getString("org.kalypso.kalypso1d2d.pjt.wizards.RestartSelectWizardPage1.3"), 100 ); //$NON-NLS-1$

          final IFile lengthSectionFile = importWizard.getLengthSection();
          final Restart1DImporter restart1DImporter = new Restart1DImporter( (IScenarioResultMeta) RestartSelectWizardPage1.this.getResultRoot(), scenarioFolder );
          restart1DImporter.doImport( lengthSectionFile, IWspmDictionaryConstants.LS_COMPONENT_STATION, IWspmDictionaryConstants.LS_COMPONENT_WATERLEVEL, IWspmDictionaryConstants.LS_COMPONENT_VELOCITY, IWspmDictionaryConstants.LS_COMPONENT_TYPE, progress.newChild( 80 ) );

          final SzenarioDataProvider szenarioDataProvider = (SzenarioDataProvider) modelProvider;
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

    final IStatus result = RunnableContextHelper.execute( getContainer(), true, false, operation );
    ErrorDialog.openError( getShell(), getTitle(), Messages.getString("org.kalypso.kalypso1d2d.pjt.wizards.RestartSelectWizardPage1.5"), result ); //$NON-NLS-1$
  }
}
