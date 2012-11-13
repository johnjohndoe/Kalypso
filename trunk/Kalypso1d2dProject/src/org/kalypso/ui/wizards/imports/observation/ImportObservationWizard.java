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
package org.kalypso.ui.wizards.imports.observation;

import java.io.File;
import java.util.List;
import java.util.TimeZone;

import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.ISources;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.handlers.IHandlerService;
import org.eclipse.ui.ide.IDE;
import org.kalypso.afgui.perspective.Perspective;
import org.kalypso.afgui.scenarios.ScenarioHelper;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.contribs.eclipse.swt.widgets.DateRangeInputControl;
import org.kalypso.contribs.java.io.filter.MultipleWildCardFileFilter;
import org.kalypso.core.status.StatusDialog;
import org.kalypso.kalypso1d2d.internal.i18n.Messages;
import org.kalypso.kalypso1d2d.pjt.Kalypso1d2dProjectPlugin;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DHelper;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.adapter.INativeObservationAdapter;
import org.kalypso.ogc.sensor.metadata.ITimeseriesConstants;
import org.kalypso.ogc.sensor.zml.repository.ZmlObservationRepository;
import org.kalypso.repository.IRepository;
import org.kalypso.repository.RepositoryContainerSingelton;
import org.kalypso.repository.container.IRepositoryContainer;
import org.kalypso.repository.file.FileItem;
import org.kalypso.zml.ui.imports.ImportObservationAxisMappingWizardPage;
import org.kalypso.zml.ui.imports.ImportObservationData;
import org.kalypso.zml.ui.imports.ImportObservationOperation;
import org.kalypso.zml.ui.imports.ObservationImportSelection;

/**
 * FIXME: aaarggg! Copy/Paste of ImportObservationWizard from KalypsoUI!
 * 
 * @deprecated Merge this class with the other import wizard!
 */
@Deprecated
public class ImportObservationWizard extends Wizard implements INewWizard
{
  private ImportObservationSelectionWizardPage m_importPage = null;

  private IStructuredSelection m_selection;

  private ImportObservationAxisMappingWizardPage m_axisMappingPage;

  private IFolder m_timeseriesFolder;

  private ZmlObservationRepository m_configuredRepository;

  private IViewPart m_timeseriesView;

  public ImportObservationWizard( )
  {
    super();
    setHelpAvailable( false );
    setNeedsProgressMonitor( false );
    setWindowTitle( Messages.getString( "org.kalypso.ui.wizards.imports.observation.ImportObservationWizard.0" ) ); //$NON-NLS-1$
  }

  @Override
  public void init( final IWorkbench workbench, final IStructuredSelection currentSelection )
  {
    // TODO: this is not good, because now this wizard really does not work from the 'new' menu. So be consequent and
    // dont let it be a INewWizard!

    final IHandlerService handlerService = (IHandlerService)workbench.getService( IHandlerService.class );
    final IEvaluationContext context = handlerService.getCurrentState();
    final IFolder scenarioFolder = ScenarioHelper.getScenarioFolder();

    final IWorkbenchWindow window = (IWorkbenchWindow)context.getVariable( ISources.ACTIVE_WORKBENCH_WINDOW_NAME );
    final IWorkbenchPage page = window == null ? null : window.getActivePage();
    m_timeseriesView = page == null ? null : page.findView( Perspective.TIMESERIES_REPOSITORY_VIEW_ID );

    m_timeseriesFolder = KalypsoModel1D2DHelper.getTimeeseriesFolder( scenarioFolder );
    m_selection = currentSelection;

    // TODO: why not use context.getVariable( ISource.ACTIVE_CURRENT_SELECTION_NAME ) ? Please comment at least
    final List< ? > selectedResources = IDE.computeSelectedResources( currentSelection );
    if( !selectedResources.isEmpty() )
    {
      // TODO: at least dont set a selection here! What the wizard really wants is the base-folder
      // where to import its stuff. So translate the selection to a resource here
      // and dont talk about selections any more!
      m_selection = new StructuredSelection( selectedResources );
    }

    setWindowTitle( Messages.getString( "org.kalypso.ui.wizards.imports.observation.ImportObservationWizard.Title" ) ); //$NON-NLS-1$
    setNeedsProgressMonitor( true );

    // REMARK: each time this wizard is opened, the central repository is configured.
    // This is probably NOT the right place to do this, but at the moment it works...
    /* We remember the repository in order to import into this one */
    m_configuredRepository = configureRepository();
  }

  private ZmlObservationRepository configureRepository( )
  {
    /* repository container */
    final IRepositoryContainer repositoryContainer = RepositoryContainerSingelton.getInstance().getContainer();

    /* add new project repository */
    final String location = m_timeseriesFolder.getLocation().toOSString();
    final String configuration = location + "#*.zml"; //$NON-NLS-1$

    /* Remove all repositories which do not fit to this scenario */
    final IRepository[] repositories = repositoryContainer.getRepositories();
    for( final IRepository rep : repositories )
    {
      if( rep instanceof ZmlObservationRepository && configuration.equals( rep.getConfiguration() ) )
        continue;

      repositoryContainer.removeRepository( rep );
    }

    /* If we still have a good repository, ok */
    if( repositoryContainer.getRepositories().length > 0 )
      return (ZmlObservationRepository)repositoryContainer.getRepositories()[0];

    /* Respository was not configured before, so create a new one */

    /* First, make sure that the folder exists */
    new File( location ).mkdirs();

    final MultipleWildCardFileFilter filter = new MultipleWildCardFileFilter( new String[] { "*.zml" }, false, true, false ); //$NON-NLS-1$

    final ZmlObservationRepository repository = new ZmlObservationRepository( this.getClass().getName(), configuration, location, "Time Series", false, false, filter ); //$NON-NLS-1$

    /* set datarange to 0 -> show all observation this item inherits */
    repository.setProperty( DateRangeInputControl.USE_RANGE, "0" ); //$NON-NLS-1$
    repository.setProperty( DateRangeInputControl.NUMBER_OF_DAYS, "0" ); //$NON-NLS-1$

    repositoryContainer.addRepository( repository );

    return repository;
  }

  @Override
  public void addPages( )
  {
    super.addPages();

    m_importPage = new ImportObservationSelectionWizardPage( "importPage", m_timeseriesFolder ); //$NON-NLS-1$
    m_axisMappingPage = new ImportObservationAxisMappingWizardPage( "sourcePage" ); //$NON-NLS-1$

    addPage( m_importPage );
    addPage( m_axisMappingPage );

    m_importPage.setSelection( m_selection );
    m_importPage.addSelectionChangedListener( m_axisMappingPage );
  }

  @Override
  public boolean performFinish( )
  {
    final String[] allowedTypes = new String[] { ITimeseriesConstants.TYPE_WATERLEVEL, ITimeseriesConstants.TYPE_RUNOFF, ITimeseriesConstants.TYPE_RAINFALL, ITimeseriesConstants.TYPE_TEMPERATURE,
        ITimeseriesConstants.TYPE_VOLUME, ITimeseriesConstants.TYPE_EVAPORATION };

    final ImportObservationData data = new ImportObservationData( allowedTypes );

    /* Translate old selection to data */
    final ObservationImportSelection selection = (ObservationImportSelection)m_importPage.getSelection();
    data.getSourceFileData().setFile( selection.getFileSource() );
    data.setTargetFile( selection.getFileTarget() );

    final TimeZone timezone = selection.getSourceTimezone();
    data.setTimezone( timezone.getID() );

    final INativeObservationAdapter nativaAdapter = selection.getNativeAdapter();
    data.setAdapter( nativaAdapter );

    /* Run operation */
    final Shell shell = getShell();

    final IAxis[] axesSrc = m_axisMappingPage.getAxisMappingSrc();
    final IAxis[] axesNew = m_axisMappingPage.getAxisMappingTarget();

    final ImportObservationOperation importOperation = new ImportObservationOperation( data, shell, axesSrc, axesNew, selection );
    final IStatus status = RunnableContextHelper.execute( getContainer(), true, false, importOperation );
    if( !status.isOK() )
      StatusDialog.open( getShell(), status, getWindowTitle() );

    try
    {
      m_timeseriesFolder.refreshLocal( IResource.DEPTH_INFINITE, null );

      /* Reload repositories in order show new timeserie */
      m_configuredRepository.reload();

      /* Select this item inside the timeseries observation */
      // TODO: does not work at the moment
      // Probably a problem of the tree viewer in combinisation with missing parent elements
      // or something similar
      if( m_timeseriesView != null )
      {
        final IFile targetFile = data.getTargetFile();
        final File targetJavaFile = targetFile.getLocation().toFile();
        final FileItem item = m_configuredRepository.createItem( targetJavaFile );
        m_timeseriesView.getViewSite().getSelectionProvider().setSelection( new StructuredSelection( item ) );
      }
    }
    catch( final CoreException e )
    {
      final String title = getWindowTitle();
      final String message = Messages.getString("ImportObservationWizard.0"); //$NON-NLS-1$

      final IStatus error = new Status( IStatus.ERROR, Kalypso1d2dProjectPlugin.PLUGIN_ID, message, e );

      StatusDialog.open( shell, error, title );
      return false;
    }

    return !status.matches( IStatus.ERROR );
  }
}