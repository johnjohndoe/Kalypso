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
import java.net.URL;
import java.util.List;
import java.util.TimeZone;

import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.ISources;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.handlers.IHandlerService;
import org.eclipse.ui.ide.IDE;
import org.kalypso.afgui.perspective.Perspective;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.swt.widgets.DateRangeInputControl;
import org.kalypso.contribs.java.io.filter.MultipleWildCardFileFilter;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DHelper;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITupleModel;
import org.kalypso.ogc.sensor.adapter.INativeObservationAdapter;
import org.kalypso.ogc.sensor.impl.SimpleObservation;
import org.kalypso.ogc.sensor.impl.SimpleTupleModel;
import org.kalypso.ogc.sensor.metadata.MetadataList;
import org.kalypso.ogc.sensor.status.KalypsoStati;
import org.kalypso.ogc.sensor.status.KalypsoStatusUtils;
import org.kalypso.ogc.sensor.timeseries.wq.WQTuppleModel;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.ogc.sensor.zml.repository.ZmlObservationRepository;
import org.kalypso.repository.IRepository;
import org.kalypso.repository.RepositoryContainerSingelton;
import org.kalypso.repository.container.IRepositoryContainer;
import org.kalypso.repository.file.FileItem;
import org.kalypso.ui.wizard.sensor.ImportObservationAxisMappingWizardPage;
import org.kalypso.ui.wizard.sensor.ObservationImportSelection;
import org.kalypso.ui.wizards.i18n.Messages;

import de.renew.workflow.contexts.ICaseHandlingSourceProvider;

public class ImportObservationWizard extends Wizard implements INewWizard
{
  private ImportObservationSelectionWizardPage m_page1 = null;

  private IStructuredSelection m_selection;

  private ImportObservationAxisMappingWizardPage m_page2;

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

  /**
   * @see org.eclipse.ui.IWorkbenchWizard#init(org.eclipse.ui.IWorkbench,
   *      org.eclipse.jface.viewers.IStructuredSelection)
   */
  @Override
  public void init( final IWorkbench workbench, final IStructuredSelection currentSelection )
  {
    // TODO: this is not good, because now this wizard really does not work from the 'new' menu. So be consequent and
    // dont let it be a INewWizard!

    final IHandlerService handlerService = (IHandlerService) workbench.getService( IHandlerService.class );
    final IEvaluationContext context = handlerService.getCurrentState();
    final IFolder scenarioFolder = ((IFolder) context.getVariable( ICaseHandlingSourceProvider.ACTIVE_CASE_FOLDER_NAME ));

    final IWorkbenchWindow window = (IWorkbenchWindow) context.getVariable( ISources.ACTIVE_WORKBENCH_WINDOW_NAME );
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
      return (ZmlObservationRepository) repositoryContainer.getRepositories()[0];

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

  /**
   * @see org.eclipse.jface.wizard.IWizard#addPages()
   */
  @Override
  public void addPages( )
  {
    super.addPages();
    m_page1 = new ImportObservationSelectionWizardPage( "Dateien waehlen", m_timeseriesFolder ); //$NON-NLS-1$
    m_page2 = new ImportObservationAxisMappingWizardPage( "Analyse der Import-Datei" ); //$NON-NLS-1$

    addPage( m_page1 );
    addPage( m_page2 );

    m_page1.setSelection( m_selection );
    m_page1.addSelectionChangedListener( m_page2 );
  }

  /**
   * @see org.eclipse.jface.wizard.IWizard#performCancel()
   */
  @Override
  public boolean performCancel( )
  {
    return true;
  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#performFinish()
   */
  @Override
  public boolean performFinish( )
  {
    try
    {
      final ObservationImportSelection selection = (ObservationImportSelection) m_page1.getSelection();
      final File fileSource = selection.getFileSource();
      final IFile fileTarget = selection.getFileTarget();

      final TimeZone timezone = selection.getSourceTimezone();

      final INativeObservationAdapter nativaAdapter = selection.getNativeAdapter();
      IObservation srcObservation = nativaAdapter.createObservationFromSource( fileSource, timezone, false );
      if( srcObservation == null )
      {
        final MessageBox messageBox = new MessageBox( getShell(), SWT.ICON_QUESTION | SWT.YES | SWT.NO );
        messageBox.setMessage( Messages.getString( "org.kalypso.ui.wizards.imports.observation.ImportObservationWizard.3" ) ); //$NON-NLS-1$
        messageBox.setText( Messages.getString( "org.kalypso.ui.wizards.imports.observation.ImportObservationWizard.2" ) ); //$NON-NLS-1$
        if( messageBox.open() == SWT.NO )
          return true;
        else
          srcObservation = nativaAdapter.createObservationFromSource( fileSource, timezone, true );
      }

      final IAxis[] axesSrc = m_page2.getAxisMappingSrc();
      final IAxis[] axesNew = m_page2.getAxisMappingTarget();

      final ITupleModel tuppelModelSrc = srcObservation.getValues( null );
      final int countSrc = tuppelModelSrc.size();

      final IObservation targetObservation;
      final ITupleModel tuppelModelTarget;
      final int countTarget;
      if( fileTarget.exists() && (selection.isAppend() || selection.isRetainMetadata()) )
      {
        final URL targetLocation = ResourceUtilities.createURL( fileTarget );
        targetObservation = ZmlFactory.parseXML( targetLocation );
        tuppelModelTarget = targetObservation.getValues( null );
        if( selection.isAppend() )
          countTarget = tuppelModelTarget.size();
        else
          countTarget = 0;
      }
      else
      {
        targetObservation = null;
        tuppelModelTarget = null;
        countTarget = 0;
      }
      // create new values
      final ITupleModel newTuppelModel;
      if( tuppelModelTarget != null )
      {
        // w/q specials...
        if( tuppelModelTarget instanceof WQTuppleModel )
        {
          final WQTuppleModel wq = (WQTuppleModel) (tuppelModelTarget);
          final Object[][] newValues = new Object[countSrc + countTarget][axesNew.length - 1];
          final ITupleModel model = new SimpleTupleModel( axesNew, newValues );

          newTuppelModel = new WQTuppleModel( model, wq.getMetadata(), axesNew, wq.getSourceAxes(), wq.getTargetAxes(), wq.getConverter() );
        }
        else
        {
          final Object[][] newValues = new Object[countSrc + countTarget][axesNew.length];
          newTuppelModel = new SimpleTupleModel( axesNew, newValues );
        }
      }
      else
      {
        final Object[][] newValues = new Object[countSrc + countTarget][axesNew.length];
        newTuppelModel = new SimpleTupleModel( axesNew, newValues );
      }
      // fill from source
      for( int i = 0; i < countSrc; i++ )
      {
        for( int a = 0; a < axesNew.length; a++ )
        {
          final Object newValue;
          if( axesSrc[a] == null )
          {
            if( KalypsoStatusUtils.isStatusAxis( axesNew[a] ) )
              newValue = new Integer( KalypsoStati.BIT_USER_MODIFIED );
            else
              newValue = null;
          }
          else
            newValue = tuppelModelSrc.get( i, axesSrc[a] );
          if( newValue != null )
            newTuppelModel.set( i, axesNew[a], newValue );
        }
      }
      // append from existing target
      if( tuppelModelTarget != null )
      {
        for( int i = 0; i < countTarget; i++ )
          for( final IAxis element : axesNew )
            newTuppelModel.set( countSrc + i, element, tuppelModelTarget.get( i, element ) );
      }
      final String href = ""; //$NON-NLS-1$
      final String name = srcObservation.getName();
      final MetadataList metadata = new MetadataList();
      if( targetObservation != null && selection.isRetainMetadata() )
        metadata.putAll( targetObservation.getMetadataList() );
      metadata.putAll( srcObservation.getMetadataList() );

      final IObservation newObservation = new SimpleObservation( href, name, metadata, newTuppelModel );

      ZmlFactory.writeToFile( newObservation, fileTarget );

      m_timeseriesFolder.refreshLocal( IResource.DEPTH_INFINITE, null );

      /* Reload repositories in order show new timeserie */
      m_configuredRepository.reload();

      /* Select this item inside the timeseries observation */
      // TODO: does not work at the moment
      // Probably a problem of the tree viewer in combinisation with missing parent elements
      // or something similar
      if( m_timeseriesView != null )
      {
        final File targetJavaFile = fileTarget.getLocation().toFile();
        final FileItem item = m_configuredRepository.createItem( targetJavaFile );
        m_timeseriesView.getViewSite().getSelectionProvider().setSelection( new StructuredSelection( item ) );
      }
    }
    catch( final Exception e )
    {
      final IStatus status = StatusUtilities.statusFromThrowable( e );
      final String title = Messages.getString( "org.kalypso.ui.wizards.imports.observation.ImportObservationWizard.4" ); //$NON-NLS-1$
      final String message = Messages.getString( "org.kalypso.ui.wizards.imports.observation.ImportObservationWizard.5" ); //$NON-NLS-1$
      ErrorDialog.openError( getShell(), title, message, status );
      return false;
    }
    return true;
  }
}