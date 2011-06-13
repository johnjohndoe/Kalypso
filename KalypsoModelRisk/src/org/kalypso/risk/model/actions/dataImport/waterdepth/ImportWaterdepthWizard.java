/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
package org.kalypso.risk.model.actions.dataImport.waterdepth;

import java.util.List;

import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.handlers.IHandlerService;
import org.kalypso.afgui.scenarios.SzenarioDataProvider;
import org.kalypso.commons.command.EmptyCommand;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.ogc.gml.GisTemplateMapModell;
import org.kalypso.risk.i18n.Messages;
import org.kalypso.risk.model.operation.RiskImportWaterdepthRunnable;
import org.kalypso.risk.model.schema.binding.IRasterDataModel;
import org.kalypso.risk.model.utils.RiskModelHelper;
import org.kalypso.risk.plugin.KalypsoRiskPlugin;
import org.kalypso.ui.views.map.MapView;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;

import de.renew.workflow.contexts.ICaseHandlingSourceProvider;

/**
 * @author Dejan Antanaskovic, <a href="mailto:dejan.antanaskovic@tuhh.de">dejan.antanaskovic@tuhh.de</a>
 */
public class ImportWaterdepthWizard extends Wizard implements INewWizard
{
  protected ImportWaterdepthPage m_page;

  public ImportWaterdepthWizard( )
  {
    super();
  }

  @Override
  public void init( final IWorkbench workbench, final IStructuredSelection selection )
  {
    setNeedsProgressMonitor( true );
    setWindowTitle( Messages.getString( "org.kalypso.risk.model.actions.dataImport.waterdepth.ImportWaterdepthWizard.0" ) ); //$NON-NLS-1$
  }

  @Override
  public void addPages( )
  {
    m_page = new ImportWaterdepthPage();
    addPage( m_page );
  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#canFinish()
   */
  @Override
  public boolean canFinish( )
  {
    return m_page.isPageComplete();
  }

  /**
   * This method is called by the wizard framework when the user presses the Finish button.
   */
  @Override
  public boolean performFinish( )
  {
    final IWorkbench workbench = PlatformUI.getWorkbench();
    final MapView mapView = (MapView) workbench.getActiveWorkbenchWindow().getActivePage().findView( MapView.ID );
    if( mapView == null )
    {
      StatusUtilities.createWarningStatus( Messages.getString( "org.kalypso.risk.model.actions.dataImport.waterdepth.ImportWaterdepthWizard.8" ) ); //$NON-NLS-1$
      return false;
    }
    final GisTemplateMapModell mapModell = (GisTemplateMapModell) mapView.getMapPanel().getMapModell();
    final IHandlerService handlerService = (IHandlerService) workbench.getService( IHandlerService.class );
    final IEvaluationContext context = handlerService.getCurrentState();
    final SzenarioDataProvider scenarioDataProvider = (SzenarioDataProvider) context.getVariable( ICaseHandlingSourceProvider.ACTIVE_CASE_DATA_PROVIDER_NAME );
    final IFolder scenarioFolder = (IFolder) context.getVariable( ICaseHandlingSourceProvider.ACTIVE_CASE_FOLDER_NAME );
    final List<AsciiRasterInfo> rasterInfos = m_page.getRasterInfos();

    try
    {
      final GMLWorkspace workspace = scenarioDataProvider.getCommandableWorkSpace( IRasterDataModel.class.getName() );
      final IRasterDataModel rasterDataModel = scenarioDataProvider.getModel( IRasterDataModel.class.getName(), IRasterDataModel.class );

      final ICoreRunnableWithProgress importWaterdepthRunnable = new RiskImportWaterdepthRunnable( rasterDataModel, rasterInfos, scenarioFolder );

      final IStatus execute = RunnableContextHelper.execute( getContainer(), true, true, importWaterdepthRunnable );
      ErrorDialog.openError( getShell(), Messages.getString( "org.kalypso.risk.model.actions.dataImport.waterdepth.ImportWaterdepthWizard.9" ), Messages.getString( "org.kalypso.risk.model.actions.dataImport.waterdepth.ImportWaterdepthWizard.10" ), execute ); //$NON-NLS-1$ //$NON-NLS-2$

      if( !execute.isOK() )
      {
        KalypsoRiskPlugin.getDefault().getLog().log( execute );
        // TODO: clean partly imported stuff
      }

      scenarioDataProvider.postCommand( IRasterDataModel.class.getName(), new EmptyCommand( "Get dirty!", false ) ); //$NON-NLS-1$

      // Undoing this operation is not possible because old raster files are deleted...
      scenarioDataProvider.saveModel( new NullProgressMonitor() );

      // remove themes that are showing invalid coverages
      RiskModelHelper.updateWaterdepthLayers( rasterDataModel, rasterInfos, mapModell );

      // fireModellEvent to redraw a map...
      // final IFeatureBindingCollection<IAnnualCoverageCollection> waterdepthCoverageCollection =
      // rasterDataModel.getWaterlevelCoverageCollection();
      final Feature f1 = (Feature) rasterDataModel.getFeature().getProperty( IRasterDataModel.PROPERTY_WATERLEVEL_COVERAGE_COLLECTION );
      workspace.fireModellEvent( new FeatureStructureChangeModellEvent( workspace, f1, new Feature[] { f1 }, FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD ) );

    }
    catch( final Exception e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }

    return true;
  }
}
