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

import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;

import org.apache.commons.io.IOUtils;
import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.ISources;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchWizard;
import org.eclipse.ui.handlers.IHandlerService;
import org.kalypso.afgui.KalypsoAFGUIFrameworkPlugin;
import org.kalypso.afgui.scenarios.ScenarioHelper;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.kalypso1d2d.internal.i18n.Messages;
import org.kalypso.kalypso1d2d.pjt.Kalypso1d2dProjectPlugin;
import org.kalypso.kalypso1d2d.pjt.map.HydrographUtils;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.ICalcUnitResultMeta;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IScenarioResultMeta;
import org.kalypso.kalypsomodel1d2d.schema.binding.results.IHydrographCollection;
import org.kalypso.kalypsosimulationmodel.core.resultmeta.IResultMeta;
import org.kalypso.ogc.gml.IKalypsoLayerModell;
import org.kalypso.ogc.gml.serialize.GmlSerializeException;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ui.wizards.results.filters.NonCalcUnitResultViewerFilter;
import org.kalypsodeegree.model.feature.Feature;

import de.renew.workflow.connector.cases.IScenarioDataProvider;

/**
 * @author Thomas Jung
 */
public class SelectCalcUnitForHydrographWizard extends Wizard implements IWorkbenchWizard
{
  private final static String PAGE_SELECT_RESULTS_NAME = "selectResults"; //$NON-NLS-1$

  private IScenarioResultMeta m_resultModel;

  private IFolder m_scenarioFolder;

  private IKalypsoLayerModell m_mapModell;

  private IScenarioDataProvider m_dataProvider;

  public SelectCalcUnitForHydrographWizard( )
  {
    setWindowTitle( Messages.getString( "org.kalypso.ui.wizards.results.SelectCalcUnitForHydrographWizard.1" ) ); //$NON-NLS-1$
  }

  @Override
  public void addPages( )
  {
    final String title = Messages.getString( "org.kalypso.ui.wizards.results.SelectCalcUnitForHydrographWizard.2" );

    final SelectResultWizardPage selectResultWizardPage = new SelectResultWizardPage( PAGE_SELECT_RESULTS_NAME, title );

    selectResultWizardPage.setFilter( new NonCalcUnitResultViewerFilter() );
    selectResultWizardPage.setComparator( new Result1d2dMetaComparator() );

    selectResultWizardPage.setResultMeta( m_resultModel );

    addPage( selectResultWizardPage );
  }

  @Override
  public boolean performFinish( )
  {
    /* collect all time step node results of the selected calc unit */

    final SelectResultWizardPage page = (SelectResultWizardPage)getPage( PAGE_SELECT_RESULTS_NAME );
    final IResultMeta[] results = page.getSelectedResults();

    // final List<IResultMeta> resultList = new LinkedList<IResultMeta>();

    for( final IResultMeta resultMeta : results )
    {
      if( resultMeta instanceof ICalcUnitResultMeta )
      {

        final ICalcUnitResultMeta calcUnitResult = (ICalcUnitResultMeta)resultMeta;
        try
        {
          /* gml handling */

          IHydrographCollection hydrograph = HydrographUtils.getHydrograph( calcUnitResult, m_scenarioFolder );
          if( hydrograph == null )
          {
            hydrograph = HydrographUtils.createHydrograph( calcUnitResult, m_scenarioFolder );
          }

          /* save the feature */
          saveModel( calcUnitResult, hydrograph );

          /* map handling */
          // add hydrograph theme to the map
          if( hydrograph != null )
            HydrographUtils.addHydrographTheme( m_mapModell, hydrograph, calcUnitResult );

        }
        catch( final Exception e )
        {
          e.printStackTrace();
          StatusUtilities.statusFromThrowable( e, Messages.getString( "org.kalypso.ui.wizards.results.SelectCalcUnitForHydrographWizard.3" ) ); //$NON-NLS-1$
          return false;
        }
      }
    }
    return true;

  }

  // FIXME: please let the pool handle the saving!!!
  private void saveModel( final ICalcUnitResultMeta calcUnitResult, final Feature hydrograph ) throws GmlSerializeException, CoreException, IOException
  {
    // get a path
    final IPath docPath = calcUnitResult.getFullPath().append( "hydrograph" ); //$NON-NLS-1$
    final IFolder calcUnitFolder = m_scenarioFolder.getFolder( docPath );

    final IFile gmlResultFile = calcUnitFolder.getFile( "hydrograph.gml" ); //$NON-NLS-1$

    final Feature feature = hydrograph;
    OutputStreamWriter writer = null;
    try
    {
      // final String charset = gmlResultFile.getCharset();
      // FIXME: use stream + charset instead of writer

      writer = new OutputStreamWriter( new FileOutputStream( gmlResultFile.getLocation().toFile() ) );
      GmlSerializer.serializeWorkspace( writer, feature.getWorkspace(), "UTF-8", false ); //$NON-NLS-1$
      writer.close();

      // refresh workspace
      /* update resource folder */
      gmlResultFile.refreshLocal( IResource.DEPTH_INFINITE, new NullProgressMonitor() );

    }
    finally
    {
      IOUtils.closeQuietly( writer );
    }
  }

  public void setMapModel( final IKalypsoLayerModell modell )
  {
    m_mapModell = modell;
  }

  @Override
  public void init( final IWorkbench workbench, final IStructuredSelection selection )
  {
    final IHandlerService handlerService = (IHandlerService)workbench.getService( IHandlerService.class );
    final IEvaluationContext context = handlerService.getCurrentState();
    final Shell shell = (Shell)context.getVariable( ISources.ACTIVE_SHELL_NAME );
    m_dataProvider = KalypsoAFGUIFrameworkPlugin.getDataProvider();
    m_scenarioFolder = ScenarioHelper.getScenarioFolder();
    try
    {
      // Sometimes there is a NPE here... maybe wait until the models are loaded?
      m_resultModel = m_dataProvider.getModel( IScenarioResultMeta.class.getName() );
    }
    catch( final CoreException e )
    {
      Kalypso1d2dProjectPlugin.getDefault().getLog().log( e.getStatus() );
      ErrorDialog.openError( shell, Messages.getString( "org.kalypso.ui.wizards.results.SelectCalcUnitForHydrographWizard.6" ), Messages.getString( "org.kalypso.ui.wizards.results.SelectCalcUnitForHydrographWizard.7" ), e.getStatus() ); //$NON-NLS-1$ //$NON-NLS-2$
    }
  }

  public void setResultModel( final IScenarioResultMeta resultModel )
  {
    m_resultModel = resultModel;
  }

}
