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

import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.ISources;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.handlers.IHandlerService;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.kalypso1d2d.pjt.Kalypso1d2dProjectPlugin;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IScenarioResultMeta;
import org.kalypso.kalypsosimulationmodel.core.modeling.IModel;
import org.kalypso.ogc.gml.IKalypsoLayerModell;
import org.kalypso.ui.wizard.IKalypsoDataImportWizard;
import org.kalypso.ui.wizards.results.filters.NonCalcUnitResultViewerFilter;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;

import de.renew.workflow.connector.cases.CaseHandlingSourceProvider;
import de.renew.workflow.connector.cases.ICaseDataProvider;

/**
 * @author Thomas Jung
 * 
 */
public class SelectCalcUnitForHydrographWizard extends Wizard implements IKalypsoDataImportWizard
{

  private final static String PAGE_SELECT_RESULTS_NAME = "selectResults";

  private IScenarioResultMeta m_resultModel;

  private final IFolder m_scenarioFolder = null;

  private final ICaseDataProvider<IModel> m_modelProvider = null;

  public SelectCalcUnitForHydrographWizard( )
  {
    setWindowTitle( "1D2D-Ergebnisse" );
  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#addPages()
   */
  @Override
  public void addPages( )
  {
    final NonCalcUnitResultViewerFilter resultFilter = new NonCalcUnitResultViewerFilter();
    final SelectResultWizardPage selectResultWizardPage = new SelectResultWizardPage( PAGE_SELECT_RESULTS_NAME, "Teilmodelle für Ganglinienerstellung auswählen", null, resultFilter, null );

    selectResultWizardPage.setResultMeta( m_resultModel );

    addPage( selectResultWizardPage );
  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#performFinish()
   */
  @Override
  public boolean performFinish( )
  {
    // TODO Auto-generated method stub
    return false;
  }

  /**
   * @see org.kalypso.ui.wizard.IKalypsoDataImportWizard#setCommandTarget(org.kalypso.commons.command.ICommandTarget)
   */
  public void setCommandTarget( ICommandTarget commandTarget )
  {
    // TODO Auto-generated method stub

  }

  /**
   * @see org.kalypso.ui.wizard.IKalypsoDataImportWizard#setMapModel(org.kalypso.ogc.gml.IKalypsoLayerModell)
   */
  public void setMapModel( IKalypsoLayerModell modell )
  {
    // TODO Auto-generated method stub

  }

  /**
   * @see org.eclipse.ui.IWorkbenchWizard#init(org.eclipse.ui.IWorkbench,
   *      org.eclipse.jface.viewers.IStructuredSelection)
   */
  @SuppressWarnings("unchecked")
  public void init( final IWorkbench workbench, final IStructuredSelection selection )
  {
    final IHandlerService handlerService = (IHandlerService) workbench.getService( IHandlerService.class );
    final IEvaluationContext context = handlerService.getCurrentState();
    final Shell shell = (Shell) context.getVariable( ISources.ACTIVE_SHELL_NAME );
    final ICaseDataProvider<IFeatureWrapper2> modelProvider = (ICaseDataProvider<IFeatureWrapper2>) context.getVariable( CaseHandlingSourceProvider.ACTIVE_CASE_DATA_PROVIDER_NAME );

    try
    {
      // Sometimes there is a NPE here... maybe wait until the models are loaded?
      m_resultModel = modelProvider.getModel( IScenarioResultMeta.class );
    }
    catch( final CoreException e )
    {
      Kalypso1d2dProjectPlugin.getDefault().getLog().log( e.getStatus() );
      ErrorDialog.openError( shell, "1D2D-Ergebnisse", "Ergebnis-Metadaten nicht vorhanden.", e.getStatus() );
    }
  }

  public void setResultModel( IScenarioResultMeta resultModel )
  {
    m_resultModel = resultModel;
  }

}
