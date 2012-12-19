/** This file is part of Kalypso
 *
 *  Copyright (c) 2012 by
 *
 *  Björnsen Beratende Ingenieure GmbH, Koblenz, Germany (Bjoernsen Consulting Engineers), http://www.bjoernsen.de
 *  Technische Universität Hamburg-Harburg, Institut für Wasserbau, Hamburg, Germany
 *  (Technical University Hamburg-Harburg, Institute of River and Coastal Engineering), http://www.tu-harburg.de/wb/
 *
 *  Kalypso is free software: you can redistribute it and/or modify it under the terms  
 *  of the GNU Lesser General Public License (LGPL) as published by the Free Software 
 *  Foundation, either version 3 of the License, or (at your option) any later version.
 *
 *  Kalypso is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied 
 *  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with Kalypso.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.kalypso.kalypsomodel1d2d.sim.preprocessing;

import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.util.List;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.afgui.KalypsoAFGUIFrameworkPlugin;
import org.kalypso.commons.java.util.zip.ZipUtilities;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.conv.results.IRestartInfo;
import org.kalypso.kalypsomodel1d2d.conv.results.RestartNodes;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModel1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModel1D2DCollection;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModelGroup;
import org.kalypso.kalypsomodel1d2d.sim.IRMAPreprocessing;
import org.kalypso.kalypsosimulationmodel.core.flowrel.IFlowRelationshipModel;
import org.kalypso.kalypsosimulationmodel.core.roughness.IRoughnessClsCollection;
import org.kalypso.kalypsosimulationmodel.core.wind.IWindModel;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.simulation.core.ISimulationDataProvider;
import org.kalypso.simulation.core.SimulationException;
import org.kalypsodeegree.model.feature.GMLWorkspace;

import de.renew.workflow.connector.cases.IModel;
import de.renew.workflow.connector.cases.IScenarioDataProvider;

/**
 * Provides all data from the simulation input provider.
 * 
 * @author Gernot Belger
 */
public class PreRMAData implements IPreRMAData
{
  private static final String SERVER_INPUT_LOCAL = "InputLocal"; //$NON-NLS-1$

  private final String m_input = System.getProperty( "org.kalypso.service.wps.input" ); //$NON-NLS-1$

  private final ISimulationDataProvider m_inputProvider;

  private final IScenarioDataProvider m_dataProvider;

  public PreRMAData( final IScenarioDataProvider dataProvider, final ISimulationDataProvider inputProvider )
  {
    m_dataProvider = dataProvider;
    m_inputProvider = inputProvider;
  }

  private IContainer findScenarioFolder( )
  {
    if( StringUtils.isEmpty( m_input ) || SERVER_INPUT_LOCAL.equals( m_input ) )
    {
      final IScenarioDataProvider caseDataProvider = KalypsoAFGUIFrameworkPlugin.getDataProvider();
      return caseDataProvider.getScenarioFolder();
    }

    return null;
  }

  private <T extends IModel> T getModel( final String inputID, final Class<T> modelType ) throws CoreException, SimulationException
  {
    try
    {
      return m_dataProvider.getModel( modelType.getName() );
    }
    catch( final CoreException e )
    {
      // TODO: turbo ugly, but this was even worse before, so this is slightly better
    }

    // Fall through, try fetch from input provider
    return loadModel( inputID, modelType );
  }

  private <T extends IModel> T loadModel( final String inputID, final Class<T> modelType ) throws CoreException, SimulationException
  {
    try
    {
      final URL meshUrl = (URL)m_inputProvider.getInputForID( inputID );

      final GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( meshUrl, null );
      return modelType.cast( workspace.getRootFeature() );
    }
    catch( final SimulationException se )
    {
      throw se;
    }
    catch( final Exception e )
    {
      final String message = String.format( "Failed to load model for id '%s'", inputID ); //$NON-NLS-1$;
      throw new CoreException( new Status( IStatus.ERROR, KalypsoModel1D2DPlugin.PLUGIN_ID, message ) );
    }
  }

  @Override
  public IControlModel1D2D getControlModel( ) throws CoreException, SimulationException
  {
    // FIXME: control model (and referenced disc model) always reloaded here...
    // might be important, as the calculation changes the control model; still ugly and should be fixed
    // final IControlModelGroup controlModelGroup = getModel( IRMAPreprocessing.INPUT_CONTROL, IControlModelGroup.class );
    final IControlModelGroup controlModelGroup = loadModel( IRMAPreprocessing.INPUT_CONTROL, IControlModelGroup.class );

    return findControlModel( controlModelGroup );
  }

  private IControlModel1D2D findControlModel( final IControlModelGroup controlModelGroup ) throws SimulationException, CoreException
  {
    final IControlModel1D2DCollection controlModel1d2dCollection = controlModelGroup.getModel1D2DCollection();

    // specified calculation unit overrides control model calc unit
    final IControlModel1D2D controlModel = controlModel1d2dCollection.getActiveControlModel();

    // FIXME: not used by 1d2d preprocessing, only by swan stuff, but this does not use this code...
    if( !m_inputProvider.hasID( IRMAPreprocessing.INPUT_CALCULATION_UNIT_ID ) )
      return controlModel;

    final String calcUnitID = (String)m_inputProvider.getInputForID( IRMAPreprocessing.INPUT_CALCULATION_UNIT_ID );
    for( final IControlModel1D2D existingControlModel : controlModel1d2dCollection.getControlModels() )
    {
      final ICalculationUnit existingCalculationUnit = existingControlModel.getCalculationUnit();
      if( existingCalculationUnit.getId().equals( calcUnitID ) )
        return existingControlModel;
    }

    final String message = String.format( "Calculation unit specified ('%s') but not found. Simulation aborted.", calcUnitID ); //$NON-NLS-1$
    final IStatus status = new Status( IStatus.ERROR, KalypsoModel1D2DPlugin.PLUGIN_ID, message );
    throw new CoreException( status );
  }

  @Override
  public IFEDiscretisationModel1d2d getDiscretisationModel( ) throws CoreException, SimulationException
  {
    return getModel( IRMAPreprocessing.INPUT_MESH, IFEDiscretisationModel1d2d.class );
  }

  @Override
  public IFlowRelationshipModel getFlowRelationshipModel( ) throws CoreException, SimulationException
  {
    return getModel( IRMAPreprocessing.INPUT_FLOW_RELATIONSHIPS, IFlowRelationshipModel.class );
  }

  @Override
  public IRoughnessClsCollection getRoughnessClassCollection( ) throws CoreException, SimulationException
  {
    return getModel( IRMAPreprocessing.INPUT_ROUGHNESS, IRoughnessClsCollection.class );
  }

  @Override
  public IWindModel getWindModel( ) throws CoreException, SimulationException
  {
    return getModel( IRMAPreprocessing.INPUT_WIND_RELATIONSHIPS, IWindModel.class );
  }

  @Override
  public RestartNodes prepareRestart( final IControlModel1D2D controlModel, final File tmpDir ) throws IOException, SimulationException, CoreException
  {
    if( !controlModel.getRestart() )
      return null;

    final IContainer scenarioFolder = findScenarioFolder();

    URL restartPrefixURL = scenarioFolder.getLocationURI().toURL();
    if( !StringUtils.isEmpty( m_input ) && !SERVER_INPUT_LOCAL.equals( m_input ) )
    {
      final URL restartFileUrl = (URL)m_inputProvider.getInputForID( IRMAPreprocessing.INPUT_RESTART_FILE );
      ZipUtilities.unzip( restartFileUrl, tmpDir );
      restartPrefixURL = tmpDir.toURI().toURL();
    }

    final RestartNodes restartNodes = new RestartNodes();

    final List<IRestartInfo> restartInfos = controlModel.getRestartInfos();

    for( final Object element : restartInfos )
    {
      final IRestartInfo iRestartInfo = (IRestartInfo)element;

      URL fullPrefixURL = restartPrefixURL;

      if( !restartPrefixURL.toString().endsWith( "/" ) ) //$NON-NLS-1$
        fullPrefixURL = new URL( restartPrefixURL.toString() + "/" ); //$NON-NLS-1$

      final URL restartURL = new URL( fullPrefixURL, iRestartInfo.getRestartFilePath().toPortableString() );
      restartNodes.addResultUrl( restartURL );
    }

    return restartNodes;
  }
}