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
package org.kalypso.kalypsomodel1d2d.sim;

import java.io.File;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import net.opengeospatial.wps.IOValueType.ComplexValueReference;
import net.opengeospatial.wps.ProcessDescriptionType;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubMonitor;
import org.kalypso.afgui.KalypsoAFGUIFrameworkPlugin;
import org.kalypso.commons.java.util.zip.ZipUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModel1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModelGroup;
import org.kalypso.kalypsomodel1d2d.sim.i18n.Messages;
import org.kalypso.kalypsosimulationmodel.core.wind.IWindDataModelSystem;
import org.kalypso.kalypsosimulationmodel.core.wind.IWindDataProvider;
import org.kalypso.kalypsosimulationmodel.core.wind.IWindModel;
import org.kalypso.service.wps.client.WPSRequest;
import org.kalypso.service.wps.client.simulation.SimulationDelegate;
import org.kalypso.simulation.core.simspec.Modeldata;
import org.kalypso.simulation.core.util.SimulationUtilitites;

import de.renew.workflow.connector.cases.IScenarioDataProvider;

/**
 * @author kurzbach
 * @author ig
 */
public class ExecutePreSWANKalypso
{
  private final String m_serviceEndpoint;

  private final String m_calcUnitID;

  private final Modeldata m_modelInput;

  private String m_modelDataSimulationPath;

  private final WPSRequest m_wpsRequest;

  private final URI m_rmaOutputPath;

  private boolean m_boolDoHotStart = false;

  private URL m_urlAdditionalCoordFile;

  private final IContainer m_scenarioFolder;

  private final IWindModel m_windRelationshipModel;

  /**
   * Create execute request to PreRMAKalypso WPS with given restart infos and calcUnit
   */
  public ExecutePreSWANKalypso( final String serviceEndpoint, final String calcUnitID, final URI rmaCalcPath ) throws CoreException
  {
    final IScenarioDataProvider caseDataProvider = KalypsoAFGUIFrameworkPlugin.getDataProvider();
    m_scenarioFolder = caseDataProvider.getScenarioFolder();
    final IControlModelGroup controlModelGroup = caseDataProvider.getModel( IControlModelGroup.class.getName() );
    final IControlModel1D2D controlModel = controlModelGroup.getModel1D2DCollection().getActiveControlModel();

    m_boolDoHotStart = controlModel.getINITialValuesSWAN() == 3;
    try
    {
      m_urlAdditionalCoordFile = new URL( controlModel.getInputFileAdditionalCoordSWAN() );
    }
    catch( final Exception e )
    {
      m_urlAdditionalCoordFile = null;
    }

    m_windRelationshipModel = caseDataProvider.getModel( IWindModel.class.getName() );

//    m_boolCopyWindBinaries = !controlModel.isConstantWindSWAN() &&
    m_serviceEndpoint = serviceEndpoint;
    m_rmaOutputPath = rmaCalcPath;
    m_calcUnitID = calcUnitID;
    m_modelInput = createInputs();
    // currently 60 minutes timeout
    m_wpsRequest = new WPSRequest( PreSWANKalypso.ID, m_serviceEndpoint, 60 * 60 * 1000 );

  }

  @SuppressWarnings( "deprecation" )
  public IStatus run( final IProgressMonitor monitor )
  {
    final SubMonitor progress = SubMonitor.convert( monitor, 1000 );

    try
    {
      // for getting WPS input list relative to scenario
      final SimulationDelegate delegate = new SimulationDelegate( SWANKalypsoSimulation.ID, m_scenarioFolder, m_modelInput );
      delegate.init();

      final ProcessDescriptionType processDescription = m_wpsRequest.getProcessDescription( progress.newChild( 100, SubMonitor.SUPPRESS_ALL_LABELS ) );
      final Map<String, Object> inputs = delegate.createInputs( processDescription, progress.newChild( 100, SubMonitor.SUPPRESS_ALL_LABELS ) );

      // add calc unit input if desired
      if( m_calcUnitID != null )
      {
        inputs.put( IRMAPreprocessing.INPUT_CALCULATION_UNIT_ID, m_calcUnitID );
      }

      // the delegate is not used for outputs
      final List<String> outputs = new ArrayList<>();
      outputs.add( PreSWANKalypso.OUTPUT_PATH_SWAN );

      // run the preprocessing, this will create references to ascii files from gml files
      final IStatus preRMAstatus = m_wpsRequest.run( inputs, outputs, progress.newChild( 800, SubMonitor.SUPPRESS_NONE ) );

      // abort on error
      if( !preRMAstatus.isOK() )
        return preRMAstatus;

      final Map<String, ComplexValueReference> references = m_wpsRequest.getReferences();
      final IStatus resultStatus = checkResults( references );

      // abort if results are missing
      if( !resultStatus.isOK() )
        return resultStatus;

      // if we get here, everything is OK
      return Status.OK_STATUS;
    }
    catch( final Throwable e )
    {
      return StatusUtilities.statusFromThrowable( e, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.RMA10Calculation.22" ) ); //$NON-NLS-1$
    }
    finally
    {
      progress.done();
    }
  }

  private final Modeldata createInputs( )
  {
    final Map<String, String> inputs = new HashMap<>();
    inputs.put( IRMAPreprocessing.INPUT_CONTROL, "models/control.gml" ); //$NON-NLS-1$
    inputs.put( IRMAPreprocessing.INPUT_MESH, "models/discretisation.gml" ); //$NON-NLS-1$
    inputs.put( IRMAPreprocessing.INPUT_FLOW_RELATIONSHIPS, "models/flowrelations.gml" ); //$NON-NLS-1$
    inputs.put( IRMAPreprocessing.INPUT_WIND_RELATIONSHIPS, "models/wind.gml" ); //$NON-NLS-1$
    try
    {
      inputs.put( PreSWANKalypso.OUTPUT_PATH_RMA, m_rmaOutputPath.toURL().toExternalForm() );
      inputs.put( PreSWANKalypso.INPUT_PATH_RESULT_META, "models/scenarioResultMeta.gml" ); //$NON-NLS-1$
    }
    catch( final MalformedURLException e )
    {
      KalypsoCorePlugin.getDefault().getLog().log( StatusUtilities.statusFromThrowable( e ) );
    }

    if( m_serviceEndpoint.equals( WPSRequest.SERVICE_LOCAL ) )
    {
      if( m_urlAdditionalCoordFile != null )
      {
        inputs.put( PreSWANKalypso.ADDITIONAL_DATA_FILE, m_urlAdditionalCoordFile.toExternalForm() );
      }
    }
    else
    {
      final List<File> lListFilesToZip = new ArrayList<>();
      final String additionalDataFileName = "models/native_tem/" + PreSWANKalypso.ADDITIONAL_DATA_FILE + ".zip"; //$NON-NLS-1$ //$NON-NLS-2$
      final File zipOutput = new File( additionalDataFileName );
      if( zipOutput.exists() )
      {
        zipOutput.delete();
      }
      if( m_urlAdditionalCoordFile != null )
      {
        try
        {
          lListFilesToZip.add( new File( m_urlAdditionalCoordFile.toURI() ) );
          inputs.put( PreSWANKalypso.ADDITIONAL_DATA_FILE, additionalDataFileName );
        }
        catch( final Exception e )
        {
          // TODO Auto-generated catch block
          e.printStackTrace();
        }
      }

      getWindBinaryFilesAsList( lListFilesToZip, m_windRelationshipModel );

      if( lListFilesToZip.size() > 0 )
      {
        try
        {
          ZipUtilities.zip( zipOutput, lListFilesToZip.toArray( new File[lListFilesToZip.size()] ), new File( "models/native_tem/" ) ); //$NON-NLS-1$
        }
        catch( final IOException e )
        {
          // TODO Auto-generated catch block
          e.printStackTrace();
        }
      }
    }

    // TODO: implement selection if it will be needed for restart files... write now it is just taken from last run of
    // this calc unit
    if( m_boolDoHotStart )
    {
      inputs.put( PreSWANKalypso.HOT_START_FILE, "results/" + m_calcUnitID + "/" + ISimulation1D2DConstants.SIM_SWAN_TRIANGLE_FILE + ".zip" ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    }

    // may be implemented also for restarting both of the calculations coupled
    // if( restartInfos != null )
    // {
    // // fill restart inputs
    // int restartCount = 0;
    // for( final IRestartInfo restartInfo : restartInfos )
    // {
    // final IPath restartFilePath = restartInfo.getRestartFilePath();
    // if( restartFilePath != null )
    // inputs.put( PreRMAKalypso.INPUT_RESTART_FILE_PREFIX + restartCount++, restartFilePath.toString() );
    // }
    // }

    // here the outputs are ignored, we will only use the Modeldata for inputs
    final Map<String, String> outputs = Collections.emptyMap();
    return SimulationUtilitites.createModelData( PreSWANKalypso.ID, inputs, true, outputs, true );
  }

  private void getWindBinaryFilesAsList( final List<File> listFilesToPutIn, final IWindModel windRelationshipModel )
  {
    final List<IWindDataModelSystem> lListSystems = windRelationshipModel.getWindDataModelSystems();

    for( final IWindDataModelSystem lWindSystem : lListSystems )
    {
      for( final Object lWindDataObject : lWindSystem.getWindDataModels() )
      {
        final IWindDataProvider lWindData = (IWindDataProvider)lWindDataObject;
        try
        {
          listFilesToPutIn.add( new File( lWindData.getDataFileURL().toURI() ) );
        }
        catch( final URISyntaxException e )
        {
          e.printStackTrace();
        }
      }
    }
  }

  private IStatus checkResults( final Map<String, ComplexValueReference> references )
  {
    try
    {
      final ComplexValueReference modelPathReference = references.get( PreSWANKalypso.OUTPUT_PATH_SWAN );
      m_modelDataSimulationPath = modelPathReference.getReference();

      return Status.OK_STATUS;
    }
    catch( final Throwable e )
    {
      return StatusUtilities.statusFromThrowable( e, Messages.getString( "ExecutePreSWANKalypso.2" ) ); //$NON-NLS-1$
    }
  }

  public String getSWANModelPath( )
  {
    return m_modelDataSimulationPath;
  }

  public final WPSRequest getWpsRequest( )
  {
    return m_wpsRequest;
  }

}
