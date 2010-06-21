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

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import net.opengeospatial.wps.IOValueType.ComplexValueReference;
import net.opengeospatial.wps.ProcessDescriptionType;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubMonitor;
import org.kalypso.afgui.scenarios.ScenarioHelper;
import org.kalypso.afgui.scenarios.SzenarioDataProvider;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.kalypsomodel1d2d.conv.results.IRestartInfo;
import org.kalypso.kalypsomodel1d2d.sim.i18n.Messages;
import org.kalypso.service.wps.client.WPSRequest;
import org.kalypso.service.wps.client.simulation.SimulationDelegate;
import org.kalypso.simulation.core.simspec.Modeldata;
import org.kalypso.simulation.core.util.SimulationUtilitites;

/**
 * @author kurzbach
 * 
 */
public class ExecutePreRMAKalypso
{
  private final String m_serviceEndpoint;

  private final String m_calcUnitID;

  private Modeldata m_modelInput;

  private String m_modelFileUrl;

  private String m_controlFileUrl;

  private String m_buildingFileUrl;

  private String m_bcwqFileUrl;

  private final WPSRequest m_wpsRequest;

  private String m_windFileUrl;

  private String m_windCoordFileUrl;

  /**
   * Create execute request to PreRMAKalypso WPS with no restart infos and default calcUnit defined in control model
   */
  public ExecutePreRMAKalypso( final String serviceEndpoint )
  {
    this( serviceEndpoint, null );
  }

  /**
   * Create execute request to PreRMAKalypso WPS with given restart infos and default calcUnit defined in control model
   */
  public ExecutePreRMAKalypso( final String serviceEndpoint, final List<IRestartInfo> restartInfos )
  {
    this( serviceEndpoint, restartInfos, null );
  }

  /**
   * Create execute request to PreRMAKalypso WPS with given restart infos and calcUnit
   */
  public ExecutePreRMAKalypso( final String serviceEndpoint, final List<IRestartInfo> restartInfos, final String calcUnitID )
  {
    m_serviceEndpoint = serviceEndpoint;
    m_modelInput = createInputs( restartInfos );
    m_calcUnitID = calcUnitID;
    // currently 60 minutes timeout
    m_wpsRequest = new WPSRequest( PreRMAKalypso.ID, m_serviceEndpoint, 60 * 60 * 1000 );
  }

  @SuppressWarnings("deprecation")
  public IStatus run( final IProgressMonitor monitor )
  {
    final SubMonitor progress = SubMonitor.convert( monitor, 1000 );

    try
    {
      // final List<IRestartInfo> restartInfos;
      // if( m_controlModel.getRestart() )
      // restartInfos = m_controlModel.getRestartInfos();
      // else
      // restartInfos = Collections.emptyList();

      // for getting WPS input list relative to scenario
      final SzenarioDataProvider caseDataProvider = ScenarioHelper.getScenarioDataProvider();
      final IContainer scenarioFolder = caseDataProvider.getScenarioFolder();
      final SimulationDelegate delegate = new SimulationDelegate( RMAKalypsoSimulation.ID, scenarioFolder, m_modelInput );
      delegate.init();

      final ProcessDescriptionType processDescription = m_wpsRequest.getProcessDescription( progress.newChild( 100, SubMonitor.SUPPRESS_ALL_LABELS ) );
      final Map<String, Object> inputs = delegate.createInputs( processDescription, progress.newChild( 100, SubMonitor.SUPPRESS_ALL_LABELS ) );

      // add calc unit input if desired
      if( m_calcUnitID != null )
      {
        inputs.put( PreRMAKalypso.INPUT_CALCULATION_UNIT_ID, m_calcUnitID );
      }

      // the delegate is not used for outputs
      final List<String> outputs = new ArrayList<String>();
      outputs.add( PreRMAKalypso.OUTPUT_MESH );
      outputs.add( PreRMAKalypso.OUTPUT_CONTROL );
      outputs.add( PreRMAKalypso.OUTPUT_BUILDINGS );
      outputs.add( PreRMAKalypso.OUTPUT_BC_WQ );
      outputs.add( PreRMAKalypso.OUTPUT_WIND );
      outputs.add( PreRMAKalypso.OUTPUT_WIND_COORD );

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

  private final Modeldata createInputs( final List<IRestartInfo> restartInfos )
  {
    final Map<String, String> inputs = new HashMap<String, String>();
    inputs.put( PreRMAKalypso.INPUT_CONTROL, "models/control.gml" ); //$NON-NLS-1$
    inputs.put( PreRMAKalypso.INPUT_MESH, "models/discretisation.gml" ); //$NON-NLS-1$
    inputs.put( PreRMAKalypso.INPUT_FLOW_RELATIONSHIPS, "models/flowrelations.gml" ); //$NON-NLS-1$
    inputs.put( PreRMAKalypso.INPUT_WIND_RELATIONSHIPS, "models/wind.gml" ); //$NON-NLS-1$
    inputs.put( PreRMAKalypso.INPUT_ROUGHNESS, "../.metadata/roughness.gml" ); //$NON-NLS-1$

    if( restartInfos != null )
    {
      // fill restart inputs
      int restartCount = 0;
      for( final IRestartInfo restartInfo : restartInfos )
      {
        final IPath restartFilePath = restartInfo.getRestartFilePath();
        if( restartFilePath != null )
          inputs.put( PreRMAKalypso.INPUT_RESTART_FILE_PREFIX + restartCount++, restartFilePath.toString() );
      }
    }

    // here the outputs are ignored, we will only use the Modeldata for inputs
    final Map<String, String> outputs = Collections.emptyMap();
    return SimulationUtilitites.createModelData( RMAKalypsoSimulation.ID, inputs, true, outputs, true );
  }

  private IStatus checkResults( final Map<String, ComplexValueReference> references )
  {
    try
    {
      final ComplexValueReference modelFileReference = references.get( PreRMAKalypso.OUTPUT_MESH );
      m_modelFileUrl = modelFileReference.getReference();

      final ComplexValueReference controlFileReference = references.get( PreRMAKalypso.OUTPUT_CONTROL );
      m_controlFileUrl = controlFileReference.getReference();

      final ComplexValueReference buildingFileReference = references.get( PreRMAKalypso.OUTPUT_BUILDINGS );
      m_buildingFileUrl = buildingFileReference.getReference();

      final ComplexValueReference bcwqFileReference = references.get( PreRMAKalypso.OUTPUT_BC_WQ );
      m_bcwqFileUrl = bcwqFileReference.getReference();

      final ComplexValueReference windFileReference = references.get( PreRMAKalypso.OUTPUT_WIND );
      m_windFileUrl = windFileReference.getReference();

      final ComplexValueReference windCoordFileReference = references.get( PreRMAKalypso.OUTPUT_WIND_COORD );
      m_windCoordFileUrl = windCoordFileReference.getReference();

      return Status.OK_STATUS;
    }
    catch( final Throwable e )
    {
      return StatusUtilities.statusFromThrowable( e, "One or more of the required input files for RMA-Kalypso cannot be found." );
    }
  }

  public String getModelFileUrl( )
  {
    return m_modelFileUrl;
  }

  public String getControlFileUrl( )
  {
    return m_controlFileUrl;
  }

  public String getBuildingFileUrl( )
  {
    return m_buildingFileUrl;
  }

  public String getBcwqFileUrl( )
  {
    return m_bcwqFileUrl;
  }

  public String getWindUrl( )
  {
    return m_windFileUrl;
  }

  public String getWindCoordUrl( )
  {
    return m_windCoordFileUrl;
  }

  public final WPSRequest getWpsRequest( )
  {
    return m_wpsRequest;
  }

}
