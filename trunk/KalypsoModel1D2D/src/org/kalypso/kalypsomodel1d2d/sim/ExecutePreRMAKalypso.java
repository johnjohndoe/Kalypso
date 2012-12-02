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
import java.net.URL;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import net.opengeospatial.wps.IOValueType.ComplexValueReference;
import net.opengeospatial.wps.ProcessDescriptionType;

import org.apache.commons.lang3.ArrayUtils;
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.MultiStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubMonitor;
import org.kalypso.afgui.KalypsoAFGUIFrameworkPlugin;
import org.kalypso.commons.java.util.zip.ZipUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.conv.results.IRestartInfo;
import org.kalypso.kalypsomodel1d2d.sim.i18n.Messages;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.service.wps.client.WPSRequest;
import org.kalypso.service.wps.client.simulation.SimulationDelegate;
import org.kalypso.simulation.core.simspec.Modeldata;
import org.kalypso.simulation.core.simspec.Modeldata.Input;
import org.kalypso.simulation.core.util.SimulationUtilitites;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.gml.binding.commons.IStatusCollection;

import de.renew.workflow.connector.cases.IScenarioDataProvider;

/**
 * @author kurzbach
 */
public class ExecutePreRMAKalypso
{
  private final String m_serviceEndpoint;

  private final Modeldata m_modelInput;

  private String m_modelFileUrl;

  private String m_controlFileUrl;

  private String m_buildingFileUrl;

  private String m_bcwqFileUrl;

  private final WPSRequest m_wpsRequest;

  private String m_windFileUrl;

  private String m_windCoordFileUrl;

  private final IScenarioDataProvider m_caseDataProvider;

  private final IContainer m_scenarioFolder;

  /**
   * Create execute request to PreRMAKalypso WPS with given restart infos and default calcUnit defined in control model
   */
  public ExecutePreRMAKalypso( final String serviceEndpoint, final List<IRestartInfo> restartInfos )
  {
    m_caseDataProvider = KalypsoAFGUIFrameworkPlugin.getDataProvider();
    m_scenarioFolder = m_caseDataProvider.getScenarioFolder();

    m_serviceEndpoint = serviceEndpoint;
    m_modelInput = createInputs( restartInfos );
    // currently 60 minutes timeout... is this ok? 1h calculation with rma is not unusual...
    m_wpsRequest = new WPSRequest( PreRMAKalypso.ID, m_serviceEndpoint, 60 * 60 * 1000 );
  }

  public IStatus run( final IProgressMonitor monitor )
  {
    final SubMonitor progress = SubMonitor.convert( monitor, 1000 );

    try
    {
      // for getting WPS input list relative to scenario
      final IScenarioDataProvider caseDataProvider = KalypsoAFGUIFrameworkPlugin.getDataProvider();
      final IContainer scenarioFolder = caseDataProvider.getScenarioFolder();
      final SimulationDelegate delegate = new SimulationDelegate( PreRMAKalypso.ID, scenarioFolder, m_modelInput );
      delegate.init();

      final ProcessDescriptionType processDescription = m_wpsRequest.getProcessDescription( progress.newChild( 100, SubMonitor.SUPPRESS_ALL_LABELS ) );
      final Map<String, Object> inputs = delegate.createInputs( processDescription, progress.newChild( 100, SubMonitor.SUPPRESS_ALL_LABELS ) );

      // the delegate is not used for outputs
      final List<String> outputs = new ArrayList<>();
      outputs.add( IRMAPreprocessing.OUTPUT_LOG );
      outputs.add( IRMAPreprocessing.OUTPUT_MESH );
      outputs.add( IRMAPreprocessing.OUTPUT_CONTROL );
      outputs.add( IRMAPreprocessing.OUTPUT_BUILDINGS );
      outputs.add( IRMAPreprocessing.OUTPUT_BC_WQ );
      outputs.add( IRMAPreprocessing.OUTPUT_WIND );
      outputs.add( IRMAPreprocessing.OUTPUT_WIND_COORD );

      // run the preprocessing, this will create references to ascii files from gml files
      /* final IStatus preRMAstatus = */
      m_wpsRequest.run( inputs, outputs, progress.newChild( 800, SubMonitor.SUPPRESS_NONE ) );

      final Map<String, ComplexValueReference> references = m_wpsRequest.getReferences();
      final IStatus logStatus = readLogfile( references );
      if( logStatus.matches( IStatus.ERROR ) )
        return logStatus;

      final IStatus resultStatus = checkResults( references );

      // abort if results are missing
      if( !resultStatus.isOK() )
      {
        /* append to end of preprocessing status */
        final IStatus[] logChildren = logStatus.getChildren();

        final IStatus[] children = ArrayUtils.add( logChildren, resultStatus );

        return new MultiStatus( KalypsoModel1D2DPlugin.PLUGIN_ID, ISimulation1D2DConstants.CODE_PRE, children, logStatus.getMessage(), null );
      }

      return logStatus;

      // FIXME: preserve ascii files: we could now already zip the rma ascii files as output.zip; this would be nice,as we would even keep the files in the cae we get an error later.
      // TODO: also keep control files etc, not only the model.2d file.

      // abort on error
      // if( !preRMAstatus.isOK() )
      // return preRMAstatus;
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

  private IStatus readLogfile( final Map<String, ComplexValueReference> references )
  {
    final ComplexValueReference logReference = references.get( IRMAPreprocessing.OUTPUT_LOG );
    if( logReference == null )
      return new Status( IStatus.ERROR, KalypsoModel1D2DPlugin.PLUGIN_ID, "Preprocessing: missing log file, internal error." ); //$NON-NLS-1$

    try
    {
      final String logFileUrl = logReference.getReference();

      final URL logFile = new URL( logFileUrl );

      final GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( logFile, null );

      final IStatusCollection stati = (IStatusCollection)workspace.getRootFeature();

      final IStatus status = stati.toStatus();
      return new MultiStatus( KalypsoModel1D2DPlugin.PLUGIN_ID, 0, status.getChildren(), "Preprocessing", null ); //$NON-NLS-1$
    }
    catch( final Exception e )
    {
      return new Status( IStatus.ERROR, KalypsoModel1D2DPlugin.PLUGIN_ID, "Preprocessing: failed to read log file, internal error.", e ); //$NON-NLS-1$
    }
  }

  private final Modeldata createInputs( final List<IRestartInfo> restartInfos )
  {
    final Map<String, String> inputs = new HashMap<>();

    // FIXME: bad, get pathes from central place instead
    inputs.put( IRMAPreprocessing.INPUT_CONTROL, "models/control.gml" ); //$NON-NLS-1$
    inputs.put( IRMAPreprocessing.INPUT_MESH, "models/discretisation.gml" ); //$NON-NLS-1$
    inputs.put( IRMAPreprocessing.INPUT_FLOW_RELATIONSHIPS, "models/flowrelations.gml" ); //$NON-NLS-1$
    inputs.put( IRMAPreprocessing.INPUT_WIND_RELATIONSHIPS, "models/wind.gml" ); //$NON-NLS-1$

    if( restartInfos != null )
    {
      // fill restart ip
      int restartCount = 0;
      if( restartInfos.size() > 0 && !WPSRequest.SERVICE_LOCAL.equals( m_serviceEndpoint ) )
      {
        final String restartFilePath = zipRestartInfos( restartInfos );
        if( restartFilePath != null )
          inputs.put( IRMAPreprocessing.INPUT_RESTART_FILE_PREFIX + restartCount++, restartFilePath );
      }
    }

    // here the outputs are ignored, we will only use the Modeldata for inputs
    final Map<String, String> outputs = Collections.emptyMap();
    final Modeldata data = SimulationUtilitites.createModelData( RMAKalypsoSimulation.ID, inputs, true, outputs, true );

    /* Special case for roughness.gml: NOT relative to calc case */
    final Input input = SimulationUtilitites.createInput( IRMAPreprocessing.INPUT_ROUGHNESS, ".metadata/roughness.gml", false, false ); //$NON-NLS-1$
    data.getInput().add( input );

    return data;
  }

  private String zipRestartInfos( final List<IRestartInfo> restartInfos )
  {
    try
    {
      final File zipOutput = File.createTempFile( "kalypsoMultiRestartFile" + new Date().getTime(), ".zip" ); //$NON-NLS-1$  //$NON-NLS-2$
      zipOutput.delete();
      final List<File> files = new ArrayList<>();
      for( final IRestartInfo restartInfo : restartInfos )
      {
        final IPath restartFilePath = restartInfo.getRestartFilePath();
        final IResource foundPath = m_scenarioFolder.findMember( restartFilePath );

        if( restartFilePath != null )
          files.add( new File( foundPath.getLocationURI() ) );
      }
      ZipUtilities.zip( zipOutput, files.toArray( new File[files.size()] ), new File( m_scenarioFolder.getLocationURI() ) );
      return zipOutput.toURI().toURL().toExternalForm();
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }
    return ""; //$NON-NLS-1$
  }

  private IStatus checkResults( final Map<String, ComplexValueReference> references )
  {
    try
    {
      final ComplexValueReference modelFileReference = references.get( IRMAPreprocessing.OUTPUT_MESH );
      m_modelFileUrl = modelFileReference.getReference();

      final ComplexValueReference controlFileReference = references.get( IRMAPreprocessing.OUTPUT_CONTROL );
      m_controlFileUrl = controlFileReference.getReference();

      final ComplexValueReference buildingFileReference = references.get( IRMAPreprocessing.OUTPUT_BUILDINGS );
      m_buildingFileUrl = buildingFileReference.getReference();

      final ComplexValueReference bcwqFileReference = references.get( IRMAPreprocessing.OUTPUT_BC_WQ );
      m_bcwqFileUrl = bcwqFileReference.getReference();

      final ComplexValueReference windFileReference = references.get( IRMAPreprocessing.OUTPUT_WIND );
      m_windFileUrl = windFileReference.getReference();

      final ComplexValueReference windCoordFileReference = references.get( IRMAPreprocessing.OUTPUT_WIND_COORD );
      m_windCoordFileUrl = windCoordFileReference.getReference();

      return Status.OK_STATUS;
    }
    catch( final NullPointerException e )
    {
      // FIXME: still ugly!
      return new Status( IStatus.ERROR, KalypsoModel1D2DPlugin.PLUGIN_ID, Messages.getString( "ExecutePreRMAKalypso.0" ) ); //$NON-NLS-1$
    }
    catch( final Throwable e )
    {
      return StatusUtilities.statusFromThrowable( e, Messages.getString( "ExecutePreRMAKalypso.0" ) ); //$NON-NLS-1$
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