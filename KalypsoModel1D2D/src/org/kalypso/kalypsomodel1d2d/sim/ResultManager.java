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

import java.io.BufferedReader;
import java.io.DataInputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.net.MalformedURLException;
import java.net.URISyntaxException;
import java.nio.charset.Charset;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.Formatter;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.StringTokenizer;
import java.util.TreeSet;
import java.util.zip.ZipException;

import javax.xml.datatype.XMLGregorianCalendar;

import org.apache.commons.io.FilenameUtils;
import org.apache.commons.vfs2.FileName;
import org.apache.commons.vfs2.FileObject;
import org.apache.commons.vfs2.FileSystemException;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.MultiStatus;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubMonitor;
import org.kalypso.commons.io.VFSUtilities;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.commons.java.util.zip.ZipUtilities;
import org.kalypso.commons.vfs.FileSystemManagerWrapper;
import org.kalypso.contribs.eclipse.core.runtime.PluginUtilities;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.contribs.java.lang.NumberUtils;
import org.kalypso.contribs.java.util.DateUtilities;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.conv.SWANDataConverterHelper;
import org.kalypso.kalypsomodel1d2d.conv.results.NodeResultHelper;
import org.kalypso.kalypsomodel1d2d.conv.results.ResultMeta1d2dHelper;
import org.kalypso.kalypsomodel1d2d.conv.results.ResultType;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModel1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModelGroup;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.ICalcUnitResultMeta;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IScenarioResultMeta;
import org.kalypso.kalypsomodel1d2d.schema.dict.Kalypso1D2DDictConstants;
import org.kalypso.kalypsomodel1d2d.sim.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.ui.geolog.IGeoLog;
import org.kalypso.kalypsosimulationmodel.core.flowrel.IFlowRelationshipModel;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.ComponentUtilities;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.simulation.core.util.SimulationUtilitites;
import org.kalypsodeegree.model.geometry.GM_Position;

import de.renew.workflow.connector.cases.IScenarioDataProvider;

/**
 * This runnable will be called while running the 2d-exe and will check for new .2d result files.<br>
 * Every new 2d result file we be processed in order to return it to the kalypso client.
 * 
 * @author Gernot Belger
 */
public class ResultManager implements ISimulation1D2DConstants
{
  private static FileSystemManagerWrapper m_vfsManager;

  private final NodeResultMinMaxCatcher m_minMaxCatcher = new NodeResultMinMaxCatcher();

  private final List<ResultType> m_parameters = new ArrayList<>();

  private final IGeoLog m_geoLog;

  private final IControlModel1D2D m_controlModel;

  private final IFlowRelationshipModel m_flowModel;

  private final IScenarioResultMeta m_scenarioMeta;

  private final IFEDiscretisationModel1d2d m_discModel;

  private FileObject[] m_stepsToProcess;

  private File m_outputDir;

  private final FileObject m_resultDir;

  private FileObject m_resultDirSWAN;

  private Map<Date, FileObject> m_mapDateFile;

  private final TupleResult m_timeSteps;

  private ICalcUnitResultMeta m_calcUnitMeta = null;

  private Date[] m_allDates = null;

  private Date[] m_datesToProcess;

  private FileObject m_simulationResultFile = null;

  public ResultManager( final FileObject fileObjectRMA, final FileObject fileObjectSWAN, final IScenarioDataProvider caseDataProvider, final IGeoLog geoLog ) throws CoreException
  {
    // FIXME: avoid throwing exception in constructor; clients should provide the models, not the caseDataProvider
    this( fileObjectRMA, fileObjectSWAN, (IFEDiscretisationModel1d2d)caseDataProvider.getModel( IFEDiscretisationModel1d2d.class.getName() ), ((IControlModelGroup)caseDataProvider.getModel( IControlModelGroup.class.getName() )).getModel1D2DCollection().getActiveControlModel(), (IFlowRelationshipModel)caseDataProvider.getModel( IFlowRelationshipModel.class.getName() ), (IScenarioResultMeta)caseDataProvider.getModel( IScenarioResultMeta.class.getName() ), geoLog );
  }

  public ResultManager( final FileObject fileObjectRMA, final FileObject fileObjectSWAN, final IFEDiscretisationModel1d2d discretisationModel1d2d, final IControlModel1D2D controlModel, final IFlowRelationshipModel flowRelModel, final IScenarioResultMeta scenarioMetaData, final IGeoLog geoLog )
  {
    m_resultDir = fileObjectRMA;
    m_resultDirSWAN = fileObjectSWAN;

    m_controlModel = controlModel;
    m_flowModel = flowRelModel;
    m_discModel = discretisationModel1d2d;
    m_scenarioMeta = scenarioMetaData;

    m_geoLog = geoLog;

    m_parameters.add( ResultType.DEPTH );
    m_parameters.add( ResultType.WATERLEVEL );
    m_parameters.add( ResultType.VELOCITY );

    if( m_calcUnitMeta == null )
      m_calcUnitMeta = m_scenarioMeta.findCalcUnitMetaResult( m_controlModel.getCalculationUnit().getId() );

    if( controlModel == null )
      m_timeSteps = null;
    else
    {
      final IObservation<TupleResult> obs = controlModel.getTimeSteps();
      m_timeSteps = obs.getResult();
    }
  }

  public ResultManager( final FileObject fileObjectRMA, final FileObject fileObjectSWAN, final IScenarioDataProvider caseDataProvider, final IGeoLog geoLog, final ICalcUnitResultMeta calcUnitResultMeta ) throws CoreException
  {
    this( fileObjectRMA, fileObjectSWAN, (IFEDiscretisationModel1d2d)caseDataProvider.getModel( IFEDiscretisationModel1d2d.class.getName() ), ((IControlModelGroup)caseDataProvider.getModel( IControlModelGroup.class.getName() )).getModel1D2DCollection().getActiveControlModel(), (IFlowRelationshipModel)caseDataProvider.getModel( IFlowRelationshipModel.class.getName() ), (IScenarioResultMeta)caseDataProvider.getModel( IScenarioResultMeta.class.getName() ), geoLog );

    m_calcUnitMeta = calcUnitResultMeta;
  }

  public IStatus processResults( final ICalcUnitResultMeta calcUnitMeta, final boolean doFullEvaluate, final IProgressMonitor monitor )
  {
    final SubMonitor progress = SubMonitor.convert( monitor, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ResultManager.3" ), 1000 ); //$NON-NLS-1$
    progress.subTask( Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ResultManager.4" ) ); //$NON-NLS-1$

    try
    {
      m_vfsManager = VFSUtilities.getNewManager();

      m_outputDir = SimulationUtilitites.createSimulationTmpDir( "output" + calcUnitMeta.getCalcUnit() ); //$NON-NLS-1$

      if( m_resultDirSWAN != null )
      {
        prepareSWANResults();
      }

      if( m_stepsToProcess == null )
        return new Status( IStatus.OK, KalypsoModel1D2DPlugin.PLUGIN_ID, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ResultManager.5" ) ); //$NON-NLS-1$

      /* Process all remaining .2d files. Now including min and max files */
      final FileObject[] existing2dFiles = m_stepsToProcess;

      progress.setWorkRemaining( existing2dFiles.length );

      IStatus[] fileStati = new IStatus[existing2dFiles.length];

      if( m_controlModel.calculateTelemac() )
      {
        fileStati = new IStatus[1];
        fileStati[0] = processTelemacResultFile( calcUnitMeta, progress.newChild( 1 ), doFullEvaluate );
      }
      else
      {
        for( int i = 0; i < fileStati.length; i++ )
        {
          final FileObject file = existing2dFiles[i];
          fileStati[i] = processResultFile( file, calcUnitMeta, progress.newChild( 1 ), doFullEvaluate );
        }
      }

//      return new Status( IStatus.OK, KalypsoModel1D2DPlugin.PLUGIN_ID, CODE_POST, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ResultManager.7" ), null ); //$NON-NLS-1$

      final MultiStatus multiStatus = new MultiStatus( PluginUtilities.id( KalypsoModel1D2DPlugin.getDefault() ), CODE_POST, fileStati, "", null ); //$NON-NLS-1$
      if( multiStatus.isOK() )
        return new Status( IStatus.OK, KalypsoModel1D2DPlugin.PLUGIN_ID, CODE_POST, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ResultManager.7" ), null ); //$NON-NLS-1$

      if( multiStatus.matches( IStatus.CANCEL ) )
        return new Status( IStatus.WARNING, KalypsoModel1D2DPlugin.PLUGIN_ID, CODE_POST, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ResultManager.8" ), null ); //$NON-NLS-1$

      if( multiStatus.matches( IStatus.WARNING ) )
        return new MultiStatus( PluginUtilities.id( KalypsoModel1D2DPlugin.getDefault() ), CODE_POST, fileStati, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ResultManager.9" ), null ); //$NON-NLS-1$

      if( multiStatus.matches( IStatus.ERROR ) )
        return new MultiStatus( PluginUtilities.id( KalypsoModel1D2DPlugin.getDefault() ), CODE_POST, fileStati, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ResultManager.10" ), null ); //$NON-NLS-1$

      return new MultiStatus( PluginUtilities.id( KalypsoModel1D2DPlugin.getDefault() ), CODE_POST, fileStati, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ResultManager.11" ), null ); //$NON-NLS-1$
    }
    catch( final OperationCanceledException e )
    {
      return new Status( IStatus.CANCEL, KalypsoModel1D2DPlugin.PLUGIN_ID, CODE_POST, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ResultManager.12" ), e ); //$NON-NLS-1$
    }
    catch( final FileSystemException e )
    {
      return new Status( IStatus.CANCEL, KalypsoModel1D2DPlugin.PLUGIN_ID, CODE_POST, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ResultManager.12" ), e ); //$NON-NLS-1$
    }
    catch( final CoreException e )
    {
      return e.getStatus();
    }
    catch( final Throwable e )
    {
      return new Status( IStatus.ERROR, KalypsoModel1D2DPlugin.PLUGIN_ID, CODE_POST, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ResultManager.13" ), e ); //$NON-NLS-1$
    }
    finally
    {
      if( m_resultDir != null )
      {
        try
        {
          m_resultDir.close();

          if( m_resultDirSWAN != null )
          {
            m_resultDirSWAN.close();
          }

          m_vfsManager.close();
        }
        catch( final IOException e )
        {
          // gobble
        }
      }
    }
  }

  /*
   * compress and copy swan result file into temp directory, the whole folder will be moved into the project after
   * successful finishing the complete result evaluation.
   */
  private void prepareSWANResults( ) throws ZipException, IOException, URISyntaxException, FileSystemException, MalformedURLException
  {
    if( !m_resultDirSWAN.getName().getBaseName().endsWith( "zip" ) ) //$NON-NLS-1$
    {
      try
      {
        final FileObject swanResFile = m_resultDirSWAN.getChild( ISimulation1D2DConstants.SIM_SWAN_TRIANGLE_FILE + "." + ISimulation1D2DConstants.SIM_SWAN_MAT_RESULT_EXT ); //$NON-NLS-1$
        final FileObject swanResShiftFile = m_resultDirSWAN.getChild( ISimulation1D2DConstants.SIM_SWAN_COORD_SHIFT_FILE );
        final FileObject swanResOutTabFile = m_resultDirSWAN.getChild( ISimulation1D2DConstants.SIM_SWAN_TRIANGLE_FILE + "_out.tab" ); //$NON-NLS-1$
        processSWANTabFile( swanResOutTabFile, swanResShiftFile );
        final File zipOutput = new File( m_outputDir, ISimulation1D2DConstants.SIM_SWAN_TRIANGLE_FILE + ".zip" ); //$NON-NLS-1$
        final List<File> lListFilesToZip = new ArrayList<>();
        lListFilesToZip.add( new File( swanResFile.getURL().toURI() ) );
        lListFilesToZip.add( new File( swanResShiftFile.getURL().toURI() ) );
        lListFilesToZip.add( new File( swanResOutTabFile.getURL().toURI() ) );
        if( m_controlModel.getINITialValuesSWAN() == 3 )
        {
          final FileObject swanResHotFile = m_resultDirSWAN.getChild( ISimulation1D2DConstants.SIM_SWAN_HOT_FILE );
          lListFilesToZip.add( new File( swanResHotFile.getURL().toURI() ) );
        }
        ZipUtilities.zip( zipOutput, lListFilesToZip.toArray( new File[lListFilesToZip.size()] ), new File( m_resultDirSWAN.getURL().toURI() ) );
        swanResFile.close();
        swanResShiftFile.close();
        swanResOutTabFile.close();
      }
      catch( final Exception e )
      {
        e.printStackTrace();
      }
    }
    else
    {
      // swan mat file should be unpacked for using in within JMatIO-Reader, so we put the uncompressed version in
      // to the working directory.
      ZipUtilities.unzip( new File( m_resultDirSWAN.getURL().toURI() ), new File( m_outputDir.toURI() ) );
      m_resultDirSWAN = m_vfsManager.resolveFile( m_outputDir.toURI().toURL().toExternalForm() );
    }
  }

  private IStatus processTelemacResultFile( final ICalcUnitResultMeta calcUnitMeta, final SubMonitor monitor, final boolean doFullEvaluate ) throws CoreException
  {
    try
    {
      if( m_allDates == null )
      {
        findCalculatedStepsForTelemac();
      }
      final String filename = m_simulationResultFile.getName().getBaseName();

      String resultFileName = FileUtilities.nameWithoutExtension( filename );

      if( m_resultDirSWAN == null )
      {
        final IPath lPath = ResultMeta1d2dHelper.getSavedPathFromResultData( calcUnitMeta, ResultMeta1d2dHelper.SWAN_RAW_DATA_META_NAME );

        try
        {
          m_resultDirSWAN = m_simulationResultFile.getParent().getParent().resolveFile( lPath.toOSString() );
        }
        catch( final Exception e )
        {
          m_geoLog.formatLog( IStatus.INFO, CODE_RUNNING_FINE, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ResultManager.15" ), resultFileName ); //$NON-NLS-1$
        }
      }

      m_geoLog.formatLog( IStatus.INFO, CODE_RUNNING_FINE, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ResultManager.14" ), resultFileName ); //$NON-NLS-1$

      final ProcessResultTelemacOperation processResultsJob = new ProcessResultTelemacOperation( m_simulationResultFile, m_outputDir, m_resultDirSWAN, m_flowModel, m_controlModel, m_discModel, m_parameters, m_datesToProcess, calcUnitMeta, doFullEvaluate );
      final IStatus result = processResultsJob.execute( monitor );
      return result;

    }
    finally
    {
      ProgressUtilities.done( monitor );
    }

  }

  private void processSWANTabFile( final FileObject swanResOutTabFile, final FileObject swanResShiftFile )
  {
    final GM_Position lShiftPosition = SWANDataConverterHelper.readCoordinateShiftValues( swanResShiftFile );
    if( lShiftPosition == null )
    {
      return;
    }
    try
    {
      if( swanResOutTabFile.isContentOpen() )
      {
        swanResOutTabFile.close();
      }
      final FileObject swanResOutTabFileBackUp = swanResOutTabFile.getParent().resolveFile( swanResOutTabFile.getName().getBaseName() + ".bck" ); //$NON-NLS-1$
      swanResOutTabFile.moveTo( swanResOutTabFileBackUp );

      // int lIntLinesCounter = 0;
      final OutputStream lOutStream = swanResOutTabFile.getContent().getOutputStream();
      final DataInputStream lInDataStream = new DataInputStream( swanResOutTabFileBackUp.getContent().getInputStream() );
      BufferedReader streamReader = new BufferedReader( new InputStreamReader( lInDataStream ) );
      final Formatter lFormatter = new Formatter( lOutStream, Charset.defaultCharset().name(), Locale.US );
      while( lInDataStream.available() != 0 )
      {
        final String lStrTmpLine = streamReader.readLine().trim();
        // ++lIntLinesCounter;
        if( lStrTmpLine.startsWith( "%" ) ) { //$NON-NLS-1$
          lFormatter.format( "%s\n", lStrTmpLine ); //$NON-NLS-1$
          continue;
        }
        final StringTokenizer lStrTokenizer = new StringTokenizer( lStrTmpLine, " " ); //$NON-NLS-1$
        int lIntTokenCounter = 0;
        String lStrNewLine = ""; //$NON-NLS-1$
        while( lStrTokenizer.hasMoreTokens() )
        {
          final String lStrToken = lStrTokenizer.nextToken();
          if( lIntTokenCounter == 1 )
          {
            lStrNewLine += String.format( Locale.US, "%.5f\t", NumberUtils.parseQuietDouble( lStrToken ) + lShiftPosition.getX() ); //$NON-NLS-1$
          }
          else if( lIntTokenCounter == 2 )
          {
            lStrNewLine += String.format( Locale.US, "%.5f\t", NumberUtils.parseQuietDouble( lStrToken ) + lShiftPosition.getY() ); //$NON-NLS-1$
          }
          else
          {
            lStrNewLine += lStrToken + "\t"; //$NON-NLS-1$
          }
          lIntTokenCounter++;
        }
        lFormatter.format( "%s\n", lStrNewLine ); //$NON-NLS-1$

      }
      lFormatter.close();
      lInDataStream.close();
      lOutStream.close();
    }
    catch( final Exception e )
    {
      return;
    }

    return;
  }

  private IStatus processResultFile( final FileObject file, final ICalcUnitResultMeta calcUnitResultMeta, final IProgressMonitor monitor, final boolean doFullEvaluate ) throws CoreException
  {
    try
    {
      final String filename = file.getName().getBaseName();

      if( ISimulation1D2DConstants.MODEL_2D.equals( filename ) )
        return Status.OK_STATUS;
      Date stepDate = null;
      String resultFileName = FileUtilities.nameWithoutExtension( filename );

      // check if the given result file is already compressed
      if( filename != null && filename.endsWith( ".2d.zip" ) ) //$NON-NLS-1$
      {
        resultFileName = filename;
        if( file.toString().startsWith( STEADY_PREFIX ) )
          stepDate = STEADY_DATE;
        else if( file.toString().startsWith( MAXI_PREFIX ) )
          stepDate = MAXI_DATE;
        else
          stepDate = ResultMeta1d2dHelper.resolveDateFromResultStep( file );

        if( m_resultDirSWAN == null )
        {
          final IPath lPath = ResultMeta1d2dHelper.getSavedPathFromResultData( calcUnitResultMeta, ResultMeta1d2dHelper.SWAN_RAW_DATA_META_NAME );
          if( lPath != null && lPath.toFile().exists() )
          {
            try
            {
              m_resultDirSWAN = file.getParent().getParent().resolveFile( lPath.toOSString() );
            }
            catch( final Exception e )
            {
              m_geoLog.formatLog( IStatus.INFO, CODE_RUNNING_FINE, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ResultManager.15" ), resultFileName ); //$NON-NLS-1$
            }
          }
        }
      }
      else
        stepDate = findStepDate( file );

      if( stepDate == null )
        return Status.OK_STATUS;

      m_geoLog.formatLog( IStatus.INFO, CODE_RUNNING_FINE, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ResultManager.14" ), resultFileName ); //$NON-NLS-1$

      // start a job for each unknown 2d file.
      final String outDirName = NodeResultHelper.createOutDirName( stepDate );

      final File resultOutputDir = new File( m_outputDir, outDirName );
      resultOutputDir.mkdirs();
      final ProcessResult2DOperation processResultsJob = new ProcessResult2DOperation( file, m_resultDirSWAN, resultOutputDir, m_flowModel, m_controlModel, m_discModel, m_parameters, stepDate, calcUnitResultMeta, doFullEvaluate, m_geoLog );
      final IStatus result = processResultsJob.execute( monitor );

      m_minMaxCatcher.addNodeResultMinMaxCatcher( processResultsJob.getMinMaxData() );

      // TODO: set this status as step result status?
      return result;
    }
    finally
    {
      ProgressUtilities.done( monitor );
    }
  }

  private Date findStepDate( final FileObject file )
  {
    final String filename = file.getName().getBaseName();

    if( filename.startsWith( STEADY_PREFIX ) )
      return STEADY_DATE;

    if( filename.startsWith( MAXI_PREFIX ) )
      return MAXI_DATE;

    if( filename.startsWith( MINI_PREFIX ) || filename.startsWith( MODEL_PREFIX ) )
      return null;

    // check if the given result file is already compressed
    if( filename.endsWith( ".2d.zip" ) ) //$NON-NLS-1$
      return ResultMeta1d2dHelper.resolveDateFromResultStep( file );

    final String name = FilenameUtils.removeExtension( filename );
    final int step = Integer.parseInt( name.substring( 1 ) );

    final IComponent componentTime = ComponentUtilities.findComponentByID( m_timeSteps.getComponents(), Kalypso1D2DDictConstants.DICT_COMPONENT_TIME );
    int indexTime = m_timeSteps.indexOfComponent( componentTime );
    final XMLGregorianCalendar stepCal = (XMLGregorianCalendar)m_timeSteps.get( step ).getValue( indexTime );
    return DateUtilities.toDate( stepCal );
  }

  // FIXME: bad, use result-meta to access all files! This is just pfusch...!
  private FileObject[] find2dFiles( final FileObject remoteWorking ) throws IOException
  {
    final List<FileObject> resultList = new ArrayList<>();
    if( remoteWorking == null )
      return null;

    final FileObject[] children = remoteWorking.getChildren();
    for( final FileObject child : children )
    {
      final FileName childName = child.getName();
      final String baseName = childName.getBaseName();
      if( FilenameUtils.wildcardMatch( baseName, "*.2d" ) || FilenameUtils.wildcardMatch( baseName, "*.2d.zip" ) ) //$NON-NLS-1$ //$NON-NLS-2$
      {
        resultList.add( child );
      }

    }
    return resultList.toArray( new FileObject[resultList.size()] );
  }

  public Date[] findCalculatedSteps( ) throws IOException
  {
    if( m_controlModel.calculateTelemac() )
    {
      return findCalculatedStepsForTelemac();
    }
    final Set<Date> dates = new TreeSet<>();

    final FileObject[] existing2dFiles = find2dFiles( m_resultDir );

    if( existing2dFiles != null )
    {
      for( final FileObject file : existing2dFiles )
      {
        final Date stepDate = findStepDate( file );
        if( stepDate != null )
          dates.add( stepDate );
      }
    }

    return dates.toArray( new Date[dates.size()] );
  }

  private Date[] findCalculatedStepsForTelemac( )
  {
    int size = m_controlModel.getTimeSteps().getResult().size();
    if( size < 2 )
    {
      return new Date[] {};
    }
    m_allDates = Arrays.copyOfRange( NodeResultHelper.getDatesFromObservation( m_controlModel.getTimeSteps() ), 1, size - 1 );
    return m_allDates;
  }

  public IControlModel1D2D getControlModel( )
  {
    return m_controlModel;
  }

  public IFlowRelationshipModel getFlowModel( )
  {
    return m_flowModel;
  }

  public IScenarioResultMeta getScenarioMeta( )
  {
    return m_scenarioMeta;
  }

  public File getOutputDir( )
  {
    return m_outputDir;
  }

  public IGeoLog getGeoLog( )
  {
    return m_geoLog;
  }

  public void setStepsToProcess( final Date[] dates ) throws IOException
  {
    m_datesToProcess = dates;
    final List<FileObject> fileList = new ArrayList<>();
    if( m_mapDateFile == null )
      fillStepMap();

    for( final Date date : dates )
    {
      final FileObject stepFile = m_mapDateFile.get( date );
      if( stepFile != null )
        fileList.add( stepFile );
    }

    m_stepsToProcess = fileList.toArray( new FileObject[fileList.size()] );
  }

  private void fillStepMap( ) throws IOException
  {
    m_mapDateFile = new HashMap<>();
    if( m_controlModel.calculateTelemac() && m_resultDir != null )
    {
      m_simulationResultFile = findTelemacResultFile( m_resultDir );
      for( int i = 0; i < m_controlModel.getTimeSteps().getResult().size(); ++i )
      {
        final Date fileDate = getDateOfTimeStepNr( i );
        if( fileDate != null )
          m_mapDateFile.put( fileDate, m_simulationResultFile );
      }
    }
    else
    {
      final FileObject[] existing2dFiles = find2dFiles( m_resultDir );

      for( final FileObject file : existing2dFiles )
      {
        final Date fileDate = getStepMapDate( file );
        if( fileDate != null )
          m_mapDateFile.put( fileDate, file );
      }
    }
  }

  private FileObject findTelemacResultFile( final FileObject resultDir )
  {
    if( resultDir == null )
      return null;

    FileObject[] children = null;
    try
    {
      children = resultDir.getChildren();
    }
    catch( FileSystemException e )
    {
      e.printStackTrace();
    }
    for( final FileObject child : children )
    {
      final FileName childName = child.getName();
      final String baseName = childName.getBaseName();
      if( FilenameUtils.wildcardMatch( baseName, "*res*.slf*" ) ) //$NON-NLS-1$ 
      {
        return child;
      }
    }

    final IPath path = ResultMeta1d2dHelper.getSavedPathFromResultData( m_calcUnitMeta, ResultMeta1d2dHelper.TELEMAC_RAW_DATA_META_NAME );
    if( path != null )
    {
      try
      {
        return resultDir.getParent().resolveFile( path.toOSString() );
      }
      catch( final Exception e )
      {
        m_geoLog.formatLog( IStatus.INFO, CODE_RUNNING_FINE, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ResultManager.15" ), resultDir.getName().getBaseName() ); //$NON-NLS-1$
        return null;
      }
    }
    return null;
  }

  private Date getStepMapDate( final FileObject file )
  {
    final String baseName = file.getName().getBaseName();
    if( baseName.equals( "steady.2d" ) ) //$NON-NLS-1$
      return STEADY_DATE;

    if( baseName.equals( "maxi.2d" ) ) //$NON-NLS-1$
      return MAXI_DATE;

    if( baseName.equals( "steady.2d" ) || baseName.equals( "maxi.2d" ) || baseName.equals( "mini.2d" ) || baseName.equals( "model.2d" ) ) //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
      return null;

    if( baseName.endsWith( ".2d.zip" ) ) //$NON-NLS-1$
      return ResultMeta1d2dHelper.resolveDateFromResultStep( file );

    final String resultFileName = baseName;
    final int index = resultFileName.indexOf( "." ); //$NON-NLS-1$
    final CharSequence sequence = resultFileName.subSequence( 1, index );
    final String string = sequence.toString();
    final int step = Integer.parseInt( string );

    return getDateOfTimeStepNr( step );

  }

  private Date getDateOfTimeStepNr( int step )
  {
    final IObservation<TupleResult> obs = m_controlModel.getTimeSteps();
    final TupleResult timeSteps = obs.getResult();

    final IComponent componentTime = ComponentUtilities.findComponentByID( timeSteps.getComponents(), Kalypso1D2DDictConstants.DICT_COMPONENT_TIME );
    int indexTime = timeSteps.indexOfComponent( componentTime );
    final XMLGregorianCalendar stepCal = (XMLGregorianCalendar)timeSteps.get( step ).getValue( indexTime );
    return DateUtilities.toDate( stepCal );
  }

  public final Map<Date, FileObject> getDateFileMap( )
  {
    return m_mapDateFile;
  }

  public ICalcUnitResultMeta getCalcUnitMeta( )
  {
    return m_calcUnitMeta;
  }

  public static String getFolderForDate( final Date stepDate )
  {
    final String stepResultFolder;
    if( stepDate.equals( ISimulation1D2DConstants.STEADY_DATE ) )
      stepResultFolder = ISimulation1D2DConstants.STEADY_PREFIX;
    else if( stepDate.equals( ISimulation1D2DConstants.MAXI_DATE ) )
      stepResultFolder = ISimulation1D2DConstants.MAXI_PREFIX;
    else
    {
      final SimpleDateFormat timeFormatter = new SimpleDateFormat( ResultMeta1d2dHelper.FULL_DATE_TIME_FORMAT_RESULT_STEP );
      stepResultFolder = ResultMeta1d2dHelper.TIME_STEP_PREFIX + timeFormatter.format( stepDate );
    }
    return stepResultFolder;
  }

}
