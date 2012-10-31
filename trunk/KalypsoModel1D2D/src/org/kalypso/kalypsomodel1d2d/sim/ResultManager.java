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

import java.io.DataInputStream;
import java.io.File;
import java.io.IOException;
import java.io.OutputStream;
import java.nio.charset.Charset;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.Formatter;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.StringTokenizer;
import java.util.TreeSet;

import javax.xml.datatype.XMLGregorianCalendar;

import org.apache.commons.io.FilenameUtils;
import org.apache.commons.vfs2.FileName;
import org.apache.commons.vfs2.FileObject;
import org.apache.commons.vfs2.impl.StandardFileSystemManager;
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
import org.kalypso.contribs.eclipse.core.runtime.PluginUtilities;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.contribs.java.lang.NumberUtils;
import org.kalypso.contribs.java.util.DateUtilities;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.conv.SWANDataConverterHelper;
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
  private final NodeResultMinMaxCatcher m_minMaxCatcher = new NodeResultMinMaxCatcher();

  private final List<ResultType> m_parameters = new ArrayList<>();

  private final IGeoLog m_geoLog;

  private final IControlModel1D2D m_controlModel;

  private final IFlowRelationshipModel m_flowModel;

  private final IScenarioResultMeta m_scenarioMeta;

  private final IFEDiscretisationModel1d2d m_discModel;

  private FileObject[] m_stepsToProcess;

  private File m_outputDir;

  private final FileObject m_resultDirRMA;

  private FileObject m_resultDirSWAN;

  private Map<Date, FileObject> m_mapDateFile;

  private final TupleResult m_timeSteps;

  private ICalcUnitResultMeta m_calcUnitMeta = null;

  public ResultManager( final FileObject fileObjectRMA, final FileObject fileObjectSWAN, final IScenarioDataProvider caseDataProvider, final IGeoLog geoLog ) throws CoreException
  {
    // FIXME: avoid throwing exception in constructor; clients should provide the models, not the caseDataProvider
    this( fileObjectRMA, fileObjectSWAN, (IFEDiscretisationModel1d2d)caseDataProvider.getModel( IFEDiscretisationModel1d2d.class.getName() ), ((IControlModelGroup)caseDataProvider.getModel( IControlModelGroup.class.getName() )).getModel1D2DCollection().getActiveControlModel(), (IFlowRelationshipModel)caseDataProvider.getModel( IFlowRelationshipModel.class.getName() ), (IScenarioResultMeta)caseDataProvider.getModel( IScenarioResultMeta.class.getName() ), geoLog );
  }

  public ResultManager( final FileObject fileObjectRMA, final FileObject fileObjectSWAN, final IFEDiscretisationModel1d2d discretisationModel1d2d, final IControlModel1D2D controlModel, final IFlowRelationshipModel flowRelModel, final IScenarioResultMeta scenarioMetaData, final IGeoLog geoLog )
  {
    m_resultDirRMA = fileObjectRMA;
    m_resultDirSWAN = fileObjectSWAN;

    m_controlModel = controlModel;
    m_flowModel = flowRelModel;
    m_discModel = discretisationModel1d2d;
    m_scenarioMeta = scenarioMetaData;

    m_geoLog = geoLog;

    m_parameters.add( ResultType.DEPTH );
    m_parameters.add( ResultType.WATERLEVEL );
    m_parameters.add( ResultType.VELOCITY );
    m_parameters.add( ResultType.TERRAIN );

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
      m_outputDir = SimulationUtilitites.createSimulationTmpDir( "output" + calcUnitMeta.getCalcUnit() ); //$NON-NLS-1$

      /*
       * compress and copy swan result file into temp directory, the whole folder will be moved into the project after
       * successful finishing the complete result evaluation.
       */
      // FIXME: move this into s separate class
      if( m_resultDirSWAN != null )
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
          // FIXME: resource leak, manager never closed!
          m_resultDirSWAN = VFSUtilities.getNewManager().resolveFile( m_outputDir.toURI().toURL().toExternalForm() );
        }
      }

      if( m_stepsToProcess == null )
        return new Status( IStatus.OK, KalypsoModel1D2DPlugin.PLUGIN_ID, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ResultManager.5" ) ); //$NON-NLS-1$

      /* Process all remaining .2d files. Now including min and max files */
      final FileObject[] existing2dFiles = m_stepsToProcess;

      progress.setWorkRemaining( existing2dFiles.length );

      final IStatus[] fileStati = new IStatus[existing2dFiles.length];

      for( int i = 0; i < fileStati.length; i++ )
      {
        final FileObject file = existing2dFiles[i];
        fileStati[i] = processResultFile( file, m_resultDirSWAN, m_controlModel, m_flowModel, m_discModel, calcUnitMeta, progress.newChild( 1 ), doFullEvaluate );
      }

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
      if( m_resultDirRMA != null )
      {
        try
        {
          m_resultDirRMA.close();

          // FIXME: dubios, closing the global available file system... TODO: check...
          // ... and why is the swan dir not also closed... ??
          final StandardFileSystemManager fileSystemManager = (StandardFileSystemManager)m_resultDirRMA.getFileSystem().getFileSystemManager();
          fileSystemManager.close();
        }
        catch( final IOException e )
        {
          // gobble
        }
      }
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

      final Formatter lFormatter = new Formatter( lOutStream, Charset.defaultCharset().name(), Locale.US );
      while( lInDataStream.available() != 0 )
      {
        final String lStrTmpLine = lInDataStream.readLine().trim();
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

  private IStatus processResultFile( final FileObject file, final FileObject fileResSWAN, final IControlModel1D2D controlModel, final IFlowRelationshipModel flowModel, final IFEDiscretisationModel1d2d discModel, final ICalcUnitResultMeta calcUnitResultMeta, final IProgressMonitor monitor, final boolean doFullEvaluate ) throws CoreException
  {
    try
    {
      final String filename = file.getName().getBaseName();

      FileObject lFileObjectSWANResult = fileResSWAN;

      if( ISimulation1D2DConstants.MODEL_2D.equals( filename ) )
        return Status.OK_STATUS;

      Date stepDate = null;
      String resultFileName = FileUtilities.nameWithoutExtension( filename );

      // check if the given result file is already compressed
      if( filename != null && filename.endsWith( ".2d.zip" ) ) //$NON-NLS-1$
      {
        resultFileName = filename;
        if( file.toString().startsWith( STEADY_PREFIX ) ) //$NON-NLS-1$
          stepDate = STEADY_DATE;
        else if( file.toString().startsWith( MAXI_PREFIX ) ) //$NON-NLS-1$
          stepDate = MAXI_DATE;
        else
          stepDate = ResultMeta1d2dHelper.resolveDateFromResultStep( file );

        if( lFileObjectSWANResult == null )
        {
          final IPath lPath = ResultMeta1d2dHelper.getSavedSWANRawResultData( calcUnitResultMeta );

          try
          {
            lFileObjectSWANResult = file.getParent().getParent().resolveFile( lPath.toOSString() );
          }
          catch( final Exception e )
          {
            m_geoLog.formatLog( IStatus.INFO, CODE_RUNNING_FINE, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ResultManager.15" ), resultFileName ); //$NON-NLS-1$
          }
        }
      }
      else
        stepDate = findStepDate( file );

      if( stepDate == null )
        return Status.OK_STATUS;

      m_geoLog.formatLog( IStatus.INFO, CODE_RUNNING_FINE, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ResultManager.14" ), resultFileName ); //$NON-NLS-1$

      // start a job for each unknown 2d file.
      final String outDirName = createOutDirName( stepDate );

      final File resultOutputDir = new File( m_outputDir, outDirName );
      resultOutputDir.mkdirs();
      final ProcessResult2DOperation processResultsJob = new ProcessResult2DOperation( file, lFileObjectSWANResult, resultOutputDir, flowModel, controlModel, discModel, m_parameters, stepDate, calcUnitResultMeta, doFullEvaluate );
      final IStatus result = processResultsJob.execute( monitor );

      m_minMaxCatcher.addNodeResultMinMaxCatcher( processResultsJob.getMinMaxData() );

      // TODO: set this status as step result status?

      // only process terrain once for all steps
      m_parameters.remove( ResultType.TERRAIN );

      return result;
    }
    finally
    {
      ProgressUtilities.done( monitor );
    }
  }

  public static String createOutDirName( final Date stepDate )
  {
    if( stepDate == STEADY_DATE )
      return STEADY_PREFIX;

    if( stepDate == MAXI_DATE )
      return MAXI_PREFIX;

    final SimpleDateFormat timeFormatter = new SimpleDateFormat( ResultMeta1d2dHelper.FULL_DATE_TIME_FORMAT_RESULT_STEP );
    return ResultMeta1d2dHelper.TIME_STEP_PREFIX + timeFormatter.format( stepDate );
  }

  private FileObject findSwanResultFile( final FileObject result2DFile, final FileObject resSWANfile, final ICalcUnitResultMeta calcUnitResultMeta )
  {
    if( resSWANfile != null )
      return resSWANfile;

    final IPath lPath = ResultMeta1d2dHelper.getSavedSWANRawResultData( calcUnitResultMeta );

    try
    {
      return result2DFile.getParent().getParent().resolveFile( lPath.toOSString() );
    }
    catch( final Exception e )
    {
      m_geoLog.formatLog( IStatus.INFO, CODE_RUNNING_FINE, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ResultManager.15" ), result2DFile.getName().getBaseName() ); //$NON-NLS-1$
      return null;
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
    final XMLGregorianCalendar stepCal = (XMLGregorianCalendar)m_timeSteps.get( step ).getValue( componentTime );
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
    final Set<Date> dates = new TreeSet<>();

    final FileObject[] existing2dFiles = find2dFiles( m_resultDirRMA );

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

  public void setStepsToProcess( final Date[] dates, final IControlModel1D2D controlModel ) throws IOException
  {
    final List<FileObject> fileList = new ArrayList<>();
    if( m_mapDateFile == null )
      fillStepMap( controlModel );

    for( final Date date : dates )
    {
      final FileObject stepFile = m_mapDateFile.get( date );
      if( stepFile != null )
        fileList.add( stepFile );
    }

    m_stepsToProcess = fileList.toArray( new FileObject[fileList.size()] );
  }

  private void fillStepMap( final IControlModel1D2D controlModel ) throws IOException
  {
    m_mapDateFile = new HashMap<>();
    final FileObject[] existing2dFiles = find2dFiles( m_resultDirRMA );

    for( final FileObject file : existing2dFiles )
    {
      final Date fileDate = getStepMapDate( file, controlModel );
      if( fileDate != null )
        m_mapDateFile.put( fileDate, file );
    }
  }

  private Date getStepMapDate( final FileObject file, final IControlModel1D2D controlModel )
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

    final IObservation<TupleResult> obs = controlModel.getTimeSteps();
    final TupleResult timeSteps = obs.getResult();

    final IComponent componentTime = ComponentUtilities.findComponentByID( timeSteps.getComponents(), Kalypso1D2DDictConstants.DICT_COMPONENT_TIME );
    final XMLGregorianCalendar stepCal = (XMLGregorianCalendar)timeSteps.get( step ).getValue( componentTime );
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
