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
import org.apache.commons.vfs.FileName;
import org.apache.commons.vfs.FileObject;
import org.apache.commons.vfs.impl.StandardFileSystemManager;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.MultiStatus;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubMonitor;
import org.kalypso.afgui.model.IModel;
import org.kalypso.commons.io.VFSUtilities;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.commons.java.util.zip.ZipUtilities;
import org.kalypso.contribs.eclipse.core.runtime.PluginUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
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

import de.renew.workflow.connector.cases.ICaseDataProvider;

/**
 * This runnable will be called while running the 2d-exe and will check for new .2d result files.<br>
 * Every new 2d result file we be processed in order to return it to the kalypso client.
 * 
 * @author Gernot Belger
 */
public class ResultManager implements ISimulation1D2DConstants
{
  private final NodeResultMinMaxCatcher m_minMaxCatcher = new NodeResultMinMaxCatcher();

  // never read
  // private final Pattern m_resultFilePattern = Pattern.compile( "A(\\d+)" );

  /* just for test purposes TODO: still? */
  private final List<ResultType.TYPE> m_parameters = new ArrayList<ResultType.TYPE>();

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

  public ResultManager( final FileObject fileObjectRMA, final FileObject fileObjectSWAN, final ICaseDataProvider<IModel> caseDataProvider, final IGeoLog geoLog ) throws CoreException
  {
    this( fileObjectRMA, fileObjectSWAN, caseDataProvider.getModel( IFEDiscretisationModel1d2d.class.getName(), IFEDiscretisationModel1d2d.class ), caseDataProvider.getModel( IControlModelGroup.class.getName(), IControlModelGroup.class ).getModel1D2DCollection().getActiveControlModel(), caseDataProvider.getModel( IFlowRelationshipModel.class.getName(), IFlowRelationshipModel.class ), caseDataProvider.getModel( IScenarioResultMeta.class.getName(), IScenarioResultMeta.class ), geoLog );
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

    m_parameters.add( ResultType.TYPE.DEPTH );
    m_parameters.add( ResultType.TYPE.WATERLEVEL );
    m_parameters.add( ResultType.TYPE.VELOCITY );
    m_parameters.add( ResultType.TYPE.TERRAIN );
    
    final IObservation<TupleResult> obs = controlModel.getTimeSteps();
    m_timeSteps = obs.getResult();
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
      if( m_resultDirSWAN != null )
      {
        if( !m_resultDirSWAN.getName().getBaseName().endsWith( "zip" ) ) //$NON-NLS-1$
        {
          try
          {
            FileObject swanResFile = m_resultDirSWAN.getChild( ISimulation1D2DConstants.SIM_SWAN_TRIANGLE_FILE + "." + ISimulation1D2DConstants.SIM_SWAN_MAT_RESULT_EXT ); //$NON-NLS-1$
            FileObject swanResShiftFile = m_resultDirSWAN.getChild( ISimulation1D2DConstants.SIM_SWAN_COORD_SHIFT_FILE );
            FileObject swanResOutTabFile = m_resultDirSWAN.getChild( ISimulation1D2DConstants.SIM_SWAN_TRIANGLE_FILE + "_out.tab" ); //$NON-NLS-1$
            processSWANTabFile( swanResOutTabFile, swanResShiftFile );
            File zipOutput = new File( m_outputDir, ISimulation1D2DConstants.SIM_SWAN_TRIANGLE_FILE + ".zip" ); //$NON-NLS-1$
            List<File> lListFilesToZip = new ArrayList<File>();
            lListFilesToZip.add( new File( swanResFile.getURL().toURI() ) );
            lListFilesToZip.add( new File( swanResShiftFile.getURL().toURI() ) );
            lListFilesToZip.add( new File( swanResOutTabFile.getURL().toURI() ) );
            if( m_controlModel.getINITialValuesSWAN() == 3 )
            {
              FileObject swanResHotFile = m_resultDirSWAN.getChild( ISimulation1D2DConstants.SIM_SWAN_HOT_FILE );
              lListFilesToZip.add( new File( swanResHotFile.getURL().toURI() ) );
            }
            ZipUtilities.zip( zipOutput, lListFilesToZip.toArray( new File[lListFilesToZip.size()] ), new File( m_resultDirSWAN.getURL().toURI() ) );
          }
          catch( Exception e )
          {
            e.printStackTrace();
          }
        }
        else
        {
          // swan mat file should be unpacked for using in within JMatIO-Reader, so we put the uncompressed version in
          // to the working directory.
          ZipUtilities.unzip( new File( m_resultDirSWAN.getURL().toURI() ), new File( m_outputDir.toURI() ) );
          m_resultDirSWAN = VFSUtilities.getNewManager().resolveFile( m_outputDir.toURI().toURL().toExternalForm() );
        }
      }

      if( m_stepsToProcess == null )
        return StatusUtilities.createOkStatus( Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ResultManager.5" ) ); //$NON-NLS-1$

      /* Process all remaining .2d files. Now including min and max files */
      final FileObject[] existing2dFiles = m_stepsToProcess;

      progress.setWorkRemaining( existing2dFiles.length );
      // final File[] existing2dFiles = m_inputDir.listFiles( FILTER_2D );
      // progress.setWorkRemaining( existing2dFiles.length );

      final IStatus[] fileStati = new IStatus[existing2dFiles.length];

      for( int i = 0; i < fileStati.length; i++ )
      {
        final FileObject file = existing2dFiles[i];
        fileStati[i] = processResultFile( file, m_resultDirSWAN, m_controlModel, m_flowModel, m_discModel, calcUnitMeta, progress.newChild( 1 ), doFullEvaluate );
      }

      final MultiStatus multiStatus = new MultiStatus( PluginUtilities.id( KalypsoModel1D2DPlugin.getDefault() ), CODE_POST, fileStati, "", null ); //$NON-NLS-1$
      if( multiStatus.isOK() )
        return StatusUtilities.createStatus( IStatus.OK, CODE_POST, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ResultManager.7" ), null ); //$NON-NLS-1$

      if( multiStatus.matches( IStatus.CANCEL ) )
        return StatusUtilities.createStatus( IStatus.WARNING, CODE_POST, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ResultManager.8" ), null ); //$NON-NLS-1$

      if( multiStatus.matches( IStatus.WARNING ) )
        return new MultiStatus( PluginUtilities.id( KalypsoModel1D2DPlugin.getDefault() ), CODE_POST, fileStati, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ResultManager.9" ), null ); //$NON-NLS-1$

      if( multiStatus.matches( IStatus.ERROR ) )
        return new MultiStatus( PluginUtilities.id( KalypsoModel1D2DPlugin.getDefault() ), CODE_POST, fileStati, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ResultManager.10" ), null ); //$NON-NLS-1$

      return new MultiStatus( PluginUtilities.id( KalypsoModel1D2DPlugin.getDefault() ), CODE_POST, fileStati, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ResultManager.11" ), null ); //$NON-NLS-1$
    }
    catch( final OperationCanceledException e )
    {
      return StatusUtilities.createStatus( IStatus.CANCEL, CODE_POST, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ResultManager.12" ), e ); //$NON-NLS-1$
    }
    catch( final CoreException e )
    {
      return e.getStatus();
    }
    catch( final Throwable e )
    {
      return StatusUtilities.createStatus( IStatus.ERROR, CODE_POST, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ResultManager.13" ), e ); //$NON-NLS-1$
    }
    finally
    {

      if( m_resultDirRMA != null )
      {
        try
        {
          m_resultDirRMA.close();
          final StandardFileSystemManager fileSystemManager = (StandardFileSystemManager) m_resultDirRMA.getFileSystem().getFileSystemManager();
          fileSystemManager.close();
        }
        catch( final IOException e )
        {
          // gobble
        }
      }
    }
  }

  private void processSWANTabFile( FileObject swanResOutTabFile, FileObject swanResShiftFile )
  {
    GM_Position lShiftPosition = SWANDataConverterHelper.readCoordinateShiftValues( swanResShiftFile );
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
      FileObject swanResOutTabFileBackUp = swanResOutTabFile.getParent().resolveFile( swanResOutTabFile.getName().getBaseName() + ".bck" ); //$NON-NLS-1$
      swanResOutTabFile.moveTo( swanResOutTabFileBackUp );
      
      int lIntLinesCounter = 0;
      OutputStream lOutStream = swanResOutTabFile.getContent().getOutputStream();
      DataInputStream lInDataStream = new DataInputStream( swanResOutTabFileBackUp.getContent().getInputStream() );

      Formatter lFormatter = new Formatter( lOutStream, Charset.defaultCharset().name(), Locale.US );
      while( lInDataStream.available() != 0 )
      {
        String lStrTmpLine = lInDataStream.readLine().trim();
        ++lIntLinesCounter;
        if( lStrTmpLine.startsWith( "%" ) ) { //$NON-NLS-1$
          lFormatter.format( "%s\n", lStrTmpLine ); //$NON-NLS-1$ 
          continue;
        }
        StringTokenizer lStrTokenizer = new StringTokenizer( lStrTmpLine, " " ); //$NON-NLS-1$
        int lIntTokenCounter = 0;
        String lStrNewLine = ""; //$NON-NLS-1$
        while( lStrTokenizer.hasMoreTokens() )
        {
          String lStrToken = lStrTokenizer.nextToken();
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
    catch( Exception e )
    {
      return;
    }

    return;
  }

  private IStatus processResultFile( final FileObject file, final FileObject fileResSWAN, final IControlModel1D2D controlModel, final IFlowRelationshipModel flowModel, final IFEDiscretisationModel1d2d discModel, final ICalcUnitResultMeta calcUnitResultMeta, final IProgressMonitor monitor, final boolean doFullEvaluate ) throws CoreException
  {
    try
    {
      FileObject lFileObjectSWANResult = fileResSWAN;
      final String filename = file.getName().getBaseName();

      if( ISimulation1D2DConstants.MODEL_2D.equals( filename ) )
        return Status.OK_STATUS;
      Date stepDate = null;
      String resultFileName = FileUtilities.nameWithoutExtension( filename );

      // check if the given result file is already compressed
      if( filename != null && filename.endsWith( ".2d.zip" ) ) //$NON-NLS-1$
      {
        resultFileName = filename;
        if( file.toString().contains( "steady" ) ){ //$NON-NLS-1$
          stepDate = STEADY_DATE;
        }
        else{
          stepDate = ResultMeta1d2dHelper.resolveDateFromResultStep( file );
        }
        if( lFileObjectSWANResult == null )
        {
          IPath lPath = ResultMeta1d2dHelper.getSavedSWANRawResultData( calcUnitResultMeta );

          try
          {
            lFileObjectSWANResult = file.getParent().getParent().resolveFile( lPath.toOSString() );
          }
          catch( Exception e )
          {
            m_geoLog.formatLog( IStatus.INFO, CODE_RUNNING_FINE, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ResultManager.15" ), resultFileName ); //$NON-NLS-1$

          }
        }
      }
      else
      {
        stepDate = findStepDate( controlModel, resultFileName );
      }

      if( stepDate == null )
        return Status.OK_STATUS;

      m_geoLog.formatLog( IStatus.INFO, CODE_RUNNING_FINE, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ResultManager.14" ), resultFileName ); //$NON-NLS-1$

      // start a job for each unknown 2d file.
      final String outDirName;

      if( stepDate == STEADY_DATE )
        outDirName = "steady"; //$NON-NLS-1$
      else if( stepDate == MAXI_DATE )
        outDirName = "maxi"; //$NON-NLS-1$
      else{
        SimpleDateFormat timeFormatter = new SimpleDateFormat( ResultMeta1d2dHelper.FULL_DATE_TIME_FORMAT_RESULT_STEP );
        outDirName = ResultMeta1d2dHelper.TIME_STEP_PREFIX + timeFormatter.format( stepDate );
//        outDirName = String.format( ResultMeta1d2dHelper.TIME_STEP_PREFIX + "%1$te.%1$tm.%1$tY_%1$tH_%1$tM_%1$tS_%1$ts_%1$tZ", stepDate ); //$NON-NLS-1$
      }
      final File resultOutputDir = new File( m_outputDir, outDirName );
      resultOutputDir.mkdirs();
      final ProcessResultsJob processResultsJob = new ProcessResultsJob( file, lFileObjectSWANResult, resultOutputDir, flowModel, controlModel, discModel, m_parameters, stepDate, calcUnitResultMeta, doFullEvaluate );
      final IStatus result = processResultsJob.run( monitor );

      m_minMaxCatcher.addNodeResultMinMaxCatcher( processResultsJob.getMinMaxData() );

      // TODO: set this status as step result status?

      // only process terrain once for all steps
      m_parameters.remove( ResultType.TYPE.TERRAIN );

      return result;
    }
    finally
    {
      ProgressUtilities.done( monitor );
    }
  }

  private Date findStepDate( final IControlModel1D2D controlModel, final String resultFileName )
  {
    if( resultFileName.startsWith( "steady" ) ) //$NON-NLS-1$
      return STEADY_DATE;

    if( resultFileName.startsWith( "maxi" ) ) //$NON-NLS-1$
      return MAXI_DATE;

    if( resultFileName.startsWith( "mini" ) || resultFileName.startsWith( "model" ) ) //$NON-NLS-1$ //$NON-NLS-2$
      return null;

    final int index = resultFileName.length();
    final CharSequence sequence = resultFileName.subSequence( 1, index );
    final String string = sequence.toString();

    final int step = Integer.parseInt( string );

    final IComponent componentTime = ComponentUtilities.findComponentByID( m_timeSteps.getComponents(), Kalypso1D2DDictConstants.DICT_COMPONENT_TIME );
    final XMLGregorianCalendar stepCal = (XMLGregorianCalendar) m_timeSteps.get( step ).getValue( componentTime );
    return DateUtilities.toDate( stepCal );
  }

  private FileObject[] find2dFiles( final FileObject remoteWorking ) throws IOException
  {
    final List<FileObject> resultList = new ArrayList<FileObject>();
    final FileObject[] children = remoteWorking.getChildren();
    for( final FileObject child : children )
    {
      final FileName childName = child.getName();
      final String baseName = childName.getBaseName();
      if( FilenameUtils.wildcardMatch( baseName, "*.2d" ) || FilenameUtils.wildcardMatch( baseName, "*.2d.zip" ) ) //$NON-NLS-1$
      {
        resultList.add( child );
      }

    }
    return resultList.toArray( new FileObject[resultList.size()] );
  }

  public Date[] findCalculatedSteps( ) throws IOException
  {
    final Set<Date> dates = new TreeSet<Date>();

    final FileObject[] existing2dFiles = find2dFiles( m_resultDirRMA );

    if( existing2dFiles != null )
    {
      for( final FileObject file : existing2dFiles )
      {
        final String baseName = file.getName().getBaseName();
        final String resultFileName = FileUtilities.nameWithoutExtension( baseName );
        final Date stepDate = findStepDate( m_controlModel, resultFileName );
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
    final List<FileObject> fileList = new ArrayList<FileObject>();
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

  public FileObject[] getStepsToProcess( )
  {
    return m_stepsToProcess; 
  }

  private void fillStepMap( final IControlModel1D2D controlModel ) throws IOException
  {
    m_mapDateFile = new HashMap<Date, FileObject>();
    Date fileDate = null;
    final FileObject[] existing2dFiles = find2dFiles( m_resultDirRMA );

    for( final FileObject file : existing2dFiles )
    {
      final String baseName = file.getName().getBaseName();
      if( baseName.equals( "steady.2d" ) ) { //$NON-NLS-1$
        m_mapDateFile.put( STEADY_DATE, file );
      }
      if( baseName.equals( "maxi.2d" ) ) { //$NON-NLS-1$
        m_mapDateFile.put( MAXI_DATE, file );
      }
      if( baseName.equals( "steady.2d" ) || baseName.equals( "maxi.2d" ) || baseName.equals( "mini.2d" ) || baseName.equals( "model.2d" ) ) { //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
        continue;
      }
      if( baseName.endsWith( ".2d.zip" ) ) //$NON-NLS-1$
      {
        fileDate = ResultMeta1d2dHelper.resolveDateFromResultStep( file );
      }
      else
      {
        final String resultFileName = baseName;
        final int index = resultFileName.indexOf( "." ); //$NON-NLS-1$
        final CharSequence sequence = resultFileName.subSequence( 1, index );
        final String string = sequence.toString();
        final int step = Integer.parseInt( string );

        final IObservation<TupleResult> obs = controlModel.getTimeSteps();
        final TupleResult timeSteps = obs.getResult();

        final IComponent componentTime = ComponentUtilities.findComponentByID( timeSteps.getComponents(), Kalypso1D2DDictConstants.DICT_COMPONENT_TIME );
        final XMLGregorianCalendar stepCal = (XMLGregorianCalendar) timeSteps.get( step ).getValue( componentTime );
        fileDate = DateUtilities.toDate( stepCal );
      }
      m_mapDateFile.put( fileDate, file );
    }
  }

  public final Map<Date, FileObject> getDateFileMap( )
  {
    return m_mapDateFile;
  }

}
