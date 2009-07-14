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
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

import javax.xml.datatype.XMLGregorianCalendar;

import org.apache.commons.io.FilenameUtils;
import org.apache.commons.vfs.FileName;
import org.apache.commons.vfs.FileObject;
import org.apache.commons.vfs.impl.StandardFileSystemManager;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.MultiStatus;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubMonitor;
import org.kalypso.afgui.model.IModel;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.contribs.eclipse.core.runtime.PluginUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.contribs.java.util.DateUtilities;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
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

  private final FileObject m_resultDir;

  private Map<Date, FileObject> m_dateFileMap;

  public ResultManager( final FileObject fileObject, final ICaseDataProvider<IModel> caseDataProvider, final IGeoLog geoLog ) throws CoreException
  {
    m_resultDir = fileObject;

    final IControlModelGroup controlModelGroup = caseDataProvider.getModel( IControlModelGroup.class );
    m_controlModel = controlModelGroup.getModel1D2DCollection().getActiveControlModel();
    m_flowModel = caseDataProvider.getModel( IFlowRelationshipModel.class );
    m_discModel = caseDataProvider.getModel( IFEDiscretisationModel1d2d.class );
    m_scenarioMeta = caseDataProvider.getModel( IScenarioResultMeta.class );

    m_geoLog = geoLog;

    m_parameters.add( ResultType.TYPE.DEPTH );
    m_parameters.add( ResultType.TYPE.WATERLEVEL );
    m_parameters.add( ResultType.TYPE.VELOCITY );
    m_parameters.add( ResultType.TYPE.TERRAIN );
  }

  public IStatus processResults( final ICalcUnitResultMeta calcUnitMeta, final IProgressMonitor monitor )
  {
    final SubMonitor progress = SubMonitor.convert( monitor, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ResultManager.3" ), 1000 ); //$NON-NLS-1$
    progress.subTask( Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ResultManager.4" ) ); //$NON-NLS-1$

    try
    {
      m_outputDir = SimulationUtilitites.createSimulationTmpDir( "output" + calcUnitMeta.getCalcUnit() ); //$NON-NLS-1$

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
        fileStati[i] = processResultFile( file, m_controlModel, m_flowModel, m_discModel, calcUnitMeta, progress.newChild( 1 ) );
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

      if( m_resultDir != null )
      {
        try
        {
          m_resultDir.close();
          final StandardFileSystemManager fileSystemManager = (StandardFileSystemManager) m_resultDir.getFileSystem().getFileSystemManager();
          fileSystemManager.close();
        }
        catch( final IOException e )
        {
          // gobble
        }
      }
    }
  }

  private IStatus processResultFile( final FileObject file, final IControlModel1D2D controlModel, final IFlowRelationshipModel flowModel, final IFEDiscretisationModel1d2d discModel, final ICalcUnitResultMeta calcUnitResultMeta, final IProgressMonitor monitor ) throws CoreException
  {
    try
    {
      final String filename = file.getName().getBaseName();

      if( ISimulation1D2DConstants.MODEL_2D.equals( filename ) )
        return Status.OK_STATUS;

      final String resultFileName = FileUtilities.nameWithoutExtension( filename );

      final Date stepDate = findStepDate( controlModel, resultFileName );
      if( stepDate == null )
        return Status.OK_STATUS;

      m_geoLog.formatLog( IStatus.INFO, CODE_RUNNING_FINE, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ResultManager.14" ), resultFileName ); //$NON-NLS-1$

      // start a job for each unknown 2d file.
      final String outDirName;

      if( stepDate == STEADY_DATE )
        outDirName = "steady"; //$NON-NLS-1$
      else if( stepDate == MAXI_DATE )
        outDirName = "maxi"; //$NON-NLS-1$
      else
        outDirName = String.format( "timestep-%1$te.%1$tm.%1$tY_%1$tH_%1$tM_%1$tZ", stepDate ); //$NON-NLS-1$

      final File resultOutputDir = new File( m_outputDir, outDirName );
      resultOutputDir.mkdirs();
      final ProcessResultsJob processResultsJob = new ProcessResultsJob( file, resultOutputDir, flowModel, controlModel, discModel, m_parameters, stepDate, calcUnitResultMeta );
      final IStatus result = processResultsJob.run( monitor );

      m_minMaxCatcher.addNodeResultMinMaxCatcher( processResultsJob.getMinMaxData() );

      // TODO: set this status as step result status?

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

    final IObservation<TupleResult> obs = controlModel.getTimeSteps();
    final TupleResult timeSteps = obs.getResult();

    final IComponent componentTime = ComponentUtilities.findComponentByID( timeSteps.getComponents(), Kalypso1D2DDictConstants.DICT_COMPONENT_TIME );
    final XMLGregorianCalendar stepCal = (XMLGregorianCalendar) timeSteps.get( step ).getValue( componentTime );
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
      if( FilenameUtils.wildcardMatch( baseName, "*.2d" ) ) //$NON-NLS-1$
      {
        resultList.add( child );
      }

    }
    return resultList.toArray( new FileObject[resultList.size()] );
  }

  public Date[] findCalculatedSteps( ) throws IOException
  {
    final Set<Date> dates = new TreeSet<Date>();

    final FileObject[] existing2dFiles = find2dFiles(m_resultDir);

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
    if( m_dateFileMap == null )
      fillStepMap( controlModel );

    for( final Date date : dates )
    {
      final FileObject stepFile = m_dateFileMap.get( date );
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
    m_dateFileMap = new HashMap<Date, FileObject>();

    final FileObject[] existing2dFiles = find2dFiles( m_resultDir );

    for( final FileObject file : existing2dFiles )
    {
      final String baseName = file.getName().getBaseName();
      if( baseName.equals( "steady.2d" ) ) //$NON-NLS-1$
        m_dateFileMap.put( STEADY_DATE, file );

      if( baseName.equals( "maxi.2d" ) ) //$NON-NLS-1$
        m_dateFileMap.put( MAXI_DATE, file );

      if( baseName.equals( "steady.2d" ) || baseName.equals( "maxi.2d" ) || baseName.equals( "mini.2d" ) || baseName.equals( "model.2d" ) ) //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
        continue;

      final String resultFileName = baseName;
      final int index = resultFileName.indexOf( "." ); //$NON-NLS-1$
      final CharSequence sequence = resultFileName.subSequence( 1, index );
      final String string = sequence.toString();
      final int step = Integer.parseInt( string );

      final IObservation<TupleResult> obs = controlModel.getTimeSteps();
      final TupleResult timeSteps = obs.getResult();

      final IComponent componentTime = ComponentUtilities.findComponentByID( timeSteps.getComponents(), Kalypso1D2DDictConstants.DICT_COMPONENT_TIME );
      final XMLGregorianCalendar stepCal = (XMLGregorianCalendar) timeSteps.get( step ).getValue( componentTime );
      final Date fileDate = DateUtilities.toDate( stepCal );
      m_dateFileMap.put( fileDate, file );
    }
  }
}
