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

import java.io.File;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.net.URISyntaxException;
import java.util.Date;
import java.util.List;

import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.vfs2.FileObject;
import org.apache.commons.vfs2.FileSystemException;
import org.apache.commons.vfs2.FileSystemManager;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.kalypso.afgui.model.IModel;
import org.kalypso.commons.io.VFSUtilities;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.kalypso1d2d.pjt.Kalypso1d2dProjectPlugin;
import org.kalypso.kalypsomodel1d2d.conv.results.ResultMeta1d2dHelper;
import org.kalypso.kalypsomodel1d2d.conv.results.ResultType.TYPE;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModel1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.ICalcUnitResultMeta;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IScenarioResultMeta;
import org.kalypso.kalypsomodel1d2d.sim.ProcessResult2DOperation;
import org.kalypso.kalypsomodel1d2d.sim.ResultManager;
import org.kalypso.kalypsosimulationmodel.core.flowrel.IFlowRelationshipModel;
import org.kalypso.kalypsosimulationmodel.core.resultmeta.IResultMeta;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;

import de.renew.workflow.connector.cases.ICaseDataProvider;

/**
 * @author Gernot Belger
 */
public class Import2DResultsOperation implements ICoreRunnableWithProgress
{
  private final IStatusCollector m_stati = new StatusCollector( Kalypso1d2dProjectPlugin.PLUGIN_ID );

  private final File[] m_files;

  private final ICaseDataProvider<IModel> m_modelProvider;

  public Import2DResultsOperation( final File[] files, final ICaseDataProvider<IModel> modelProvider )
  {
    m_files = files;
    m_modelProvider = modelProvider;
  }

  @Override
  public IStatus execute( final IProgressMonitor monitor ) throws CoreException, InvocationTargetException
  {
    monitor.beginTask( "Importing external 2D results", 1 + m_files.length );

    // TODO: create iCalcunitMeta
    final ICalcUnitResultMeta calcMeta = findOrCreateCalcMeta();

    // TODO: remove old nodes with same file name -> the operation wil ladd the automatically
    removeOldResults( calcMeta );

    // prerequisites:
    // - empty result node
    // - ...?
    final File outputDir = new File( FileUtils.getTempDirectory(), "2dtest" );

    ProgressUtilities.worked( monitor, 1 );

    final FileSystemManager manager = getFileSystemManager();

    for( final File file : m_files )
    {
      final String filename = file.getName();
      monitor.subTask( String.format( "importing %s", filename ) );
      try
      {
        final FileObject file2d = manager.resolveFile( file.toURI().toString() );
        importFile( file2d, outputDir, calcMeta, new SubProgressMonitor( monitor, 1 ) );
      }
      catch( final FileSystemException e )
      {
        m_stati.add( IStatus.ERROR, "Failed to import result file '%s'", e, filename );
      }
    }

    monitor.done();

    return m_stati.asMultiStatusOrOK( "Problem(s) during import", String.format( "%d file(s) imported", m_files.length ) );
  }

  private void removeOldResults( final ICalcUnitResultMeta calcMeta )
  {
    // FIXME: the result processor addd the step meta himself, so we are not able to know what id/name is set :-(
    // For now, we clear everything
    // TODO: clear underlying data

    final IFeatureBindingCollection<IResultMeta> children = calcMeta.getChildren();
    children.clear();
  }

  private ICalcUnitResultMeta findOrCreateCalcMeta( ) throws CoreException
  {
    final String calcUnitMetaName = findCalcMetaName();

    final IScenarioResultMeta scenarioResultMeta = m_modelProvider.getModel( IScenarioResultMeta.class.getName(), IScenarioResultMeta.class );
    final ICalcUnitResultMeta existingMeta = scenarioResultMeta.findCalcUnitMetaResult( calcUnitMetaName );
    if( existingMeta != null )
      return existingMeta;

    final IFeatureBindingCollection<IResultMeta> children = scenarioResultMeta.getChildren();
    final ICalcUnitResultMeta newMeta = children.addNew( ICalcUnitResultMeta.QNAME, calcUnitMetaName, ICalcUnitResultMeta.class );
    final String description = String.format( "External .2d files imported from '%s'", calcUnitMetaName );
    newMeta.setDescription( description );
    newMeta.setName( calcUnitMetaName );
    return newMeta;
  }

  private String findCalcMetaName( )
  {
    final String path = m_files[0].getParentFile().getAbsolutePath();
    return FileUtilities.validateName( path, "_" ); //$NON-NLS-1$
  }

  private FileSystemManager getFileSystemManager( ) throws InvocationTargetException
  {
    try
    {
      return VFSUtilities.getManager();
    }
    catch( final FileSystemException e )
    {
      throw new InvocationTargetException( e );
    }
  }

  private void importFile( final FileObject inputFile, final File outputDir, final ICalcUnitResultMeta calcMeta, final IProgressMonitor monitor ) throws InvocationTargetException
  {
    final Date stepDate = findStepDate( inputFile );

    final File resultDir = new File( outputDir, inputFile.getName().getBaseName() );
    resultDir.mkdirs();

    final IFlowRelationshipModel flowModel = null;
    final IControlModel1D2D controlModel = null;
    final IFEDiscretisationModel1d2d discModel = null;

    final List<TYPE> parameter = null; // read all default parameters

    final ProcessResult2DOperation operation = new ProcessResult2DOperation( inputFile, null, resultDir, flowModel, controlModel, discModel, parameter, stepDate, calcMeta, false );
    final IStatus fileStatus = operation.execute( monitor );
    m_stati.add( fileStatus );
  }

  private Date findStepDate( final FileObject inputFile ) throws InvocationTargetException
  {
    try
    {
      final String timeLine = ResultMeta1d2dHelper.findFirstSpecifiedLine2dFile( inputFile, "TI" ); //$NON-NLS-1$
      if( StringUtils.isBlank( timeLine ) )
        return ResultManager.STEADY_DATE;

      // FIXME: doe snot work yet -> special case for bce-2d
      final Date stepDate = ResultMeta1d2dHelper.interpreteRMA10TimeLine( timeLine );
      if( stepDate == null )
        return ResultManager.STEADY_DATE;

      return stepDate;
    }
    catch( final IOException e )
    {
      // stop operation if we have an io exception
      throw new InvocationTargetException( e );
    }
    catch( final URISyntaxException e )
    {
      e.printStackTrace();
      throw new InvocationTargetException( e );
    }
  }
}