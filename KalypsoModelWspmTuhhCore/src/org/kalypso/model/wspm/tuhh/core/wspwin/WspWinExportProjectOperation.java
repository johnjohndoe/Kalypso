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
package org.kalypso.model.wspm.tuhh.core.wspwin;

import java.io.File;
import java.util.ArrayList;
import java.util.Collection;

import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.model.wspm.core.gml.WspmReach;
import org.kalypso.model.wspm.core.gml.WspmWaterBody;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.core.KalypsoModelWspmTuhhCorePlugin;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReach;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhWspmProject;
import org.kalypso.model.wspm.tuhh.core.i18n.Messages;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.wspwin.core.WspCfg.TYPE;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;

/**
 * @author Gernot Belger
 */
public class WspWinExportProjectOperation implements ICoreRunnableWithProgress
{
  private final WspWinExportProjectData m_data;

  public WspWinExportProjectOperation( final WspWinExportProjectData data )
  {
    m_data = data;
  }

  @Override
  public IStatus execute( final IProgressMonitor monitor ) throws CoreException
  {
    final IProject[] projects = m_data.getSelectedProjects();

    monitor.beginTask( Messages.getString( "org.kalypso.model.wspm.tuhh.core.wspwin.WspWinExporter.0" ), projects.length ); //$NON-NLS-1$

    monitor.subTask( Messages.getString( "org.kalypso.model.wspm.tuhh.core.wspwin.WspWinExporter.1" ) ); //$NON-NLS-1$

    for( final IProject project : projects )
    {
      try
      {
        exportProject( project, new SubProgressMonitor( monitor, 1 ) );
      }
      catch( final CoreException e )
      {
        throw e;
      }
      catch( final Throwable t )
      {
        final String message = String.format( Messages.getString( "WspWinExporter.0" ), t.getLocalizedMessage() ); //$NON-NLS-1$
        return new Status( IStatus.ERROR, KalypsoModelWspmTuhhCorePlugin.PLUGIN_ID, message, t );
      }
      finally
      {
        // clean up
        monitor.done();
      }
    }
    return Status.OK_STATUS;
  }

  private void exportProject( final IProject project, final IProgressMonitor monitor ) throws Exception
  {
    monitor.beginTask( StringUtils.EMPTY, 100 );

    final IFile modelGmlFile = project.getFile( IWspmTuhhConstants.FILE_MODELL_GML );

    // read gml workspace
    monitor.subTask( Messages.getString( "org.kalypso.model.wspm.tuhh.core.wspwin.WspWinExporter.3", project.getName() ) ); //$NON-NLS-1$
    final GMLWorkspace modelWorkspace = GmlSerializer.createGMLWorkspace( modelGmlFile, new SubProgressMonitor( monitor, 50 ) );
    final TuhhWspmProject wspmProject = (TuhhWspmProject) modelWorkspace.getRootFeature();

    // create unique wspwinProjectDir
    final File exportDir = m_data.getOutputDir();
    final boolean overwriteExisting = m_data.getOverwriteExisting();
    final File wspwinProjDir = new File( exportDir, project.getName() );
    final TYPE projectType = m_data.getProjectType();

    final WspWinOutputDirGenerator dirGenerator = new WspWinOutputDirGenerator( wspwinProjDir, overwriteExisting );

    final String roughnessType = m_data.getRoughnessType();
    final boolean preferRoughnessClasses = m_data.getPreferRoughnessClasses();
    final boolean preferVegetationClasses = m_data.getPreferVegetationClasses();

    // write data into wspwinDir projectDir
    monitor.subTask( Messages.getString( "org.kalypso.model.wspm.tuhh.core.wspwin.WspWinExporter.6" ) ); //$NON-NLS-1$

    // We write one wspwin-project per water body
    final IFeatureBindingCollection<WspmWaterBody> waterBodies = wspmProject.getWaterBodies();
    for( final WspmWaterBody waterBody : waterBodies )
    {
      /* We always export all reaches of this water body */
      final TuhhReach[] reaches = getReaches( waterBody );

      final File outputDir = dirGenerator.generateOutputDir( waterBody.getName(), waterBody.getId() );
      if( outputDir != null )
      {
        FileUtils.deleteDirectory( outputDir );

        final WspWinProjectExporter exporter = new WspWinProjectExporter( waterBody, reaches, projectType, roughnessType, preferRoughnessClasses, preferVegetationClasses );
        exporter.export( outputDir );
      }
    }

    monitor.worked( 50 );

    monitor.done();
  }

  private TuhhReach[] getReaches( final WspmWaterBody waterBody )
  {
    final Collection<TuhhReach> result = new ArrayList<>();

    final IFeatureBindingCollection<WspmReach> reaches = waterBody.getReaches();
    for( final WspmReach reach : reaches )
    {
      if( reach instanceof TuhhReach )
        result.add( (TuhhReach) reach );
    }

    return result.toArray( new TuhhReach[result.size()] );
  }
}
