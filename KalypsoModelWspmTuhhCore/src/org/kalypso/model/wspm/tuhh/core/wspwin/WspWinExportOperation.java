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

import org.apache.commons.io.FileUtils;
import org.apache.commons.lang.StringUtils;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.swt.awt.SWT_AWT_Utilities;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.core.KalypsoModelWspmTuhhCorePlugin;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhCalculation;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhWspmProject;
import org.kalypso.model.wspm.tuhh.core.i18n.Messages;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * @author Gernot Belger
 */
public class WspWinExportOperation implements ICoreRunnableWithProgress
{
  enum AskMode
  {
    ALWAYS,
    NEVER,
    NOTSET;
  }

  private AskMode m_askMode = AskMode.NOTSET;

  private final WspWinExportData m_data;

  public WspWinExportOperation( final WspWinExportData data )
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
    final File wspwinDir = m_data.getOutputDir();
    final File wspwinProjDir = new File( wspwinDir, project.getName() );

    // write data into wspwinDir projectDir
    monitor.subTask( Messages.getString( "org.kalypso.model.wspm.tuhh.core.wspwin.WspWinExporter.6" ) ); //$NON-NLS-1$

    // CalculationTuhh
    final TuhhCalculation[] tuhhCalcs = wspmProject.getCalculations();
    for( final TuhhCalculation calculation : tuhhCalcs )
    {
      final String calcName = calculation.getName();
      /* Fall back to id if name is blank */
      final String calcDirName = StringUtils.isBlank( calcName ) ? calculation.getId() : calcName;
      final String dirName = FileUtilities.validateName( calcDirName, "_" ); //$NON-NLS-1$
      final File dir = new File( wspwinProjDir, dirName );
      final File calcOutputDir = checkExistance( dir );
      if( calcOutputDir != null )
      {
        FileUtils.deleteDirectory( calcOutputDir );
        WspWinExporter.writeForTuhhKernel( calculation, calcOutputDir );
      }
    }
    monitor.worked( 50 );

    monitor.done();
  }

  private File checkExistance( final File dir ) throws CoreException
  {
    /* No check, if we should just overwrite */
    if( m_data.getOverwriteExisting() )
      return dir;

    if( !dir.exists() )
      return dir;

    if( dir.isFile() )
    {
      final String msg = String.format( "Cannot create directory '%s'. A file with the same name already exists.", dir.getName() );
      final IStatus status = new Status( IStatus.ERROR, KalypsoModelWspmTuhhCorePlugin.PLUGIN_ID, msg );
      throw new CoreException( status );
    }

    switch( m_askMode )
    {
      case ALWAYS:
        return dir;
      case NEVER:
        return null;

      case NOTSET:
      default:
        return askForExistence( dir );
    }
  }

  private File askForExistence( final File dir )
  {
    final String message = String.format( "Output directory '%s' already exists. Overwrite?", dir.getName() );
    final String[] labels = new String[] { IDialogConstants.YES_LABEL, IDialogConstants.NO_LABEL, "&Always", "&Never", IDialogConstants.CANCEL_LABEL };

    final Shell shell = SWT_AWT_Utilities.findActiveShell();
    final MessageDialog dialog = new MessageDialog( shell, "WspWin Export", null, message, MessageDialog.QUESTION, labels, 0 );
    switch( SWT_AWT_Utilities.openSwtMessageDialog( dialog ) )
    {
      case 0:
        return dir;

      case 1:
        return null;

      case 2:
        m_askMode = AskMode.ALWAYS;
        return dir;

      case 3:
        m_askMode = AskMode.NEVER;
        return null;

      case 4:
        throw new OperationCanceledException();
      default:
        throw new IllegalStateException();
    }
  }
}
