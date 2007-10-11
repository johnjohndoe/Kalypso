/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and

 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de

 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.risk.project;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.util.logging.Logger;

import org.apache.commons.io.IOUtils;
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.resources.IProjectNature;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.kalypso.commons.java.util.zip.ZipUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.risk.plugin.KalypsoRiskPlugin;

/**
 * Project Nature for 1d 2d simulation, requires {@link org.kalypso.afgui.ScenarioHandlingProjectNature}
 * 
 * @author Patrice Congo, Stefan Kurzbach
 */
public class KalypsoRiskProjectNature implements IProjectNature
{
  public static final String ID = "org.kalypso.risk.project.KalypsoRiskProjectNature";  //$NON-NLS-1$

  private final static Logger logger = Logger.getLogger( KalypsoRiskProjectNature.class.getName() );

  // TODO: change tracing option id (use Debug helper class, see KalypsoUIDebug example)
  private static final boolean log = Boolean.parseBoolean( Platform.getDebugOption( "org.kalypso.kalypso1d2d.pjt/debug" ) );  //$NON-NLS-1$

  static
  {
    if( !log )
      logger.setUseParentHandlers( false );
  }

  private static final String EMPTY_PROJECT_ZIP_PATH = "RiskProjectTemplate.zip";  //$NON-NLS-1$

  private IProject m_project;

  /**
   * @see org.eclipse.core.resources.IProjectNature#configure()
   */
  public void configure( ) throws CoreException
  {
    final NullProgressMonitor monitor = new NullProgressMonitor();

    final IFolder basisFolder = getProject().getFolder( "Basis" );
    if( !basisFolder.exists() )
    {
      final URL zipLocation = getClass().getResource( EMPTY_PROJECT_ZIP_PATH );
      unzipToContainer( zipLocation, getProject(), monitor );
    }
  }

  public static final boolean isOfThisNature( final IProject project ) throws CoreException
  {
    return project == null ? false : project.hasNature( ID );
  }

  public static final void addNature( final IProject project ) throws CoreException
  {
    // TODO: move that to a central place (i.e. platform contribs)
    if( project.hasNature( ID ) )
    {
      return;
    }
    else
    {
      IProjectDescription description = project.getDescription();
      String[] natures = description.getNatureIds();
      String[] newNatures = new String[natures.length + 1];
      System.arraycopy( natures, 0, newNatures, 0, natures.length );
      newNatures[natures.length] = ID;
      description.setNatureIds( newNatures );
      project.setDescription( description, new NullProgressMonitor() );
    }
  }

  /**
   * TODO: move this method into common helper class inside KalypsoCommons.
   * 
   * @see org.eclipse.core.resources.IProjectNature#configure()
   */
  private void unzipToContainer( final URL zipLocation, final IContainer targetContainer, final IProgressMonitor monitor ) throws CoreException
  {
    monitor.beginTask( "", 2 );  //$NON-NLS-1$

    InputStream zipStream = null;
    try
    {
      // unpack other file structure from zip
      zipStream = new BufferedInputStream( zipLocation.openStream() );

      // REMARK: unzipping files with non UTF-8 filename encoding is really awesome in java.
      // We do use the apache tools, which let us at least set the encoding.
      // Try and error led to use the encoding: "IBM850" for WinZippes .zip's.
      // REMARK: This doesn�t work in the deploy (I don�t know why???) -use simple unzip instead works!!!
      // ZipUtilities.unzipApache( zipStream, targetContainer.getLocation().toFile(), true, "IBM850" );
      ZipUtilities.unzip( zipStream, targetContainer.getLocation().toFile() );
      zipStream.close();
      monitor.worked( 1 );

      targetContainer.refreshLocal( IResource.DEPTH_INFINITE, new SubProgressMonitor( monitor, 1 ) );
    }
    catch( final IOException e )
    {
      final IStatus status = StatusUtilities.statusFromThrowable( e );
      KalypsoRiskPlugin.getDefault().getLog().log( status );
      throw new CoreException( status );
    }
    finally
    {
      IOUtils.closeQuietly( zipStream );
    }
  }

  public static KalypsoRiskProjectNature getNature( final IProject project ) throws CoreException
  {
    return (KalypsoRiskProjectNature) project.getNature( ID );
  }

  /**
   * @see org.eclipse.core.resources.IProjectNature#deconfigure()
   */
  public void deconfigure( )
  {
    // does nothing by default
  }

  /**
   * @see org.eclipse.core.resources.IProjectNature#getProject()
   */
  public IProject getProject( )
  {
    return m_project;
  }

  /**
   * @see org.eclipse.core.resources.IProjectNature#setProject(org.eclipse.core.resources.IProject)
   */
  public void setProject( final IProject project )
  {
    this.m_project = project;
  }
}