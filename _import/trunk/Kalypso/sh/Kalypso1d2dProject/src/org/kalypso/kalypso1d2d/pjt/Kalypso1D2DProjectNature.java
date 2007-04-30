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
package org.kalypso.kalypso1d2d.pjt;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.util.logging.Logger;

import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;

import org.apache.commons.io.IOUtils;
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.resources.IProjectNature;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.kalypso.afgui.scenarios.IScenarioManagerListener;
import org.kalypso.afgui.scenarios.Scenario;
import org.kalypso.afgui.scenarios.ScenarioManager;
import org.kalypso.commons.java.util.zip.ZipUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.simulation.core.ISimulationService;
import org.kalypso.simulation.core.KalypsoSimulationCorePlugin;
import org.kalypso.simulation.core.simspec.Modeldata;
import org.kalypso.simulation.ui.calccase.CalcJobHandler;
import org.kalypso.simulation.ui.calccase.ModelNature;

/**
 * Project Nature for 1d 2d simulation
 * 
 * @author Patrice Congo
 */
public class Kalypso1D2DProjectNature implements IProjectNature, IScenarioManagerListener
{
  private final static Logger logger = Logger.getLogger( Kalypso1D2DProjectNature.class.getName() );

  private static final boolean log = Boolean.parseBoolean( Platform.getDebugOption( "org.kalypso.kalypso1d2d.pjt/debug" ) );

  static
  {
    if( !log )
      logger.setUseParentHandlers( false );
  }

  public static final String ID = "org.kalypso.kalypso1d2d.pjt.Kalypso1D2DProjectNature";

  private static final String EMPTY_PROJECT_ZIP_PATH = "resources/emptyProject.zip";

  private ScenarioManager m_scenarioManager;

  private IProject m_project;

  private IFolder m_metaDataFolder;

  /**
   * @see org.eclipse.core.resources.IProjectNature#configure()
   */
  public void configure( ) throws CoreException
  {
    logger.info( "Configuring: " + m_project );
    addNature( m_project );

    final IFolder metaFolder = getProject().getFolder( ScenarioManager.METADATA_FOLDER );
    if( !metaFolder.exists() )
    {
      final NullProgressMonitor monitor = new NullProgressMonitor();
      final URL zipLocation = getClass().getResource( EMPTY_PROJECT_ZIP_PATH );
      unzipToContainer( zipLocation, getProject(), monitor );
    }
  }

  /**
   * @see org.eclipse.core.resources.IProjectNature#deconfigure()
   */
  public void deconfigure( )
  {
    // not possible
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
  synchronized public void setProject( final IProject project )
  {
    logger.info( "Setting project" );
    this.m_project = project;
  }

  private void init( )
  {
    m_metaDataFolder = m_project.getFolder( ScenarioManager.METADATA_FOLDER );
    m_scenarioManager = new ScenarioManager( m_project );
    m_scenarioManager.addScenarioManagerListener( this );
  }

  synchronized public ScenarioManager getScenarioManager( )
  {
    if( m_scenarioManager == null )
    {
      init();
    }
    return m_scenarioManager;
  }

  public static final boolean isOfThisNature( final IProject project ) throws CoreException
  {
    return project == null ? false : project.hasNature( ID );
  }

  public static final Kalypso1D2DProjectNature toThisNature( IProject project ) throws CoreException
  {
    // project.hasNature(ID);
    return (Kalypso1D2DProjectNature) project.getNature( ID );
  }

  public static final void addNature( final IProject project ) throws CoreException
  {
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
      // Kalypso1d2dProjectPlugin.getDefault().showMessage(Arrays.asList(newNatures).toString());
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
    monitor.beginTask( "", 2 );

    InputStream zipStream = null;
    try
    {
      // unpack other file structure from zip
      zipStream = new BufferedInputStream( zipLocation.openStream() );

      // REMARK: unzipping files with non UTF-8 filename encoding is really awesome in java.
      // We do use the apache tools, which let us at least set the encoding.
      // Try and error led to use the encoding: "IBM850" for WinZippes .zip's.
      // REMARK: This doesn´t work in the deploy (I don´t know why???) -use simple unzip instead works!!!
      // ZipUtilities.unzipApache( zipStream, targetContainer.getLocation().toFile(), true, "IBM850" );
      ZipUtilities.unzip( zipStream, targetContainer.getLocation().toFile() );
      zipStream.close();
      monitor.worked( 1 );

      targetContainer.refreshLocal( IResource.DEPTH_INFINITE, new SubProgressMonitor( monitor, 1 ) );
    }
    catch( final IOException e )
    {
      final IStatus status = StatusUtilities.statusFromThrowable( e );
      Kalypso1d2dProjectPlugin.getDefault().getLog().log( status );
      throw new CoreException( status );
    }
    finally
    {
      IOUtils.closeQuietly( zipStream );
    }
  }

  public IFolder getImportFolder( )
  {
    return m_metaDataFolder;
  }

  /**
   * @see org.kalypso.afgui.scenarios.IScenarioManagerListener#scenarioAdded(org.kalypso.afgui.scenarios.Scenario)
   */
  public void scenarioAdded( final Scenario scenario )
  {
    final IFolder newFolder = m_project.getFolder( m_scenarioManager.getProjectPath( scenario ) );

    if( !newFolder.exists() )
    {
      try
      {
        newFolder.create( false, true, null );
        final URL resource = getClass().getResource( EMPTY_PROJECT_ZIP_PATH );
        final IPath newLocation = newFolder.getLocation();
        ZipUtilities.unzip( resource.openStream(), "Basis/**", newLocation.toFile(), false );
        newFolder.refreshLocal( IResource.DEPTH_INFINITE, null );
        final IFolder basisFolder = newFolder.getFolder( "Basis" );
        for( final IResource res : basisFolder.members() )
        {
          res.move( newFolder.getFullPath().append( res.getName() ), false, null );
        }
        basisFolder.delete( false, null );
      }
      catch( final Throwable e )
      {
        final Shell activeShell = PlatformUI.getWorkbench().getDisplay().getActiveShell();
        final IStatus status = StatusUtilities.statusFromThrowable( e );
        ErrorDialog.openError( activeShell, "Problem", "Konnte neue Szenariodaten nicht erzeugen.", status );
        Kalypso1d2dProjectPlugin.getDefault().getLog().log( status );
      }
    }
  }

  /**
   * @see org.kalypso.afgui.scenarios.IScenarioManagerListener#scenarioRemoved(org.kalypso.afgui.scenarios.Scenario)
   */
  public void scenarioRemoved( final Scenario scenario )
  {
    final IFolder folder = m_project.getFolder( m_scenarioManager.getProjectPath( scenario ) );
    try
    {
      folder.delete( true, null );
    }
    catch( final CoreException e )
    {
      final Shell activeShell = PlatformUI.getWorkbench().getDisplay().getActiveShell();
      final IStatus status = StatusUtilities.statusFromThrowable( e );
      ErrorDialog.openError( activeShell, "Problem", "Konnte Szenario nicht löschen.", status );
      Kalypso1d2dProjectPlugin.getDefault().getLog().log( status );
    }
  }

  public static Kalypso1D2DProjectNature getNature( final IProject project ) throws CoreException
  {
    return (Kalypso1D2DProjectNature) project.getNature( ID );
  }

  public IStatus startCalculation( final IFolder scenarioFolder, final IProgressMonitor monitor ) throws CoreException
  {
    monitor.beginTask( "Modellrechnung wird durchgeführt", 5 );

    try
    {
      final Modeldata modelspec = loadModelspec(  );
      final String typeID = modelspec.getTypeID();
      final ISimulationService calcService = KalypsoSimulationCorePlugin.findCalculationServiceForType( typeID );
      monitor.worked( 1 );
      final CalcJobHandler cjHandler = new CalcJobHandler( modelspec, calcService );
      return cjHandler.runJob( scenarioFolder, new SubProgressMonitor( monitor, 4 ) );
    }
    finally
    {
      monitor.done();
    }
  }
  
  private Modeldata loadModelspec(  ) throws CoreException
  {
    try
    {
      final IFolder modelFolder = m_project.getFolder( ModelNature.MODELLTYP_FOLDER );
      final IFile file = modelFolder.getFile( ModelNature.MODELLTYP_MODELSPEC_XML );
      if( !file.exists() )
        return null;

      final Unmarshaller unmarshaller = ModelNature.JC_SPEC.createUnmarshaller();
      return (Modeldata) unmarshaller.unmarshal( file.getContents() );
    }
    catch( final JAXBException e )
    {
      e.printStackTrace();

      throw new CoreException( StatusUtilities.statusFromThrowable( e, "Fehler beim Laden der Modell-Spezifikation" ) );
    }
  }

  
  
}