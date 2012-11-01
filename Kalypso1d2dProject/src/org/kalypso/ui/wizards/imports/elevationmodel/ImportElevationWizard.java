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
package org.kalypso.ui.wizards.imports.elevationmodel;

import java.io.File;
import java.io.UnsupportedEncodingException;
import java.lang.reflect.InvocationTargetException;
import java.net.URLDecoder;
import java.util.Date;
import java.util.Iterator;
import java.util.Random;

import org.apache.commons.io.FileUtils;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.SubMonitor;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.IWorkbench;
import org.kalypso.afgui.KalypsoAFGUIFrameworkPlugin;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.kalypso1d2d.internal.i18n.Messages;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.INativeTerrainElevationModelWrapper;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.ITerrainElevationModelSystem;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.ITerrainModel;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.NativeTerrainElevationModelWrapper;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;

import de.renew.workflow.connector.cases.IScenarioDataProvider;

/**
 * @author Madanagopal
 * @author Patrice Congo
 */
public class ImportElevationWizard extends Wizard implements INewWizard/* INewWizardKalypsoImport */
{
  private IStructuredSelection m_initialSelection;

  private ElevationMainPage m_mPage;

  /**
   * Folder containing the terrain model
   */
  protected IFolder m_modelFolder;

  protected ITerrainModel m_terrainModel;

  // private String m_scenarioFolder;
  /**
   * destination folders for the native terrain elevation model
   */
  protected IFolder m_tempFolder;

  /**
   * Construct a new instance and initialize the dialog settings for this instance.
   */
  public ImportElevationWizard( )
  {
  }

  /**
   * The required selection structure is:
   * <ul>
   * <li/>Length=2
   * <li/>First element an instance of {@link ITerrainModel}
   * <li/>Second element an instance of {@link IFolder}
   * <li/>third element an instance of {@link CommandableWorkspace}
   * </ul>
   * 
   * @param workbench
   *          the current workbench
   * @param selection
   *          the current object selection
   */
  @Override
  public void init( final IWorkbench workbench, final IStructuredSelection selection )
  {
    m_initialSelection = selection;
    final Iterator< ? > selIterator = selection.iterator();
    m_terrainModel = (ITerrainModel)selIterator.next();
    m_modelFolder = (IFolder)selIterator.next();
    m_tempFolder = (IFolder)selIterator.next();
  }

  @Override
  public void addPages( )
  {
    setWindowTitle( Messages.getString( "org.kalypso.ui.wizards.imports.elevationModel.Elevation.0" ) ); //$NON-NLS-1$
    m_mPage = new ElevationMainPage();
    addPage( m_mPage );
    m_mPage.init( m_initialSelection );
  }

  @Override
  public boolean performFinish( )
  {
    try
    {
      final IPath sourcePath = m_mPage.getSourceLocation();// .toOSString();
      final String setFileName = m_mPage.getNameForFile();
      final String setFileDescription = m_mPage.getDescriptionForFileArea();
      final String defaultText = ""; //$NON-NLS-1$
      final String replaceText = Messages.getString( "org.kalypso.ui.wizards.imports.elevationModel.Elevation.10" ); //$NON-NLS-1$
      final String selectedCoordinateSystem = m_mPage.getCoordinateSystem();

      getContainer().run( true, true, new IRunnableWithProgress()
      {
        @Override
        public void run( final IProgressMonitor monitor ) throws InvocationTargetException, IllegalArgumentException
        {
          try
          {
            SubMonitor progress = null;
            if( monitor != null )
            {
              progress = SubMonitor.convert( monitor, Messages.getString( "ImportElevationWizard.0" ), 100 ); //$NON-NLS-1$
            }
            final ITerrainElevationModelSystem temSys = m_terrainModel.getTerrainElevationModelSystem();

            final GMLWorkspace workspace = temSys.getWorkspace();

            // Decoding the White Spaces present in the File Paths.
            final File modelFolderFile = getUTF_DecodedFile( new File( FileLocator.toFileURL( workspace.getContext() ).getFile() ).getParentFile() );
            final File temFolderFile = getUTF_DecodedFile( new File( FileLocator.toFileURL( m_tempFolder.getLocationURI().toURL() ).getFile() ) );
            final File srcFileTif = getUTF_DecodedFile( sourcePath.toFile() );

            if( progress != null )
            {
              ProgressUtilities.worked( progress, 30 );
            }
            File dstFileTif = null;

            if( (new File( temFolderFile, srcFileTif.getName() )).exists() )
            {
              dstFileTif = new File( temFolderFile, getNewFileName( temFolderFile, srcFileTif ) );
            }
            else
            {
              dstFileTif = new File( temFolderFile, srcFileTif.getName() ); // mPage.getSourceLocation().lastSegment()
            }

            copy( srcFileTif, dstFileTif, monitor );
            m_modelFolder.getProject().refreshLocal( IResource.DEPTH_INFINITE, null/* new NullProgressMonitor() */);
            String nativeTEMRelPath = modelFolderFile.toURI().relativize( new File( URLDecoder.decode( dstFileTif.toString(), "UTF-8" ) ).toURI() ).toString(); //$NON-NLS-1$
            if( nativeTEMRelPath == null )
            {
              nativeTEMRelPath = getUTF_DecodedFile( dstFileTif ).toString();
            }

            final INativeTerrainElevationModelWrapper tem = (INativeTerrainElevationModelWrapper)temSys.getTerrainElevationModels().addNew( NativeTerrainElevationModelWrapper.SIM_BASE_F_NATIVE_TERRAIN_ELE_WRAPPER );
            tem.setSourceFile( nativeTEMRelPath );

            // TODO introduce in the first page a name imput field and gets the
            // name from there

            final String name = dstFileTif.getName();
            if( setFileName.compareTo( "" ) != 0 ) //$NON-NLS-1$
            {
              tem.setName( setFileName );
            }
            else
            {
              tem.setName( name );
            }
            if( setFileDescription.compareTo( defaultText ) != 0 )
            {
              tem.setDescription( setFileDescription );
            }
            else
            {
              tem.setDescription( replaceText );
            }

            if( selectedCoordinateSystem.compareTo( "" ) != 0 ) //$NON-NLS-1$
            {
              tem.setCoordinateSystem( selectedCoordinateSystem );
            }

            final Feature temFeature = tem;
            workspace.fireModellEvent( new FeatureStructureChangeModellEvent( workspace, temSys, temFeature, FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD ) );
            workspace.fireModellEvent( new FeatureStructureChangeModellEvent( workspace, temFeature.getOwner(), temFeature, FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD ) );
            // TODO check why saving thow pool does not work
            final IScenarioDataProvider caseDataProvider = KalypsoAFGUIFrameworkPlugin.getDataProvider();
            caseDataProvider.postCommand( ITerrainModel.class.getName(), new AddTerrainelevationModelCmd() );

            caseDataProvider.saveModel( ITerrainModel.class.getName(), null );

            if( progress != null )
            {
              ProgressUtilities.worked( progress, 100 );
              progress.done();
            }
          }

          catch( final Exception e )
          {
            e.printStackTrace();
            throw new InvocationTargetException( e );
          }
        }
      } );

    }
    catch( final Throwable th )
    {
      th.printStackTrace();
      return false;
    }

    return true;
  }

  String getNewFileName( final File folder, final File srcFileTif )
  {
    final Random generator = new Random( 126545 );
    final int key = (int)((new Date()).getTime() + generator.nextInt());

    if( new File( folder, getFileNameNoExtension( srcFileTif ) + "_" + key + "." + getExtension( srcFileTif ).toString() ).exists() ) //$NON-NLS-1$ //$NON-NLS-2$
      getNewFileName( folder, srcFileTif );

    return getFileNameNoExtension( srcFileTif ) + "_" + key + "." + getExtension( srcFileTif ); //$NON-NLS-1$ //$NON-NLS-2$
  }

  public static String getExtension( final File f )
  {
    String ext = null;
    final String s = f.getName();
    final int i = s.lastIndexOf( '.' );

    if( i > 0 && i < s.length() - 1 )
    {
      ext = s.substring( i + 1 ).toLowerCase();
    }
    return ext;
  }

  public static String getFileNameNoExtension( final File f )
  {
    String ext = null;
    final String s = f.getName();
    final int i = s.lastIndexOf( '.' );

    if( i > 0 && i < s.length() - 1 )
    {
      ext = s.substring( 0, i );
    }
    return ext;
  }

  protected boolean copy( final File src, final File dst, final IProgressMonitor monitor2 )
  {
    SubMonitor progress = null;
    if( monitor2 != null )
    {
      final String taskName = String.format( Messages.getString( "ImportElevationWizard.1" ), src.getName(), dst.getName() ); //$NON-NLS-1$
      progress = SubMonitor.convert( monitor2, taskName, 100 );
    }

    try
    {
      FileUtils.copyFile( src, dst );
      if( progress != null )
      {
        ProgressUtilities.worked( progress, 100 );
      }
      return true;
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      return false;
    }
  }

  /**
   * Answer the selected source location
   */
  public IPath getSourceLocationsss( )
  {
    return m_mPage.getSourceLocation();
  }

  public String getFileDescription( )
  {
    return m_mPage.getDescriptionForFileArea();
  }

  public String getFileUserName( )
  {
    return m_mPage.getNameForFile();
  }

  public File getUTF_DecodedFile( final File file )
  {
    try
    {
      return new File( URLDecoder.decode( file.toString(), "UTF-8" ) ); //$NON-NLS-1$
    }
    catch( final UnsupportedEncodingException e )
    {
      e.printStackTrace();
    }
    return file;
  }

}
