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
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.UnsupportedEncodingException;
import java.lang.reflect.InvocationTargetException;
import java.net.URLDecoder;
import java.util.Iterator;
import java.util.Random;

import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.IWorkbench;
import org.kalypso.afgui.scenarios.SzenarioDataProvider;
import org.kalypso.core.util.pool.IPoolListener;
import org.kalypso.core.util.pool.IPoolableObjectType;
import org.kalypso.core.util.pool.ResourcePool;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.ITerrainElevationModel;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.ITerrainElevationModelSystem;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.ITerrainModel;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.NativeTerrainElevationModelWrapper;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.TerrainElevationModelSystem;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.ui.wizards.i18n.Messages;
import org.kalypso.ui.wizards.imports.utils.Util;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;

/**
 * @author Madanagopal
 * @author Patrice Congo
 */
public class ImportElevationWizard extends Wizard implements INewWizard/* INewWizardKalypsoImport */
{
  private IStructuredSelection initialSelection;

  private ElevationMainPage mPage;

  /**
   * Folder containing the terrain model
   */
  private IFolder modelFolder;

  private ITerrainModel terrainModel;

  // private String m_scenarioFolder;
  /**
   * destination folders for the native terrain elevation model
   */
  private IFolder temFolder;

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
  public void init( final IWorkbench workbench, final IStructuredSelection selection )
  {
    initialSelection = selection;
    final Iterator selIterator = selection.iterator();
    terrainModel = (ITerrainModel) selIterator.next();
    modelFolder = (IFolder) selIterator.next();
    temFolder = (IFolder) selIterator.next();
  }

  @Override
  public void addPages( )
  {
    setWindowTitle( Messages.getString( "org.kalypso.ui.wizards.imports.elevationModel.Elevation.0" ) ); //$NON-NLS-1$
    mPage = new ElevationMainPage();
    addPage( mPage );
    mPage.init( initialSelection );
  }

  @Override
  public boolean performFinish( )
  {
    try
    {
      final IPath sourcePath = mPage.getSourceLocation();// .toOSString();
      final String setFileName = mPage.getNameForFile();
      final String setFileDescription = mPage.getDescriptionForFileArea();
      final String defaultText = "" ; //$NON-NLS-1$
      final String replaceText = Messages.getString( "org.kalypso.ui.wizards.imports.elevationModel.Elevation.10" ); //$NON-NLS-1$
      final String selectedCoordinateSystem = mPage.getCoordinateSystem();

      getContainer().run( true, true, new IRunnableWithProgress()
      {
        public void run( final IProgressMonitor monitor ) throws InvocationTargetException, IllegalArgumentException
        {
          try
          {
            ITerrainElevationModelSystem temSys = ImportElevationWizard.this.terrainModel.getTerrainElevationModelSystem();
            if( temSys == null )
            {
              temSys = new TerrainElevationModelSystem( ImportElevationWizard.this.terrainModel );
            }

            final GMLWorkspace workspace = temSys.getFeature().getWorkspace();

            // Decoding the White Spaces present in the File Paths.
            final File modelFolderFile = getUTF_DecodedFile( new File( FileLocator.toFileURL( workspace.getContext() ).getFile() ).getParentFile() );
            final File temFolderFile = getUTF_DecodedFile( new File( FileLocator.toFileURL( temFolder.getLocationURI().toURL() ).getFile() ) );
            final File srcFileTif = getUTF_DecodedFile( sourcePath.toFile() );

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
            modelFolder.getProject().refreshLocal( IResource.DEPTH_INFINITE, null/* new NullProgressMonitor() */);
            String nativeTEMRelPath = modelFolderFile.toURI().relativize( new File( URLDecoder.decode( dstFileTif.toString(), "UTF-8" ) ).toURI() ).toString(); //$NON-NLS-1$
            if( nativeTEMRelPath == null )
            {
              nativeTEMRelPath = getUTF_DecodedFile( dstFileTif ).toString();
            }
            final ResourcePool pool = KalypsoGisPlugin.getDefault().getPool();
            final ITerrainElevationModel tem = new NativeTerrainElevationModelWrapper( temSys, nativeTEMRelPath );

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

            final Feature temFeature = tem.getFeature();
            workspace.fireModellEvent( new FeatureStructureChangeModellEvent( workspace, temSys.getFeature(), temFeature, FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD ) );
            workspace.fireModellEvent( new FeatureStructureChangeModellEvent( workspace, temFeature.getParent(), temFeature, FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD ) );
            // TODO check why saving thow pool does not work
            final SzenarioDataProvider caseDataProvider = Util.getCaseDataProvider();
            caseDataProvider.postCommand( ITerrainModel.class, new AddTerrainelevationModelCmd() );

            caseDataProvider.saveModel( ITerrainModel.class, null );

            // pool.saveObject( workspace, new SubProgressMonitor( monitor, 1 ) );
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
    // int i = 1;
    // if (!fileNumbersMap.containsKey(srcFileTif.getName()))
    // fileNumbersMap.put( srcFileTif.getName(),i+"");
    // else
    // {
    // i = Integer.valueOf( fileNumbersMap.get( srcFileTif.getName() ) ).intValue();
    // fileNumbersMap.put( srcFileTif.getName(), (i++)+"");
    // }
    final Random generator = new Random( 126545 );
    final int key = generator.nextInt();
    System.out.println( "key :" + key ); //$NON-NLS-1$

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

  boolean copy( final File src, final File dst, final IProgressMonitor monitor2 )
  {
    InputStream in;
    OutputStream out;
    try
    {
      in = new FileInputStream( src );
      if( !dst.exists() )
      {
        if( dst.createNewFile() )
        {
          // ok
        }
        else
        {
          throw new IOException( Messages.getString("org.kalypso.ui.wizards.imports.elevationmodel.ImportElevationWizard.11") + dst ); //$NON-NLS-1$
        }
      }
      else
      {
        // may be shows some message to the user
      }
      out = new FileOutputStream( dst );

      final byte[] buf = new byte[1024];
      int len;
      final int lens = ((int) src.length() / 1024 + 1);
      monitor2.beginTask( Messages.getString("org.kalypso.ui.wizards.imports.elevationmodel.ImportElevationWizard.0"), lens ); //$NON-NLS-1$
      while( (len = in.read( buf )) > 0 )
      {
        monitor2.worked( 1 );
        // monitor2.wait( 1000 );
        out.write( buf, 0, len );
      }
      monitor2.done();
      in.close();
      out.close();
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
    return mPage.getSourceLocation();
  }

  public String getFileDescription( )
  {
    return mPage.getDescriptionForFileArea();
  }

  public String getFileUserName( )
  {
    return mPage.getNameForFile();
  }

  private final IPoolListener getPoolListener( )
  {
    return new IPoolListener()
    {

      public void dirtyChanged( final IPoolableObjectType key, final boolean isDirty )
      {
        // TODO Auto-generated method stub

      }

      public boolean isDisposed( )
      {
        // TODO Auto-generated method stub
        return false;
      }

      public void objectInvalid( final IPoolableObjectType key, final Object oldValue )
      {
        // TODO Auto-generated method stub

      }

      public void objectLoaded( final IPoolableObjectType key, final Object newValue, final IStatus status )
      {
        // TODO Auto-generated method stub

      }

    };
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
