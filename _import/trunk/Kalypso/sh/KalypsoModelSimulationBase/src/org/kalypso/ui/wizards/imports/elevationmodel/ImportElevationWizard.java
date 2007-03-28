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
import java.util.Iterator;
import java.net.*;

import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.IWorkbench;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.ITerrainElevationModel;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.ITerrainElevationModelSystem;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.ITerrainModel;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.NativeTerrainElevationModelWrapper;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.TerrainElevationModelSystem;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.ui.wizards.imports.Messages;
import org.kalypso.util.pool.IPoolListener;
import org.kalypso.util.pool.IPoolableObjectType;
import org.kalypso.util.pool.ResourcePool;
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

  private static int FILENUMBER = 1;

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

  private CommandableWorkspace cmdWorkspace;

  /**
   * Construct a new instance and initialize the dialog settings for this instance.
   */
  public ImportElevationWizard( )
  {
  }

  /**
   * The required selection structure is:
   * <ul>
   * <li/>Length=2 <li/>First element an instance of {@link ITerrainModel} <li/>Second element an instance of
   * {@link IFolder} <li/>third element an instance of {@link CommandableWorkspace}
   * </ul>
   * 
   * @param workbench
   *          the current workbench
   * @param selection
   *          the current object selection
   */
  public void init( IWorkbench workbench, IStructuredSelection selection )
  {
    initialSelection = selection;
    Iterator selIterator = selection.iterator();
    terrainModel = (ITerrainModel) selIterator.next();
    modelFolder = (IFolder) selIterator.next();
    temFolder = (IFolder) selIterator.next();
    cmdWorkspace = (CommandableWorkspace) selIterator.next();
  }

  @Override
  public void addPages( )
  {
    setWindowTitle( Messages.getString( "org.kalypso.ui.wizards.imports.elevationModel.Elevation.0" ) );
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
      final String defaultText = Messages.getString( "org.kalypso.ui.wizards.imports.baseMap.BaseMapMainPage.9" );
      final String replaceText = Messages.getString( "org.kalypso.ui.wizards.imports.baseMap.BaseMapMainPage.10" );

      getContainer().run( true, true, new IRunnableWithProgress()
      {
        public void run( IProgressMonitor monitor ) throws InvocationTargetException, InterruptedException, IllegalArgumentException
        {
          try
          {
            ITerrainElevationModelSystem temSys = ImportElevationWizard.this.terrainModel.getTerrainElevationModelSystem();
            if( temSys == null )
            {
              temSys = new TerrainElevationModelSystem( ImportElevationWizard.this.terrainModel );
            }

            GMLWorkspace workspace = temSys.getWrappedFeature().getWorkspace();
            
            // Decoding the White Spaces present in the File Paths.
            File modelFolderFile_ = new File( FileLocator.toFileURL( workspace.getContext() ).getFile() ).getParentFile();
            File modelFolderFile = new File(URLDecoder.decode(modelFolderFile_.toString(),"UTF-8"));
            
            File temFolderFile_ = new File( FileLocator.toFileURL( temFolder.getLocationURI().toURL() ).getFile() );
            File temFolderFile = new File(URLDecoder.decode(temFolderFile_.toString(),"UTF-8"));
            final File srcFileTif_ = sourcePath.toFile();
            File srcFileTif = new File(URLDecoder.decode(srcFileTif_.toString(),"UTF-8"));
            
            File dstFileTif = null;
            
            if( (new File( temFolderFile, srcFileTif.getName())).exists() )
              dstFileTif = new File( temFolderFile, getFileNameNoExtension( srcFileTif ) + "_" + FILENUMBER + "." + getExtension( srcFileTif ) );
            else
              dstFileTif = new File( temFolderFile, srcFileTif.getName() ); // mPage.getSourceLocation().lastSegment()
            
            copy( srcFileTif, dstFileTif, monitor );
            modelFolder.getProject().refreshLocal( IResource.DEPTH_INFINITE, null/* new NullProgressMonitor() */);
          // String nativeTEMRelPath = modelFolderFile.toURI().relativize( dstFileTif.toURI() ).toString();
            String nativeTEMRelPath = modelFolderFile.toURI().relativize( new File(URLDecoder.decode(dstFileTif.toString(),"UTF-8")).toURI() ).toString();
            if( nativeTEMRelPath == null )
            {
           //   nativeTEMRelPath = dstFileTif.toURL().toString();
            nativeTEMRelPath = new File(URLDecoder.decode(dstFileTif.toString(),"UTF-8")).toString();
              
            }

            ResourcePool pool = KalypsoGisPlugin.getDefault().getPool();
            ITerrainElevationModel tem = new NativeTerrainElevationModelWrapper( temSys, nativeTEMRelPath );

            // TODO introduce in the first page a name imput field and gets the
            // name from there

            String name = dstFileTif.getName();

            if( setFileName.compareTo( "" ) != 0 )
            {
              tem.setName( setFileName );
            }
            else
            {
              tem.setName( name );
            }

            System.out.println( "Stats :" + setFileDescription != defaultText );
            if( setFileDescription.compareTo( defaultText ) != 0 )
            {
              tem.setDescription( setFileDescription );
            }
            else
            {
              tem.setDescription( replaceText );
            }
            System.out.println( "Workspace:" + workspace.getClass() );
            cmdWorkspace.fireModellEvent( new FeatureStructureChangeModellEvent( cmdWorkspace, temSys.getWrappedFeature(), tem.getWrappedFeature(), FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD ) );

            // TODO check why saving thow pool does not work
            pool.saveObject( cmdWorkspace, new SubProgressMonitor( monitor, 1 ) );

          }
          catch( Exception e )
          {
            e.printStackTrace();
            throw new InvocationTargetException( e );
          }

        }
      } );
      FILENUMBER++;
    }
    catch( Throwable th )
    {
      th.printStackTrace();
      return false;
    }

    return true;
  }

  public static String getExtension( File f )
  {
    String ext = null;
    String s = f.getName();
    int i = s.lastIndexOf( '.' );

    if( i > 0 && i < s.length() - 1 )
    {
      ext = s.substring( i + 1 ).toLowerCase();
    }
    return ext;
  }

  public static String getFileNameNoExtension( File f )
  {
    String ext = null;
    String s = f.getName();
    int i = s.lastIndexOf( '.' );

    if( i > 0 && i < s.length() - 1 )
    {
      ext = s.substring( 0, i );
    }
    return ext;
  }

  boolean copy( File src, File dst, IProgressMonitor monitor2 )
  {
    InputStream in;
    OutputStream out;

    
    File dst_ = dst;
    File src_ = src;
//    try
//    {
//      src_ = new File(URLDecoder.decode(src.toString(),"UTF-8"));
//    }
//    catch( UnsupportedEncodingException e2 )
//    {
//      // TODO Auto-generated catch block
//      e2.printStackTrace();
//    }
//    try
//    {
//      dst_ = new File(URLDecoder.decode( dst.toString(), "UTF-8" ));
//    }
//    catch( UnsupportedEncodingException e1 )
//    {
//      // TODO Auto-generated catch block
//      e1.printStackTrace();
//    }
    
    
    try
    {
      in = new FileInputStream( src_ );
      if( !dst_.exists() )
      {
        if( dst_.createNewFile() )
        {
          // ok
        }
        else
        {
          throw new IOException( "Could not create file:" + dst_ );
        }
      }
      else
      {
        // may be shows some message to the user
      }
      out = new FileOutputStream( dst_ );

      byte[] buf = new byte[1024];
      int len;
      int lens = ((int) src_.length() / 1024 + 1);
      monitor2.beginTask( "Copying..", lens );
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
    catch( Exception e )
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

      public void dirtyChanged( IPoolableObjectType key, boolean isDirty )
      {
        // TODO Auto-generated method stub

      }

      public boolean isDisposed( )
      {
        // TODO Auto-generated method stub
        return false;
      }

      public void objectInvalid( IPoolableObjectType key, Object oldValue )
      {
        // TODO Auto-generated method stub

      }

      public void objectLoaded( IPoolableObjectType key, Object newValue, IStatus status )
      {
        // TODO Auto-generated method stub

      }

    };
  }

  // /**
  // * @see org.kalypso.ui.wizards.imports.INewWizardKalypsoImport#initModelProperties(java.util.HashMap)
  // */
  // public void initModelProperties( HashMap<String, Object> map )
  // {
  // m_scenarioFolder = (String) map.get( "ScenarioFolder" );
  // }

}
