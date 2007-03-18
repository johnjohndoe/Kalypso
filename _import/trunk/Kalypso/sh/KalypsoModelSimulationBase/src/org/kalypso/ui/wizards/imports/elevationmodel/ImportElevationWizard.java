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
import java.io.OutputStreamWriter;
import java.lang.reflect.InvocationTargetException;
import java.util.Iterator;

import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
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
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ui.wizards.imports.Messages;
import org.kalypsodeegree.model.feature.GMLWorkspace;


/**
 * @author Madanagopal
 * @author Patrice Congo
 *
 */
public class ImportElevationWizard extends Wizard
      implements INewWizard/*INewWizardKalypsoImport*/
{
   private IStructuredSelection initialSelection;

   private ElevationMainPage mPage;
   
   /**
    * Folder containing the terrain model
    */
   private IFolder modelFolder;
   
   
   private ITerrainModel terrainModel;
   
//  private String m_scenarioFolder;

  /**
   * destination folders for the native terrain elevation model 
   */
  private IFolder temFolder;
   
   /**
    * Construct a new instance and initialize the dialog settings
    * for this instance.
    */
   public ImportElevationWizard() {
   }
   
   /**
    * The required selection structure is:
    * <ul>
    *   <li/>Length=2
    *   <li/>First element an instance of {@link ITerrainModel}
    *   <li/>Second element an instance of {@link IFolder}
    * </ul> 
    * 
    * @param workbench the current workbench
    * @param selection the current object selection
    */
   public void init(IWorkbench workbench, IStructuredSelection selection)
   {
      initialSelection = selection;
      Iterator selIterator=selection.iterator();
      terrainModel = (ITerrainModel) selIterator.next();
      modelFolder = (IFolder)selIterator.next();   
      temFolder = (IFolder)selIterator.next();
   }
   
   @Override
  public void addPages() {
      setWindowTitle(Messages.getString( "org.kalypso.ui.wizards.imports.elevationModel.Elevation.0" ));
      mPage = new ElevationMainPage();
      addPage(mPage);
      mPage.init(initialSelection);
   }

  @Override
  public boolean performFinish( )
  {
    final IPath sourcePath = mPage.getSourceLocation();//.toOSString();
    try {
      getContainer().run(true, true, new IRunnableWithProgress() {
         public void run(IProgressMonitor monitor)
            throws InvocationTargetException, InterruptedException, IllegalArgumentException
         {
           try
           {
             ITerrainElevationModelSystem temSys=
               ImportElevationWizard.this.terrainModel.getTerrainElevationModelSystem();
             if(temSys==null)
             {
               temSys= new TerrainElevationModelSystem(ImportElevationWizard.this.terrainModel);
             }
             
//             File modelFolderFile = 
//               new File(FileLocator.toFileURL( 
//                   modelFolder.getLocationURI().toURL()).getFile());
             
             GMLWorkspace workspace = temSys.getWrappedFeature().getWorkspace();
            File modelFolderFile=
               new File(FileLocator.toFileURL(workspace.getContext()).getFile()).getParentFile();
             File temFolderFile = 
               new File(FileLocator.toFileURL( temFolder.getLocationURI().toURL()).getFile());
             
             final File srcFileTif = sourcePath.toFile();
             final File dstFileTif = 
               new File(temFolderFile, sourcePath.lastSegment() );//new File( dstFilePath + mPage.getSourceLocation().lastSegment() );
             copy(srcFileTif, dstFileTif, monitor);
             modelFolder.getProject().refreshLocal( 
                 IResource.DEPTH_INFINITE, null/*new NullProgressMonitor()*/ );
             
             String nativeTEMRelPath = 
                   modelFolderFile.toURI().relativize( dstFileTif.toURI() ).toString();
             if(nativeTEMRelPath==null)
             {
               nativeTEMRelPath = dstFileTif.toURL().toString();
             }
              ITerrainElevationModel tem =
                     new NativeTerrainElevationModelWrapper(temSys,nativeTEMRelPath);
              
              //TODO introduce in the first page a name imput field and gets the
              //name from there
              
              String name=dstFileTif.getName();
              tem.setName( name );
              
              
              //TODO check why saving thow pool does not work
//              KalypsoGisPlugin.getDefault().getPool().saveObject(
//                                    workspace, new SubProgressMonitor(monitor,1) );
              //save the workspace old method
              File workspaceContextFile = 
                new File(FileLocator.toFileURL( workspace.getContext()).getFile());
              
              OutputStreamWriter outputStreamWriter= 
                    new OutputStreamWriter(
                        new FileOutputStream(workspaceContextFile));
              GmlSerializer.serializeWorkspace( outputStreamWriter, workspace );
              
              
          }
          catch( Exception e )
          {
            e.printStackTrace();
            throw new InvocationTargetException(e);
          }          
           
         }
      });
   }
   catch (Throwable th) 
   {
     th.printStackTrace(); 
     return false;
   }
    
    return true;
  }
  
  boolean copy(File src, File dst, IProgressMonitor monitor2)
  {
    InputStream in;
    OutputStream out;
    try
    {
      in = new FileInputStream( src );
      if(!dst.exists())
      {
        if(dst.createNewFile())
        {
          //ok
        }
        else
        {
          throw new IOException("Could not create file:"+dst);
        }
      }
      else
      {
        //may be shows some message to the user
      }
      out = new FileOutputStream( dst );

      byte[] buf = new byte[1024];
      int len;
      int lens = ((int)src.length()/1024+1);
      monitor2.beginTask( "Copying..", lens );
      while( (len = in.read( buf )) > 0 )
      {
        monitor2.worked(1);
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
   public IPath getSourceLocation() {
      return mPage.getSourceLocation();
   }

//  /**
//   * @see org.kalypso.ui.wizards.imports.INewWizardKalypsoImport#initModelProperties(java.util.HashMap)
//   */
//  public void initModelProperties( HashMap<String, Object> map )
//  {    
//    m_scenarioFolder = (String) map.get( "ScenarioFolder" );    
//  }

}
