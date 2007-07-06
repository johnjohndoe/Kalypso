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
package org.kalypso.kalypsomodel1d2d.schema.binding.metadata;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.net.URL;

import org.apache.commons.io.IOUtils;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.IPath;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsosimulationmodel.core.IFeatureWrapperCollection;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;

import test.org.kalypso.kalypsomodel1d2d.TestWorkspaces;

/**
 * 
 * @author Patrice Congo
 */
public class ResultDB
{
  
  /**
   * File name of the result meta data
   */
  public static final String META_DATA_FILE_NAME ="result_meta_data.gml";
  private GMLWorkspace workspace;
  private ISimulationDescriptionCollection simDB;
   
  public ResultDB(IPath folderPath)
  {
   IPath metaDataFile = 
       folderPath.append( META_DATA_FILE_NAME );
   boolean exists =
     ResourcesPlugin.getWorkspace().getRoot().exists( metaDataFile );
   System.out.println(metaDataFile.toFile()+"\n"+exists);
   
   createMetaDataFile( metaDataFile );
   simDB =
     (ISimulationDescriptionCollection)workspace.getRootFeature().getAdapter( 
                           ISimulationDescriptionCollection.class );
   System.out.println(workspace);
  }
  
  private void createMetaDataFile( IPath metaDataPath )
  {
    try
    {
      File file = metaDataPath.toFile();
      IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();
      root.refreshLocal( IResource.DEPTH_INFINITE, null );
      IFile metaDataFile = root.getFile( metaDataPath );
      if( !file.exists() )
      {
        if( !file.createNewFile() )
        {
          throw new RuntimeException("Could not create file:"+file);
        }
//        URL resource = ResultDB.class.getResource( "result_db.xml" );
//        
//        IOUtils.copy( 
//            resource.openStream(), 
//            new FileOutputStream(file) );
        

      workspace =
      FeatureFactory.createGMLWorkspace(
          Kalypso1D2DSchemaConstants.SIMMETA_F_SIMDESCRIPTOR_COLLECTION, 
          file.toURL(), 
          GmlSerializer.DEFAULT_FACTORY );
      OutputStreamWriter outStreanWriter = 
        new OutputStreamWriter(new FileOutputStream(file));
      GmlSerializer.serializeWorkspace( outStreanWriter, workspace );
      outStreanWriter.close();
        return;
      }
      else
      {
        workspace =
          GmlSerializer.createGMLWorkspace( 
            file.toURL(), 
            null );
      }
    }
    catch (Exception e) {
      e.printStackTrace();
      throw new RuntimeException("Cannot create meta gml");
    }
  }
//  private void createMetaDataFile_old( IPath metaDataPath )
//  {
//    try
//    {
//      File file = metaDataPath.toFile();
//      IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();
//      root.refreshLocal( IResource.DEPTH_INFINITE, null );
//      IFile metaDataFile = root.getFile( metaDataPath );
//      if( !file.exists() )
//      {
//        if( !file.createNewFile() )
//        {
//          throw new RuntimeException("Could not create file:"+file);
//        }
//        URL resource = ResultDB.class.getResource( "result_db.xml" );
//        
//        IOUtils.copy( 
//            resource.openStream(), 
//            new FileOutputStream(file) );
//      }
//        workspace =
//          GmlSerializer.createGMLWorkspace( 
//            file.toURL(), 
//            null );
//
//    }
//    catch (Exception e) {
//      e.printStackTrace();
//      throw new RuntimeException("Cannot create meta gml");
//    }
//  }
  
  public static final IPath getFolder()
  {
    KalypsoModel1D2DPlugin modelPlugin = 
            KalypsoModel1D2DPlugin.getDefault();
    IPath stateLocation = modelPlugin.getStateLocation();
    return stateLocation;
  }
  
  public IFeatureWrapperCollection<ISimulationDescriptor> getSimulationDescriptors()
  {
    return simDB.getSimulationDescriptors();
  }
  
  public IFeatureWrapperCollection<IModelDescriptor> getModelDescriptors()
  {
   return simDB.getModelDescriptors(); 
  }
  
  public ISimulationDescriptionCollection getSimDB( )
  {
    return simDB;
  }

  /**
   * @param modelFeatureWrapper
   * @return
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.metadata.ISimulationDescriptionCollection#addModelDescriptor(org.kalypsodeegree.model.feature.binding.IFeatureWrapper2)
   */
  public IModelDescriptor addModelDescriptor( IFeatureWrapper2 modelFeatureWrapper )
  {
    return simDB.addModelDescriptor( modelFeatureWrapper );
  }
  
  public void save()
  {
    try
    {
      URL context = workspace.getContext();
      File file = new File(context.getFile());
      
      OutputStreamWriter outStreamWriter=
        new OutputStreamWriter(new FileOutputStream(file));
      GmlSerializer.serializeWorkspace( outStreamWriter, workspace );
      outStreamWriter.close();
    }
    catch (Exception e) {
      e.printStackTrace();
      throw new RuntimeException(e);
    }

  }
}
