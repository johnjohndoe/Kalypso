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
import java.io.OutputStreamWriter;
import java.net.URL;
import java.util.GregorianCalendar;

import javax.xml.datatype.XMLGregorianCalendar;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IPath;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.conv.ITimeStepinfo;
import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModel1D2D;
import org.kalypso.kalypsomodel1d2d.sim.RMA10Calculation;
import org.kalypso.kalypsosimulationmodel.core.IFeatureWrapperCollection;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;

/**
 * 
 * @author Patrice Congo
 */
@SuppressWarnings({"unchecked","hiding"})
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
   Feature rootFeature = workspace.getRootFeature();
  simDB =
     new SimulationDescriptionCollection(rootFeature);
   System.out.println("\n\tsimdB created="+ simDB);
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
    
    IModelDescriptor existingEntry = simDB.getExistingEntry( modelFeatureWrapper );
    if( existingEntry == null )
    {
      existingEntry = simDB.addModelDescriptor( modelFeatureWrapper ); 
    }
    
    return existingEntry; 
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
  
  /**
   * To get the simulation descriptor for the given rma10 calculation
   * @param rma10Calculation the rma calculation which simulation descriptor 
   *            is to be retrieved
   * @return an {@link ISimulationDescriptor} for the given rma10s calculation or 
   *            null is this {@link ResultDB} does not hold such a descriptor
   * 
   */
  public ISimulationDescriptor getSimulationDescriptorFor(RMA10Calculation rma10Calculation )
  {
//    IControlModel1D2D controlModel = rma10Calculation.getControlModel();
    ICalculationUnit calcultionUnit = rma10Calculation.getCalculationUnit();
    IFeatureWrapperCollection<ISimulationDescriptor> simDescs = getSimulationDescriptors();
    for(ISimulationDescriptor sd:simDescs)
    {
      IModelDescriptor calUnitDesc = sd.getCalculationUnit();
      if( calUnitDesc != null )
      {
        if( calUnitDesc.isDescribing( calcultionUnit ))
        {
          return sd;
        }
      }
    }
    return null;
  }
  
  public ISimulationDescriptor addRMACalculation( RMA10Calculation rma10Calculation )
  {
    ISimulationDescriptor simDesc = getSimulationDescriptorFor( rma10Calculation );
    if( simDesc == null )
    {
      IFeatureWrapperCollection<ISimulationDescriptor> simDescs = getSimulationDescriptors();
      simDesc = simDescs.addNew( Kalypso1D2DSchemaConstants.SIMMETA_F_SIMDESCRIPTOR );
    }
    
    IControlModel1D2D controlModel = rma10Calculation.getControlModel();
    ICalculationUnit calcultionUnit = rma10Calculation.getCalculationUnit();
//    ITimeStepinfo[] timeStepInfos = rma10Calculation.getTimeStepInfos();
    
    IModelDescriptor controlModelDesc = addModelDescriptor( controlModel );
    IModelDescriptor calUnitDescr = addModelDescriptor( calcultionUnit );
    simDesc.setAutoconverged( false );//TODO
    simDesc.setRestarted( controlModel.getRestart() );
    simDesc.setCalculationUnit( calUnitDescr );
    simDesc.setControlModel( controlModelDesc );
    //start time
    XMLGregorianCalendar startCalendar = controlModel.getStartCalendar();
    if( startCalendar !=  null )
    {
      simDesc.setStartTime( startCalendar.toGregorianCalendar() );
    }
    else
    {
      simDesc.setStartTime( null );
    }
    simDesc.setEndTime( null );//TODO
    simDesc.setSimulationType( null );//TODO
    
    return simDesc;
  }
  
}
