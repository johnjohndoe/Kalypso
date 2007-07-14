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
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.xml.datatype.XMLGregorianCalendar;

import org.apache.commons.io.IOUtils;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModel1D2D;
import org.kalypso.kalypsomodel1d2d.sim.RMA10Calculation;
import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.kalypso.kalypsosimulationmodel.core.IFeatureWrapperCollection;
import org.kalypso.kalypsosimulationmodel.core.Util;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;

import de.renew.workflow.connector.cases.ICaseDataProvider;

/**
 * 
 * @author Patrice Congo
 */
public class ResultDB
{

  /**
   * File name of the result meta data
   */
  public static final String META_DATA_FILE_NAME = "result_meta_data.gml";

  private GMLWorkspace m_workspace;

  private ISimulationDescriptionCollection m_resultDB;

  private File m_metaDataFile;

  private static final Logger logger = Logger.getLogger( ResultDB.class.getName() );

  public ResultDB( ISimulationDescriptionCollection resultDB )
  {
    Assert.throwIAEOnNullParam( resultDB, "resultDB" );
    this.m_resultDB = resultDB;
  }

  public ResultDB( IPath folderPath )
  {
    m_metaDataFile = folderPath.append( META_DATA_FILE_NAME ).toFile();
    Feature rootFeature = null;
    boolean fileExists = m_metaDataFile.exists();
    if( !fileExists )
    {
      try
      {
        createMetaDataFile();
      }
      catch( Exception e1 )
      {
        throw new RuntimeException( "Result database cannot be created. Check filesystem access rights.", e1 );
      }
    }
    try
    {
      rootFeature = m_workspace.getRootFeature();
    }
    catch( Exception e )
    {
      // the file is probably corrupted, so we will re-create it first...
      logger.log( Level.WARNING, "General result database is corrupted and will be re-created.");
      try
      {
        // TODO: create a copy of the existing database (or rename it) and store it in the same folder with another extension 
        m_metaDataFile.delete();
        createMetaDataFile();
      }
      catch( Exception e2 )
      {
        throw new RuntimeException( "Result database is corrupted and cannot be re-created. Check filesystem access rights.", e2 );
      }
      rootFeature = m_workspace.getRootFeature();
    }
    m_resultDB = new SimulationDescriptionCollection( rootFeature );
  }

  /**
   * Creates metadata gml file. If file allready exists, it will be overwritten
   */
  private void createMetaDataFile( ) throws Exception
  {
    m_metaDataFile.createNewFile();
    ResourcesPlugin.getWorkspace().getRoot().refreshLocal( IResource.DEPTH_INFINITE, null );
    m_workspace = FeatureFactory.createGMLWorkspace( Kalypso1D2DSchemaConstants.SIMMETA_F_SIMDESCRIPTOR_COLLECTION, m_metaDataFile.toURL(), GmlSerializer.DEFAULT_FACTORY );
    final OutputStreamWriter outStreamWriter = new OutputStreamWriter( new FileOutputStream( m_metaDataFile ) );
    try
    {
      GmlSerializer.serializeWorkspace( outStreamWriter, m_workspace );
    }
    finally
    {
      IOUtils.closeQuietly( outStreamWriter );
    }
  }

  public static final IPath getFolder( )
  {
    KalypsoModel1D2DPlugin modelPlugin = KalypsoModel1D2DPlugin.getDefault();
    IPath stateLocation = modelPlugin.getStateLocation();
    return stateLocation;
  }

  public IFeatureWrapperCollection<ISimulationDescriptor> getSimulationDescriptors( )
  {
    return m_resultDB.getSimulationDescriptors();
  }

  public IFeatureWrapperCollection<IModelDescriptor> getModelDescriptors( )
  {
    return m_resultDB.getModelDescriptors();
  }

  public ISimulationDescriptionCollection getSimDB( )
  {
    return m_resultDB;
  }

  /**
   * @param modelFeatureWrapper
   * @return
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.metadata.ISimulationDescriptionCollection#addModelDescriptor(org.kalypsodeegree.model.feature.binding.IFeatureWrapper2)
   */
  public IModelDescriptor addModelDescriptor( IFeatureWrapper2 modelFeatureWrapper )
  {
    IModelDescriptor existingEntry = m_resultDB.getExistingEntry( modelFeatureWrapper );
    if( existingEntry == null )
    {
      existingEntry = m_resultDB.addModelDescriptor( modelFeatureWrapper );
    }
    return existingEntry;
  }

  public void save( )
  {
    try
    {
      URL context = m_workspace.getContext();
      File file = new File( context.getFile() );

      OutputStreamWriter outStreamWriter = new OutputStreamWriter( new FileOutputStream( file ) );
      GmlSerializer.serializeWorkspace( outStreamWriter, m_workspace );
      outStreamWriter.close();
    }
    catch( Exception e )
    {
      e.printStackTrace();
      throw new RuntimeException( e );
    }

  }

  /**
   * To get the simulation descriptor for the given rma10 calculation
   * 
   * @param rma10Calculation
   *            the rma calculation which simulation descriptor is to be retrieved
   * @return an {@link ISimulationDescriptor} for the given rma10s calculation or null is this {@link ResultDB} does not
   *         hold such a descriptor
   * 
   */
  public ISimulationDescriptor getSimulationDescriptorFor( RMA10Calculation rma10Calculation )
  {
// IControlModel1D2D controlModel = rma10Calculation.getControlModel();
    ICalculationUnit calcultionUnit = rma10Calculation.getCalculationUnit();
    IFeatureWrapperCollection<ISimulationDescriptor> simDescs = getSimulationDescriptors();
    for( ISimulationDescriptor sd : simDescs )
    {
      IModelDescriptor calUnitDesc = sd.getCalculationUnit();
      if( calUnitDesc != null )
      {
        if( calUnitDesc.isDescribing( calcultionUnit ) )
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
// ITimeStepinfo[] timeStepInfos = rma10Calculation.getTimeStepInfos();

    IModelDescriptor controlModelDesc = addModelDescriptor( controlModel );
    IModelDescriptor calUnitDescr = addModelDescriptor( calcultionUnit );
    simDesc.setAutoconverged( false );// TODO
    simDesc.setRestarted( controlModel.getRestart() );
    simDesc.setCalculationUnit( calUnitDescr );
    simDesc.setControlModel( controlModelDesc );
    // start time
    XMLGregorianCalendar startCalendar = controlModel.getStartCalendar();
    if( startCalendar != null )
    {
      simDesc.setStartTime( startCalendar.toGregorianCalendar() );
    }
    else
    {
      simDesc.setStartTime( null );
    }
    simDesc.setEndTime( null );// TODO
    simDesc.setSimulationType( null );// TODO

    return simDesc;
  }

  /**
   * To get the current scenario/project result db.
   * 
   * @return the current result db
   * @throws CoreException
   *             rethrow from {@link IResultDbProvider#getResultDB()}
   */
  public static final ResultDB getCurrentResultDB( ) throws CoreException
  {
    ICaseDataProvider<IFeatureWrapper2> scenarioDataProvider = Util.getScenarioDataProvider();
    if( scenarioDataProvider instanceof IResultDbProvider )
    {
      return ((IResultDbProvider) scenarioDataProvider).getResultDB();
    }
    else
    {
      return null;
    }
  }

}
