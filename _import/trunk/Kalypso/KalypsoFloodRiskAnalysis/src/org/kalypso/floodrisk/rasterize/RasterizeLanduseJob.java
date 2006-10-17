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
package org.kalypso.floodrisk.rasterize;

import java.io.File;
import java.net.URL;
import java.util.Hashtable;
import java.util.List;

import org.kalypso.floodrisk.data.ContextModel;
import org.kalypso.floodrisk.data.RasterDataModel;
import org.kalypso.floodrisk.internationalize.Messages;
import org.kalypso.floodrisk.process.IProcessResultEater;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.simulation.core.ISimulation;
import org.kalypso.simulation.core.ISimulationDataProvider;
import org.kalypso.simulation.core.ISimulationMonitor;
import org.kalypso.simulation.core.ISimulationResultEater;
import org.kalypso.simulation.core.SimulationDataPath;
import org.kalypso.simulation.core.SimulationException;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.cv.RectifiedGridCoverage;
import org.kalypsodeegree_impl.model.feature.FeaturePath;

/**
 * RasterizeLanduseJob
 * <p>
 * Job for rastering the landuse vector data created by
 * 
 * @author Nadja Peiler (15.06.2005)
 */
public class RasterizeLanduseJob implements ISimulation
{

  // IDs
  // input
  public static final String LanduseVectorDataID = "LanduseVectorData"; //$NON-NLS-1$

  public static final String ContextModelID = "ContextModel"; //$NON-NLS-1$

  public static final String BaseRasterID = "BaseRaster"; //$NON-NLS-1$

  // output
  public static final String LanduseRasterDataID = "LanduseRasterData"; //$NON-NLS-1$

  public RasterizeLanduseJob( )
  {
    super();

  }

  /**
   * @see org.kalypso.services.calculation.job.ICalcJob#run(java.io.File,
   *      org.kalypso.services.calculation.job.ICalcDataProvider, org.kalypso.services.calculation.job.ICalcResultEater,
   *      org.kalypso.services.calculation.job.ICalcMonitor)
   */
  public void run( File tmpdir, ISimulationDataProvider inputProvider, ISimulationResultEater resultEater, ISimulationMonitor monitor ) throws SimulationException
  {
    try
    {
      monitor.setMessage( Messages.getString("rasterize.RasterizeLanduseJob.LoadingInputData") ); //$NON-NLS-1$

      // landuseVectorData: featureList
      URL landuseVectorDataGML = (URL) inputProvider.getInputForID( LanduseVectorDataID );
      GMLWorkspace landuseVectorData;
      landuseVectorData = GmlSerializer.createGMLWorkspace( landuseVectorDataGML, null );
      FeaturePath featureMember = new FeaturePath( "FeatureMember" ); //$NON-NLS-1$
      List featureList = (List) featureMember.getFeature( landuseVectorData );

      // contextModel: landuseTypeList
      URL contextModelGML = (URL) inputProvider.getInputForID( ContextModelID );
      Hashtable landuseTypeList;
      ContextModel contextModel = new ContextModel( contextModelGML );
      landuseTypeList = contextModel.getLanduseList();

      // baseRaster
      URL baseRasterGML = (URL) inputProvider.getInputForID( BaseRasterID );
      RasterDataModel rasterDataModel = new RasterDataModel();
      RectifiedGridCoverage baseRaster = rasterDataModel.getRectifiedGridCoverage( baseRasterGML );

      monitor.setMessage( Messages.getString("rasterize.RasterizeLanduseJob.Calculating") ); //$NON-NLS-1$
      RectifiedGridCoverage resultGrid = VectorToGridConverter.toGrid( featureList, landuseTypeList, baseRaster, monitor );

      SimulationDataPath outputBean = (SimulationDataPath) ((IProcessResultEater) resultEater).getOutputMap().get( LanduseRasterDataID );
      File resultFile = new File( outputBean.getPath() );
      if( !resultFile.exists() )
        resultFile.createNewFile();
      monitor.setMessage( Messages.getString("rasterize.RasterizeLanduseJob.SavingResults") ); //$NON-NLS-1$
      rasterDataModel.toFile( resultFile, resultGrid );
      resultEater.addResult( outputBean.getId(), null );
    }
    catch( Exception e1 )
    {
      throw new SimulationException( e1.getMessage(), e1 );
    }
  }

  // /**
  // * returns a list of Features for a given shapeFile wird nicht mehr verwendet
  // *
  // * @param shapeFileBase
  // * (base of shape)
  // *
  // * @return List of Features
  // */
  // private List getFeatureList( String shapeFileBase, CS_CoordinateSystem cs )
  // {
  // try
  // {
  // GMLWorkspace workspace = ShapeSerializer.deserialize( shapeFileBase, cs );
  // Feature root = workspace.getRootFeature();
  // List featureList = (List)root.getProperty( "featureMember" );
  // return featureList;
  // }
  // catch( Exception e )
  // {
  // System.out.println( e );
  // return null;
  // }
  // }

  /**
   * @see org.kalypso.services.calculation.job.ICalcJob#getSpezifikation()
   */
  public URL getSpezifikation( )
  {
    return getClass().getResource( "resources/rasterLanduseCalcjob_spec.xml" ); //$NON-NLS-1$
  }

}