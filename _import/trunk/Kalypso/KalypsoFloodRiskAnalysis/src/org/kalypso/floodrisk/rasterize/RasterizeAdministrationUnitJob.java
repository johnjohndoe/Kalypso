package org.kalypso.floodrisk.rasterize;

import java.io.File;
import java.net.URL;
import java.util.Hashtable;
import java.util.List;

import org.kalypso.floodrisk.data.ContextModel;
import org.kalypso.floodrisk.data.RasterDataModel;
import org.kalypso.floodrisk.process.IProcessResultEater;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.services.calculation.job.ICalcDataProvider;
import org.kalypso.services.calculation.job.ICalcJob;
import org.kalypso.services.calculation.job.ICalcMonitor;
import org.kalypso.services.calculation.job.ICalcResultEater;
import org.kalypso.services.calculation.service.CalcJobClientBean;
import org.kalypso.services.calculation.service.CalcJobServiceException;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.cv.RectifiedGridCoverage;
import org.kalypsodeegree_impl.model.feature.FeaturePath;

/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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

/**
 * 
 * RasterizeAdministrationUnitJob
 * <p>
 * Job for rastering the administration vector data
 * 
 * created by
 * 
 * @author Nadja Peiler (15.06.2005)
 */
public class RasterizeAdministrationUnitJob implements ICalcJob
{

  //IDs
  //input
  public static final String AdministrationUnitDataID = "AdministrationUnitVectorData";

  public static final String ContextModelID = "ContextModel";

  public static final String BaseRasterID = "BaseRaster";

  //output
  public static final String AdministrationUnitRasterDataID = "AdministrationUnitRasterData";

  public RasterizeAdministrationUnitJob()
  {
    super();

  }

  /**
   * @see org.kalypso.services.calculation.job.ICalcJob#run(java.io.File,
   *      org.kalypso.services.calculation.job.ICalcDataProvider, org.kalypso.services.calculation.job.ICalcResultEater,
   *      org.kalypso.services.calculation.job.ICalcMonitor)
   */
  public void run( File tmpdir, ICalcDataProvider inputProvider, ICalcResultEater resultEater, ICalcMonitor monitor )
      throws CalcJobServiceException
  {
    try
    {
      monitor.setMessage( "Lese Eingabedateien" );

      //administrationUnitVectorData: featureList
      URL administrationUnitVectorDataGML = inputProvider.getURLForID( AdministrationUnitDataID );
      GMLWorkspace administrationUnitVectorData;
      administrationUnitVectorData = GmlSerializer.createGMLWorkspace( administrationUnitVectorDataGML );
      FeaturePath featureMember = new FeaturePath( "FeatureMember" );
      List featureList = (List)featureMember.getFeature( administrationUnitVectorData );

      //contextModel: administrationUnitTypeList
      URL contextModelGML = inputProvider.getURLForID( ContextModelID );
      Hashtable administrationUnitTypeList;
      ContextModel contextModel = new ContextModel( contextModelGML );
      administrationUnitTypeList = contextModel.getAdministrationUnitList();

      //baseRaster
      URL baseRasterGML = inputProvider.getURLForID( BaseRasterID );
      RasterDataModel rasterDataModel = new RasterDataModel();
      RectifiedGridCoverage baseRaster = rasterDataModel.getRectifiedGridCoverage( baseRasterGML );

      monitor.setMessage( "Berechne" );
      RectifiedGridCoverage resultGrid = VectorToGridConverter.toGrid( featureList, administrationUnitTypeList,
          baseRaster, monitor );

      CalcJobClientBean outputBean = (CalcJobClientBean)( (IProcessResultEater)resultEater ).getOutputMap().get(
          AdministrationUnitRasterDataID );
      File resultFile = new File( outputBean.getPath() );
      if( !resultFile.exists() )
        resultFile.createNewFile();
      monitor.setMessage( "Schreibe Ausgabedateien" );
      rasterDataModel.toFile( resultFile, resultGrid );
      resultEater.addResult( outputBean.getId(), null );
    }
    catch( Exception e1 )
    {
      throw new CalcJobServiceException( e1.getMessage(), e1 );
    }
  }

  /**
   * @see org.kalypso.services.calculation.job.ICalcJob#getSpezifikation()
   */
  public URL getSpezifikation()
  {
    return getClass().getResource( "resources/rasterAdminUnitCalcjob_spec.xml" );
  }

}