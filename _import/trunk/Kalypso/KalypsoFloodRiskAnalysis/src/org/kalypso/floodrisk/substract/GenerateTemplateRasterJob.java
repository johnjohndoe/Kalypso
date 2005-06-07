package org.kalypso.floodrisk.substract;

import java.io.File;
import java.net.MalformedURLException;
import java.net.URL;

import org.kalypso.floodrisk.data.RasterDataModel;
import org.kalypso.floodrisk.process.IProcessResultEater;
import org.kalypso.services.calculation.job.ICalcDataProvider;
import org.kalypso.services.calculation.job.ICalcJob;
import org.kalypso.services.calculation.job.ICalcMonitor;
import org.kalypso.services.calculation.job.ICalcResultEater;
import org.kalypso.services.calculation.service.CalcJobClientBean;
import org.kalypso.services.calculation.service.CalcJobServiceException;
import org.kalypsodeegree_impl.model.cv.RectifiedGridCoverage;

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

public class GenerateTemplateRasterJob implements ICalcJob
{

  //IDs
  //input
  public static final String Raster1ID = "Raster1";

  public static final String Raster2ID = "Raster2";

  //output
  public static final String TemplateRasterID = "TemplateRaster";

  RasterDataModel rasterDataModel = new RasterDataModel();

  /**
   * 
   * @see org.kalypso.services.calculation.job.ICalcJob#run(java.io.File,
   *      org.kalypso.services.calculation.job.ICalcDataProvider,
   *      org.kalypso.services.calculation.job.ICalcResultEater,
   *      org.kalypso.services.calculation.job.ICalcMonitor)
   */
  public void run( File tmpdir, ICalcDataProvider inputProvider,
      ICalcResultEater resultEater, ICalcMonitor monitor )
      throws CalcJobServiceException
  {
    try
    {
      //Generate input
      //Raster1
      URL raster1GML = inputProvider.getURLForID( Raster1ID );
      RectifiedGridCoverage raster1 = rasterDataModel
          .getRectifiedGridCoverage( raster1GML );

      //Raster2
      URL raster2GML = inputProvider.getURLForID( Raster2ID );
      RectifiedGridCoverage raster2 = rasterDataModel
          .getRectifiedGridCoverage( raster2GML );

      //Calculation
      //substract Grids
      RectifiedGridCoverage templateRaster = RasterTools.substractGrids(
          raster1, raster2 );

      //Generate output
      //template raster
      CalcJobClientBean templateRasterOutputBean = (CalcJobClientBean)( (IProcessResultEater)resultEater )
          .getOutputMap().get( TemplateRasterID );
      File templateRasterFile = new File( templateRasterOutputBean.getPath() );
      if( !templateRasterFile.exists() )
        templateRasterFile.createNewFile();
      rasterDataModel.toFile( templateRasterFile, templateRaster );
      resultEater.addResult( templateRasterOutputBean.getId(), null );
    }
    catch( MalformedURLException e )
    {
      throw new CalcJobServiceException(
          "CalculateDamageJob Service Exception: Malformed URL", e );
    }
    catch( Exception e )
    {
      throw new CalcJobServiceException(
          "CalculateDamageJob Service Exception", e );
    }
  }

  /**
   * 
   * @see org.kalypso.services.calculation.job.ICalcJob#getSpezifikation()
   */
  public URL getSpezifikation()
  {
    return null;
  }

}