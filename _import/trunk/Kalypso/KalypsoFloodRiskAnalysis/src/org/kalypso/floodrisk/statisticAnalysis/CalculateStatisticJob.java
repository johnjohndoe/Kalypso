package org.kalypso.floodrisk.statisticAnalysis;

import java.io.File;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.Hashtable;

import org.kalypso.floodrisk.data.ContextModel;
import org.kalypso.floodrisk.data.RasterDataModel;
import org.kalypso.floodrisk.process.IProcessDataProvider;
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

public class CalculateStatisticJob implements ICalcJob
{

  //IDs
  //input
  public static final String DamageRasterID = "DamageRaster";

  public static final String LanduseRasterDataID = "LanduseRasterData";

  public static final String ContextModelID = "ContextModel";

  //optional
  public static final String TemplateRasterID = "TemplateRaster";

  //output
  public static final String StatisticDataID = "StatisticData";

  //schemas
  private URL statisticData_schemaURL = getClass().getResource( "../schema/StatisticData.xsd" );

  RasterDataModel rasterDataModel = new RasterDataModel();

  /**
   * 
   * @see org.kalypso.services.calculation.job.ICalcJob#run(java.io.File,
   *      org.kalypso.services.calculation.job.ICalcDataProvider,
   *      org.kalypso.services.calculation.job.ICalcResultEater,
   *      org.kalypso.services.calculation.job.ICalcMonitor)
   */
  public void run( File tmpdir, ICalcDataProvider inputProvider, ICalcResultEater resultEater,
      ICalcMonitor monitor ) throws CalcJobServiceException
  {
    try
    {
      //Generate input
      //damageRaster
      File damageRasterGML = new File( (String)( (IProcessDataProvider)inputProvider )
          .getObjectForID( DamageRasterID ) );
      RectifiedGridCoverage damageRaster = rasterDataModel
          .getRectifiedGridCoverage( damageRasterGML.toURL() );

      //landuseRaster
      File landuseRasterGML = new File( (String)( (IProcessDataProvider)inputProvider )
          .getObjectForID( LanduseRasterDataID ) );
      RectifiedGridCoverage landuseRaster = rasterDataModel
          .getRectifiedGridCoverage( landuseRasterGML.toURL() );

      //contextModel
      File contextModelGML = new File( (String)( (IProcessDataProvider)inputProvider )
          .getObjectForID( ContextModelID ) );
      ContextModel contextModel = new ContextModel( contextModelGML.toURL() );

      //Calculation
      //statisticAnalysis
      Hashtable statistics = null;
      if( ( (IProcessDataProvider)inputProvider ).getObjectForID( TemplateRasterID ) != null )
      {
        //templateRaster
        File templateRasterGML = new File( (String)( (IProcessDataProvider)inputProvider )
            .getObjectForID( TemplateRasterID ) );
        RectifiedGridCoverage templateRaster = rasterDataModel
            .getRectifiedGridCoverage( templateRasterGML.toURL() );
        statistics = StatisticAnalysis.getStatisticsWithTemplate( damageRaster, landuseRaster,
            templateRaster );
      }
      else
      {
        statistics = StatisticAnalysis.getStatistics( damageRaster, landuseRaster );
      }

      //Generate output
      //statisticData
      CalcJobClientBean statisticDataOutputBean = (CalcJobClientBean)( (IProcessResultEater)resultEater )
          .getOutputMap().get( StatisticDataID );
      File statisticDataFile = new File( statisticDataOutputBean.getPath() );
      StatisticAnalysis.exportStatisticAsXML( statistics, contextModel.getLanduseList(),
          statisticDataFile.toURL(), statisticData_schemaURL );
      resultEater.addResult( statisticDataOutputBean.getId(), null );
    }
    catch( MalformedURLException e )
    {
      throw new CalcJobServiceException( "CalculateDamageJob Service Exception: Malformed URL", e );
    }
    catch( Exception e )
    {
      throw new CalcJobServiceException( "CalculateDamageJob Service Exception", e );
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