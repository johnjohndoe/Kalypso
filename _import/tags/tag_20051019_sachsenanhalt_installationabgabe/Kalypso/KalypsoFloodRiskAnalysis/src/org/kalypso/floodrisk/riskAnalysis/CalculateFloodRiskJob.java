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
package org.kalypso.floodrisk.riskAnalysis;

import java.awt.Color;
import java.io.File;
import java.io.IOException;
import java.io.StringReader;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.TreeMap;

import javax.xml.transform.OutputKeys;
import javax.xml.transform.Result;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.TransformerFactoryConfigurationError;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.kalypso.floodrisk.data.RasterDataModel;
import org.kalypso.floodrisk.data.RiskContextModel;
import org.kalypso.floodrisk.process.IProcessResultEater;
import org.kalypso.services.calculation.job.ICalcDataProvider;
import org.kalypso.services.calculation.job.ICalcJob;
import org.kalypso.services.calculation.job.ICalcMonitor;
import org.kalypso.services.calculation.job.ICalcResultEater;
import org.kalypso.services.calculation.service.CalcJobClientBean;
import org.kalypso.services.calculation.service.CalcJobServiceException;
import org.kalypsodeegree.graphics.sld.ColorMapEntry;
import org.kalypsodeegree.graphics.sld.RasterSymbolizer;
import org.kalypsodeegree.graphics.sld.StyledLayerDescriptor;
import org.kalypsodeegree.graphics.sld.Symbolizer;
import org.kalypsodeegree.xml.XMLTools;
import org.kalypsodeegree_impl.graphics.sld.ColorMapEntry_Impl;
import org.kalypsodeegree_impl.graphics.sld.DefaultStyleFactory;
import org.kalypsodeegree_impl.graphics.sld.StyleFactory;
import org.kalypsodeegree_impl.graphics.sld.StyledLayerDescriptor_Impl;
import org.kalypsodeegree_impl.model.cv.RectifiedGridCoverage;
import org.w3c.dom.Document;
import org.xml.sax.SAXException;

/**
 * 
 * CalculateFloodRiskJob
 * <p>
 * Job for creating damagerisk maps
 * 
 * created by
 * 
 * @author Nadja Peiler (15.06.2005)
 */
public class CalculateFloodRiskJob implements ICalcJob
{
  //IDs
  //input
  public static final String AnnualDamageRasterDataID = "AnnualDamageRasterData";

  public static final String LanduseRasterDataID = "LanduseRasterData";

  public static final String RiskContextModelID = "RiskContextModel";

  //output
  public static final String FloodRiskRasterDataID = "FloodRiskRasterData";

  public static final String FloodRiskRasterStyleID = "FloodRiskRasterStyle";

  RasterDataModel rasterDataModel = new RasterDataModel();

  /**
   * 
   * @see org.kalypso.services.calculation.job.ICalcJob#run(java.io.File,
   *      org.kalypso.services.calculation.job.ICalcDataProvider, org.kalypso.services.calculation.job.ICalcResultEater,
   *      org.kalypso.services.calculation.job.ICalcMonitor)
   */
  public void run( File tmpdir, ICalcDataProvider inputProvider, ICalcResultEater resultEater, ICalcMonitor monitor )
      throws CalcJobServiceException
  {
    try
    {
      //Generate input
      //annualDamageRaster
      monitor.setMessage( "Lese Eingabedateien" );
      URL annualDamageRasterGML = inputProvider.getURLForID( AnnualDamageRasterDataID );
      RectifiedGridCoverage annualDamageRaster = rasterDataModel.getRectifiedGridCoverage( annualDamageRasterGML );

      //landuseRaster
      URL landuseRasterGML = inputProvider.getURLForID( LanduseRasterDataID );
      RectifiedGridCoverage landuseRaster = rasterDataModel.getRectifiedGridCoverage( landuseRasterGML );

      //contextModel
      URL riskContextModelGML = inputProvider.getURLForID( RiskContextModelID );
      RiskContextModel riskContextModel = new RiskContextModel( riskContextModelGML );

      monitor.setProgress( 40 );

      //start riskAnalysis
      monitor.setMessage( "Berechne" );
      RectifiedGridCoverage floodRiskRaster = FloodRiskAnalysis.defineRisk( annualDamageRaster, landuseRaster,
          riskContextModel.getRiskClassLists() );

      monitor.setProgress( 20 );

      //Generate output
      //floodrisk raster
      monitor.setMessage( "Schreibe Ausgabedateien" );
      CalcJobClientBean floodRiskOutputBean = (CalcJobClientBean)( (IProcessResultEater)resultEater ).getOutputMap()
          .get( FloodRiskRasterDataID );
      File floodRiskResultFile = new File( floodRiskOutputBean.getPath() );
      if( !floodRiskResultFile.exists() )
      {
        if( !floodRiskResultFile.getParentFile().exists() )
          floodRiskResultFile.getParentFile().mkdir();
        floodRiskResultFile.createNewFile();
      }
      rasterDataModel.toFile( floodRiskResultFile, floodRiskRaster );
      resultEater.addResult( floodRiskOutputBean.getId(), null );

      //floodrisk style
      String styleName = "FloodRisk";
      Symbolizer rasterSymbolizer = StyleFactory.createRasterSymbolizer();
      TreeMap defaultColorMap = ( (RasterSymbolizer)rasterSymbolizer ).getColorMap();
      // add riskClass colorMapEntries to defaultColorMap
      Hashtable riskClassList = riskContextModel.getRiskClassKeyList();
      Iterator it = riskClassList.keySet().iterator();
      while( it.hasNext() )
      {
        String riskClassKey = (String)it.next();
        Integer riskClassValue = (Integer)riskClassList.get( riskClassKey );
        // color (default: white)
        Color color = Color.WHITE;
        // opacity (default: 1.0)
        double opacity = 1;
        // quantity
        double quantity = riskClassValue.doubleValue();
        // label
        String label = riskClassKey;
        // new riskClass colorMapEntry
        ColorMapEntry riskClassEntry = new ColorMapEntry_Impl( color, opacity, quantity, label );
        defaultColorMap.put( new Double( quantity ), riskClassEntry );
      }
      Symbolizer[] symbolizer = new Symbolizer[]
      { rasterSymbolizer };
      //create Style
      StyledLayerDescriptor defaultRasterStyle = DefaultStyleFactory.createDefaultStyle( styleName, symbolizer );
      //write Style to File
      CalcJobClientBean floodRiskStyleOutputBean = (CalcJobClientBean)( (IProcessResultEater)resultEater )
          .getOutputMap().get( FloodRiskRasterStyleID );
      File floodRiskStyleFile = new File( floodRiskStyleOutputBean.getPath() );
      if( !floodRiskStyleFile.exists() )
        floodRiskStyleFile.createNewFile();
      writeSLDtoFile( floodRiskStyleFile, defaultRasterStyle );
      resultEater.addResult( floodRiskStyleOutputBean.getId(), null );

      monitor.setProgress( 40 );
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

  private void writeSLDtoFile( File targetFile, StyledLayerDescriptor sld ) throws IOException, SAXException,
      TransformerFactoryConfigurationError, TransformerException
  {
    Document doc = XMLTools.parse( new StringReader( ( (StyledLayerDescriptor_Impl)sld ).exportAsXML() ) );
    final Source source = new DOMSource( doc );
    Result result = new StreamResult( targetFile );
    Transformer t = TransformerFactory.newInstance().newTransformer();
    t.setOutputProperty( "{http://xml.apache.org/xslt}indent-amount", "2" );
    t.setOutputProperty( OutputKeys.INDENT, "yes" );
    t.transform( source, result );
  }

  /**
   * 
   * @see org.kalypso.services.calculation.job.ICalcJob#getSpezifikation()
   */
  public URL getSpezifikation()
  {
    return getClass().getResource( "resources/riskCalcjob_spec.xml" );
  }

}