package org.kalypso.floodrisk.damageAnalysis;

import java.awt.Color;
import java.io.File;
import java.io.StringReader;
import java.math.BigDecimal;
import java.net.MalformedURLException;
import java.net.URI;
import java.net.URL;
import java.util.Iterator;
import java.util.List;
import java.util.TreeMap;
import java.util.Vector;

import javax.xml.transform.OutputKeys;
import javax.xml.transform.Result;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.kalypso.floodrisk.data.ContextModel;
import org.kalypso.floodrisk.data.RasterDataModel;
import org.kalypso.floodrisk.process.IProcessDataProvider;
import org.kalypso.floodrisk.process.IProcessResultEater;
import org.kalypso.floodrisk.tools.Number;
import org.kalypso.java.io.FileUtilities;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.services.calculation.job.ICalcDataProvider;
import org.kalypso.services.calculation.job.ICalcJob;
import org.kalypso.services.calculation.job.ICalcMonitor;
import org.kalypso.services.calculation.job.ICalcResultEater;
import org.kalypso.services.calculation.service.CalcJobClientBean;
import org.kalypso.services.calculation.service.CalcJobServiceException;
import org.kalypsodeegree.graphics.sld.ColorMapEntry;
import org.kalypsodeegree.graphics.sld.FeatureTypeStyle;
import org.kalypsodeegree.graphics.sld.RasterSymbolizer;
import org.kalypsodeegree.graphics.sld.Rule;
import org.kalypsodeegree.graphics.sld.Style;
import org.kalypsodeegree.graphics.sld.StyledLayerDescriptor;
import org.kalypsodeegree.graphics.sld.Symbolizer;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.xml.XMLTools;
import org.kalypsodeegree_impl.graphics.sld.ColorMapEntry_Impl;
import org.kalypsodeegree_impl.graphics.sld.FeatureTypeStyle_Impl;
import org.kalypsodeegree_impl.graphics.sld.RasterSymbolizer_Impl;
import org.kalypsodeegree_impl.graphics.sld.SLDFactory;
import org.kalypsodeegree_impl.graphics.sld.StyleFactory;
import org.kalypsodeegree_impl.graphics.sld.StyledLayerDescriptor_Impl;
import org.kalypsodeegree_impl.graphics.sld.UserStyle_Impl;
import org.kalypsodeegree_impl.model.cv.RectifiedGridCoverage;
import org.w3c.dom.Document;

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

public class CalculateDamageJob implements ICalcJob
{
  //IDs
  //input
  public static final String LanduseRasterDataID = "LanduseRasterData";

  public static final String WaterlevelDataID = "WaterlevelData";

  public static final String ContextModelID = "ContextModel";

  //output
  public static final String DamageDirectoryID = "DamageDirectory";

  public static final String AnnualDamageRasterDataID = "AnnualDamageRasterData";

  //public static final String StyleDirectoryID = "StyleDirectory";

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
      //landuseRaster
      monitor.setMessage( "Generate input" );
      File landuseRasterGML = new File( (String)( (IProcessDataProvider)inputProvider )
          .getObjectForID( LanduseRasterDataID ) );
      RectifiedGridCoverage landuseRaster = rasterDataModel
          .getRectifiedGridCoverage( landuseRasterGML.toURL() );

      //contextModel
      File contextModelGML = new File( (String)( (IProcessDataProvider)inputProvider )
          .getObjectForID( ContextModelID ) );
      ContextModel contextModel = new ContextModel( contextModelGML.toURL() );

      //WaterlevelData
      TreeMap waterlevelGrids = readWaterlevelData( new File(
          (String)( (IProcessDataProvider)inputProvider ).getObjectForID( WaterlevelDataID ) ) );
      monitor.setProgress( 40 );

      //start damageAnalysis
      // calculate damagePercentage
      monitor.setMessage( "Start calculation" );
      TreeMap damagePercentageGrids = DamageAnalysis.calculateDamagePercentages( waterlevelGrids,
          landuseRaster, contextModel.getDamageFunctionList() );

      // calculate damage
      TreeMap damageGrids = DamageAnalysis.calculateDamages( damagePercentageGrids, landuseRaster,
          null, contextModel.getAssetValueList() );

      // calculate annualDamage
      Vector tempGrids = DamageAnalysis.calculateTempGridsAnnualDamage( damageGrids );
      RectifiedGridCoverage annualDamageGrid = DamageAnalysis.calculateAnnualDamage( tempGrids );

      monitor.setProgress( 20 );

      //Generate Output
      // damage directory
      monitor.setMessage( "Generate Output" );
      CalcJobClientBean damageDirOutputBean = (CalcJobClientBean)( (IProcessResultEater)resultEater )
          .getOutputMap().get( DamageDirectoryID );
      File damageResultDir = new File( damageDirOutputBean.getPath() );
      if( !damageResultDir.exists() )
        damageResultDir.mkdir();
      generateDamageResultDir( damageGrids, tempGrids, damageResultDir );
      resultEater.addResult( damageDirOutputBean.getId(), null );

      // annualDamage
      CalcJobClientBean annualDamageOutputBean = (CalcJobClientBean)( (IProcessResultEater)resultEater )
          .getOutputMap().get( AnnualDamageRasterDataID );
      File annualDamageResultFile = new File( annualDamageOutputBean.getPath() );
      if( !annualDamageResultFile.exists() )
        annualDamageResultFile.createNewFile();
      rasterDataModel.toFile( annualDamageResultFile, annualDamageGrid );
      //style
      File styleFile = new File( FileUtilities.nameWithoutExtension( annualDamageResultFile
          .toString() )
          + ".sld" );
      Color lightRed = new Color( 255, 100, 100 );
      int numOfCategories = 4;
      String styleName = FileUtilities.nameWithoutExtension( annualDamageResultFile.getName() );
      createRasterStyle( styleFile, styleName, annualDamageGrid, lightRed, numOfCategories );
      resultEater.addResult( annualDamageOutputBean.getId(), null );

      monitor.setProgress( 30 );

      //clear resources
      landuseRaster = null;
      waterlevelGrids = null;
      damagePercentageGrids = null;
      damageGrids = null;
      tempGrids = null;

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

  private TreeMap readWaterlevelData( File waterlevelDataGML ) throws MalformedURLException,
      Exception
  {
    String waterlevelFeatureListPropertyName = "WaterlevelMember";
    String annualityPropertyName = "Annuality";
    String waterlevelDataURLPropertyName = "WaterlevelRasterData";

    TreeMap waterlevelGrids = new TreeMap();
    GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( waterlevelDataGML.toURL() );
    Feature waterlevelDataFeature = workspace.getRootFeature();

    List waterlevelDataList = (List)waterlevelDataFeature
        .getProperty( waterlevelFeatureListPropertyName );
    Iterator it = waterlevelDataList.iterator();
    while( it.hasNext() )
    {
      Feature waterlevelFeature = (Feature)it.next();
      double annuality = ( (Double)waterlevelFeature.getProperty( annualityPropertyName ) )
          .doubleValue();
      URI dataURI = (URI)waterlevelFeature.getProperty( waterlevelDataURLPropertyName );
      RectifiedGridCoverage grid = rasterDataModel.getRectifiedGridCoverage( dataURI.toURL() );
      double p = 1 / annuality;
      waterlevelGrids.put( new Double( p ), grid );
    }

    return waterlevelGrids;
  }

  public void generateDamageResultDir( TreeMap damageGrids, Vector tempGrids, File damageResultDir )
      throws Exception
  {
    Object[] keys = damageGrids.keySet().toArray();
    for( int i = 0; i < keys.length; i++ )
    {
      Double key = (Double)keys[i];
      double annuality = 1 / key.doubleValue();
      File damageFile = new File( damageResultDir, "damage_HQ" + (int)annuality + ".gml" );
      RectifiedGridCoverage damageGrid = (RectifiedGridCoverage)damageGrids.get( key );
      rasterDataModel.toFile( damageFile, damageGrid );
      //style
      File damageStyleFile = new File( FileUtilities.nameWithoutExtension( damageFile.toString() )
          + ".sld" );
      Color lightRed = new Color( 255, 100, 100 );
      int numOfCategories = 4;
      String styleName = FileUtilities.nameWithoutExtension( damageFile.getName() );
      createRasterStyle( damageStyleFile, styleName, damageGrid, lightRed, numOfCategories );

      if( i < keys.length - 1 )
      {
        Double nextKey = (Double)keys[i + 1];
        double deltaP = nextKey.doubleValue() - key.doubleValue();
        RectifiedGridCoverage tempGrid = (RectifiedGridCoverage)tempGrids.get( i );
        File tempGridFile = new File( damageResultDir, "tempGrid_deltaP"
            + Number.round( deltaP, 4, BigDecimal.ROUND_HALF_EVEN ) + ".gml" );
        rasterDataModel.toFile( tempGridFile, tempGrid );
      }
    }
  }

  private void createRasterStyle( File resultFile, String styleName, RectifiedGridCoverage grid,
      Color color, int numOfCategories ) throws Exception
  {
    TreeMap colorMap = new TreeMap();
    ColorMapEntry colorMapEntry_noData = new ColorMapEntry_Impl( Color.WHITE, 0, -9999,
        "Keine Daten" );
    colorMap.put( new Double( -9999 ), colorMapEntry_noData );
    double min = grid.getRangeSet().getMinValue();
    double max = grid.getRangeSet().getMaxValue();
    double intervalStep = ( max - min ) / numOfCategories;
    for( int i = 0; i < numOfCategories; i++ )
    {
      double quantity = Number
          .round( ( min + ( i * intervalStep ) ), 4, BigDecimal.ROUND_HALF_EVEN );
      ColorMapEntry colorMapEntry = new ColorMapEntry_Impl( color, 1, quantity, "" );
      color = color.darker();
      colorMap.put( new Double( quantity ), colorMapEntry );
    }
    ColorMapEntry colorMapEntry_max = new ColorMapEntry_Impl( Color.WHITE, 1, max, "" );
    colorMap.put( new Double( max ), colorMapEntry_max );
    RasterSymbolizer rasterSymbolizer = new RasterSymbolizer_Impl( colorMap );
    Symbolizer[] symbolizers = new Symbolizer[]
    { rasterSymbolizer };
    FeatureTypeStyle featureTypeStyle = new FeatureTypeStyle_Impl();
    double minScaleDenominator = 0;
    double maxScaleDenominator = 1.8;
    Rule rule = StyleFactory.createRule( symbolizers, "default", "default", "default",
        minScaleDenominator, maxScaleDenominator );
    featureTypeStyle.addRule( rule );
    FeatureTypeStyle[] featureTypeStyles = new FeatureTypeStyle[]
    { featureTypeStyle };
    Style[] styles = new Style[]
    { new UserStyle_Impl( styleName, styleName, null, false, featureTypeStyles ) };
    org.kalypsodeegree.graphics.sld.Layer[] layers = new org.kalypsodeegree.graphics.sld.Layer[]
    { SLDFactory.createNamedLayer( "deegree style definition", null, styles ) };
    StyledLayerDescriptor sld = SLDFactory.createStyledLayerDescriptor( layers, "1.0" );
    Document doc = XMLTools.parse( new StringReader( ( (StyledLayerDescriptor_Impl)sld )
        .exportAsXML() ) );
    final Source source = new DOMSource( doc );
    Result result = new StreamResult( resultFile );
    Transformer t = TransformerFactory.newInstance().newTransformer();
    t.setOutputProperty( "{http://xml.apache.org/xslt}indent-amount", "2" );
    t.setOutputProperty( OutputKeys.INDENT, "yes" );
    t.transform( source, result );
  }

  public URL getSpezifikation()
  {
    return null;
  }

}