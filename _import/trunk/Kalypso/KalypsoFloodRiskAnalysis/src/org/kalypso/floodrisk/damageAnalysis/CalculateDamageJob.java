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

package org.kalypso.floodrisk.damageAnalysis;

import java.awt.Color;
import java.io.File;
import java.io.StringReader;
import java.math.BigDecimal;
import java.net.MalformedURLException;
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

import org.eclipse.core.resources.IFile;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.floodrisk.data.ContextModel;
import org.kalypso.floodrisk.data.RasterDataModel;
import org.kalypso.floodrisk.internationalize.Messages;
import org.kalypso.floodrisk.process.IProcessResultEater;
import org.kalypso.floodrisk.tools.Number;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.simulation.core.ISimulation;
import org.kalypso.simulation.core.ISimulationDataProvider;
import org.kalypso.simulation.core.ISimulationMonitor;
import org.kalypso.simulation.core.ISimulationResultEater;
import org.kalypso.simulation.core.SimulationDataPath;
import org.kalypso.simulation.core.SimulationException;
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

/**
 * 
 * CalculateDamageJob
 * <p>
 * Job to process a damage analysis
 * 
 * created by
 * 
 * @author Nadja Peiler (14.06.2005)
 */
public class CalculateDamageJob implements ISimulation
{
  //IDs
  //input
  public static final String LanduseRasterDataID = "LanduseRasterData"; //$NON-NLS-1$

  //optional
  public static final String AdministrationUnitRasterDataID = "AdministrationUnitRasterData"; //$NON-NLS-1$

  public static final String WaterlevelDataID = "WaterlevelData"; //$NON-NLS-1$

  public static final String ContextModelID = "ContextModel"; //$NON-NLS-1$

  //output
  public static final String DamageDirectoryID = "DamageDirectory"; //$NON-NLS-1$

  public static final String AnnualDamageRasterDataID = "AnnualDamageRasterData"; //$NON-NLS-1$

  RasterDataModel rasterDataModel = new RasterDataModel();

  /**
   * 
   * @see org.kalypso.services.calculation.job.ICalcJob#run(java.io.File,
   *      org.kalypso.services.calculation.job.ICalcDataProvider, org.kalypso.services.calculation.job.ICalcResultEater,
   *      org.kalypso.services.calculation.job.ICalcMonitor)
   */
  public void run( File tmpdir, ISimulationDataProvider inputProvider, ISimulationResultEater resultEater, ISimulationMonitor monitor )
      throws SimulationException
  {
    try
    {
      //Generate input
      //landuseRaster
      monitor.setMessage( Messages.getString("damageAnalysis.CalculateDamageJob.LoadingInputData") ); //$NON-NLS-1$
      URL landuseRasterGML = (URL) inputProvider.getInputForID( LanduseRasterDataID );
      RectifiedGridCoverage landuseRaster = rasterDataModel.getRectifiedGridCoverage( landuseRasterGML );

      //administrationUnitRaster
      RectifiedGridCoverage administrationUnitRaster = null;
      if( inputProvider.getInputForID( AdministrationUnitRasterDataID ) != null )
      {
        URL administrationUnitRasterGML = (URL) inputProvider.getInputForID( AdministrationUnitRasterDataID );
        administrationUnitRaster = rasterDataModel.getRectifiedGridCoverage( administrationUnitRasterGML );
      }

      //contextModel
      URL contextModelGML = (URL) inputProvider.getInputForID( ContextModelID );
      ContextModel contextModel = new ContextModel( contextModelGML );

      //WaterlevelData
      TreeMap waterlevelGrids = readWaterlevelData( (URL) inputProvider.getInputForID( WaterlevelDataID ) );
      monitor.setProgress( 40 );

      //start damageAnalysis
      // calculate damagePercentage
      monitor.setMessage( Messages.getString("damageAnalysis.CalculateDamageJob.Calculating") ); //$NON-NLS-1$
      TreeMap damagePercentageGrids = DamageAnalysis.calculateDamagePercentages( waterlevelGrids, landuseRaster,
          contextModel.getDamageFunctionList() );

      // calculate damage
      TreeMap damageGrids = DamageAnalysis.calculateDamages( damagePercentageGrids, landuseRaster,
          administrationUnitRaster, contextModel.getAssetValueList() );

      // calculate annualDamage
      Vector tempGrids = DamageAnalysis.calculateTempGridsAnnualDamage( damageGrids );
      RectifiedGridCoverage annualDamageGrid = DamageAnalysis.calculateAnnualDamage( tempGrids );

      monitor.setProgress( 20 );

      //Generate Output
      // damage directory
      monitor.setMessage( Messages.getString("damageAnalysis.CalculateDamageJob.SavingOutputData") ); //$NON-NLS-1$
      SimulationDataPath damageDirOutputBean = (SimulationDataPath)( (IProcessResultEater)resultEater ).getOutputMap()
          .get( DamageDirectoryID );
      File damageResultDir = new File( damageDirOutputBean.getPath() );
      if( !damageResultDir.exists() )
        damageResultDir.mkdir();
      generateDamageResultDir( damageGrids, tempGrids, damageResultDir );
      resultEater.addResult( damageDirOutputBean.getId(), null );

      // annualDamage
      SimulationDataPath annualDamageOutputBean = (SimulationDataPath)( (IProcessResultEater)resultEater ).getOutputMap()
          .get( AnnualDamageRasterDataID );
      File annualDamageResultFile = new File( annualDamageOutputBean.getPath() );
      if( !annualDamageResultFile.exists() )
        annualDamageResultFile.createNewFile();
      rasterDataModel.toFile( annualDamageResultFile, annualDamageGrid );
      //style
      File styleFile = new File( FileUtilities.nameWithoutExtension( annualDamageResultFile.toString() ) + ".sld" ); //$NON-NLS-1$
      Color lightRed = new Color( 255, 100, 100 );
      int numOfCategories = 4;
      String styleName = FileUtilities.nameWithoutExtension( annualDamageResultFile.getName() );
      createRasterStyle( styleFile, styleName, annualDamageGrid, lightRed, numOfCategories );
      resultEater.addResult( annualDamageOutputBean.getId(), null );

      monitor.setProgress( 40 );
      //clear resources
      landuseRaster = null;
      waterlevelGrids = null;
      damagePercentageGrids = null;
      damageGrids = null;
      tempGrids = null;

    }
    catch( MalformedURLException e )
    {
      throw new SimulationException( "CalculateDamageJob Service Exception: Malformed URL", e ); //$NON-NLS-1$
    }
    catch( Exception e )
    {
      throw new SimulationException( "CalculateDamageJob Service Exception", e ); //$NON-NLS-1$
    }
  }

  /**
   * reads the waterlevelData and puts the grids in a TreeMap
   * 
   * @param waterlevelDataGML
   *          controlfile with location of each waterlevelGrid relative to the project and associated annuality
   * @return TreeMap(key=p, value=waterlevelGrid(RectifiedGridCoverage))
   * @throws MalformedURLException
   * @throws Exception
   *  
   */
  private TreeMap readWaterlevelData( URL waterlevelDataGML ) throws MalformedURLException, Exception
  {
    String waterlevelFeatureListPropertyName = "WaterlevelMember"; //$NON-NLS-1$
    String annualityPropertyName = "Annuality"; //$NON-NLS-1$
    String waterlevelDataURLPropertyName = "WaterlevelRasterData"; //$NON-NLS-1$

    TreeMap<Double, RectifiedGridCoverage> waterlevelGrids = new TreeMap<Double, RectifiedGridCoverage>();
    GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( waterlevelDataGML, null );
    Feature waterlevelDataFeature = workspace.getRootFeature();

    List waterlevelDataList = (List)waterlevelDataFeature.getProperty( waterlevelFeatureListPropertyName );
    Iterator it = waterlevelDataList.iterator();
    while( it.hasNext() )
    {
      Feature waterlevelFeature = (Feature)it.next();
      double annuality = ( (Double)waterlevelFeature.getProperty( annualityPropertyName ) ).doubleValue();
      IFile resourceFile = (IFile)waterlevelFeature.getProperty( waterlevelDataURLPropertyName );
      System.out.println( ResourceUtilities.createURL( resourceFile ) );
      RectifiedGridCoverage grid = rasterDataModel
          .getRectifiedGridCoverage( ResourceUtilities.createURL( resourceFile ) );
      double p = 1 / annuality;
      waterlevelGrids.put( new Double( p ), grid );
    }

    return waterlevelGrids;
  }

  /**
   * writes all results in the result directory
   * 
   * @param damageGrids
   * @param tempGrids
   * @param damageResultDir
   *          resultDirectory
   * @throws Exception
   *  
   */
  public void generateDamageResultDir( TreeMap damageGrids, Vector tempGrids, File damageResultDir ) throws Exception
  {
    Object[] keys = damageGrids.keySet().toArray();
    for( int i = 0; i < keys.length; i++ )
    {
      Double key = (Double)keys[i];
      double annuality = 1 / key.doubleValue();
      File damageFile = new File( damageResultDir, "damage_HQ" + (int)annuality + ".gml" ); //$NON-NLS-1$ //$NON-NLS-2$
      RectifiedGridCoverage damageGrid = (RectifiedGridCoverage)damageGrids.get( key );
      rasterDataModel.toFile( damageFile, damageGrid );
      //style
      File damageStyleFile = new File( FileUtilities.nameWithoutExtension( damageFile.toString() ) + ".sld" ); //$NON-NLS-1$
      Color lightRed = new Color( 255, 100, 100 );
      int numOfCategories = 4;
      String styleName = FileUtilities.nameWithoutExtension( damageFile.getName() );
      createRasterStyle( damageStyleFile, styleName, damageGrid, lightRed, numOfCategories );

      if( i < keys.length - 1 )
      {
        Double nextKey = (Double)keys[i + 1];
        double deltaP = nextKey.doubleValue() - key.doubleValue();
        RectifiedGridCoverage tempGrid = (RectifiedGridCoverage)tempGrids.get( i );
        File tempGridFile = new File( damageResultDir, "tempGrid_deltaP" //$NON-NLS-1$
            + Number.round( deltaP, 4, BigDecimal.ROUND_HALF_EVEN ) + ".gml" ); //$NON-NLS-1$
        rasterDataModel.toFile( tempGridFile, tempGrid );
      }
    }
  }

  /**
   * creates a default rasterStyle with the given number of categories
   * 
   * @param resultFile
   * @param styleName
   * @param grid
   *          RectifiedGridCoverage
   * @param color
   *          lightest color
   * @param numOfCategories
   * @throws Exception
   *  
   */
  private void createRasterStyle( File resultFile, String styleName, RectifiedGridCoverage grid, Color color,
      int numOfCategories ) throws Exception
  {
    TreeMap<Double, ColorMapEntry> colorMap = new TreeMap<Double, ColorMapEntry>();
    ColorMapEntry colorMapEntry_noData = new ColorMapEntry_Impl( Color.WHITE, 0, -9999, Messages.getString("damageAnalysis.CalculateDamageJob.NoData") ); //$NON-NLS-1$
    colorMap.put( new Double( -9999 ), colorMapEntry_noData );
    double min = grid.getRangeSet().getMinValue();
    double max = grid.getRangeSet().getMaxValue();
    double intervalStep = ( max - min ) / numOfCategories;
    for( int i = 0; i < numOfCategories; i++ )
    {
      double quantity = Number.round( ( min + ( i * intervalStep ) ), 4, BigDecimal.ROUND_HALF_EVEN );
      ColorMapEntry colorMapEntry = new ColorMapEntry_Impl( color, 1, quantity, "" ); //$NON-NLS-1$
      color = color.darker();
      colorMap.put( new Double( quantity ), colorMapEntry );
    }
    ColorMapEntry colorMapEntry_max = new ColorMapEntry_Impl( Color.WHITE, 1, max, "" ); //$NON-NLS-1$
    colorMap.put( new Double( Number.round( max, 4, BigDecimal.ROUND_HALF_EVEN ) ), colorMapEntry_max );
    RasterSymbolizer rasterSymbolizer = new RasterSymbolizer_Impl( colorMap );
    Symbolizer[] symbolizers = new Symbolizer[]
    { rasterSymbolizer };
    FeatureTypeStyle featureTypeStyle = new FeatureTypeStyle_Impl();
    double minScaleDenominator = 0;
    double maxScaleDenominator = 1.8;
    Rule rule = StyleFactory.createRule( symbolizers, "default", "default", "default", minScaleDenominator, //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
        maxScaleDenominator );
    featureTypeStyle.addRule( rule );
    FeatureTypeStyle[] featureTypeStyles = new FeatureTypeStyle[]
    { featureTypeStyle };
    Style[] styles = new Style[]
    { new UserStyle_Impl( styleName, styleName, null, false, featureTypeStyles ) };
    org.kalypsodeegree.graphics.sld.Layer[] layers = new org.kalypsodeegree.graphics.sld.Layer[]
    { SLDFactory.createNamedLayer( "deegree style definition", null, styles ) }; //$NON-NLS-1$
    StyledLayerDescriptor sld = SLDFactory.createStyledLayerDescriptor( layers, "1.0" ); //$NON-NLS-1$
    Document doc = XMLTools.parse( new StringReader( ( (StyledLayerDescriptor_Impl)sld ).exportAsXML() ) );
    final Source source = new DOMSource( doc );
    Result result = new StreamResult( resultFile );
    Transformer t = TransformerFactory.newInstance().newTransformer();
    t.setOutputProperty( "{http://xml.apache.org/xslt}indent-amount", "2" ); //$NON-NLS-1$ //$NON-NLS-2$
    t.setOutputProperty( OutputKeys.INDENT, "yes" ); //$NON-NLS-1$
    t.transform( source, result );
  }

  /**
   * 
   * @see org.kalypso.services.calculation.job.ICalcJob#getSpezifikation()
   */
  public URL getSpezifikation()
  {
    return getClass().getResource( "resources/damageCalcjob_spec.xml" ); //$NON-NLS-1$
  }

}