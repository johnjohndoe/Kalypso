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
package org.kalypso.wizard;

import java.awt.Color;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.StringReader;
import java.math.BigDecimal;
import java.net.URL;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.TreeMap;
import java.util.Vector;

import javax.xml.bind.Marshaller;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Result;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.commons.java.util.zip.ZipUtilities;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.floodrisk.schema.UrlCatalogFloodRisk;
import org.kalypso.floodrisk.tools.GridUtils;
import org.kalypso.floodrisk.tools.Number;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.KalypsoFeatureTheme;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.serialize.GmlSerializeException;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ogc.gml.serialize.ShapeSerializer;
import org.kalypso.template.gismapview.Gismapview;
import org.kalypso.template.gismapview.GismapviewType.LayersType;
import org.kalypso.template.gismapview.GismapviewType.LayersType.Layer;
import org.kalypso.template.types.ExtentType;
import org.kalypso.template.types.StyledLayerType.StyleType;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypsodeegree.graphics.sld.ColorMapEntry;
import org.kalypsodeegree.graphics.sld.FeatureTypeStyle;
import org.kalypsodeegree.graphics.sld.RasterSymbolizer;
import org.kalypsodeegree.graphics.sld.Rule;
import org.kalypsodeegree.graphics.sld.Style;
import org.kalypsodeegree.graphics.sld.StyledLayerDescriptor;
import org.kalypsodeegree.graphics.sld.Symbolizer;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureAssociationTypeProperty;
import org.kalypsodeegree.model.feature.FeatureType;
import org.kalypsodeegree.model.feature.FeatureTypeProperty;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.xml.XMLTools;
import org.kalypsodeegree_impl.gml.schema.GMLSchema;
import org.kalypsodeegree_impl.gml.schema.GMLSchemaCatalog;
import org.kalypsodeegree_impl.graphics.sld.ColorMapEntry_Impl;
import org.kalypsodeegree_impl.graphics.sld.FeatureTypeStyle_Impl;
import org.kalypsodeegree_impl.graphics.sld.RasterSymbolizer_Impl;
import org.kalypsodeegree_impl.graphics.sld.SLDFactory;
import org.kalypsodeegree_impl.graphics.sld.StyleFactory;
import org.kalypsodeegree_impl.graphics.sld.StyledLayerDescriptor_Impl;
import org.kalypsodeegree_impl.graphics.sld.UserStyle_Impl;
import org.kalypsodeegree_impl.model.cv.RectifiedGridCoverage;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;
import org.kalypsodeegree_impl.model.feature.GMLWorkspace_Impl;
import org.opengis.cs.CS_CoordinateSystem;
import org.w3c.dom.Document;

/**
 * CreateFloodRiskProjectJob
 * <p>
 * Job for creating a floodrisk project
 * 
 * created by
 * 
 * @author Nadja Peiler (13.06.2005)
 */
public class CreateFloodRiskProjectJob extends Job
{
  private final String m_resourceBase = "resources/projecttemplate.zip";

  private List layerList;

  private org.kalypso.template.gismapview.ObjectFactory mapTemplateOF;

  private org.kalypso.template.types.ObjectFactory typeOF;

  private GMLWorkspace landuseShapeWS;

  private IPath m_workspacePath;

  private IProject m_projectHandel;

  private File m_landuseDataFile;

  private String m_landusePropertyName;

  private CS_CoordinateSystem m_landuseCooSystem;

  private boolean m_autogenerateLanduseCollection;

  private Vector m_waterlevelGrids;

  private CS_CoordinateSystem m_waterlevelCooSystem;

  /**
   * Constructor, gets all information needed for creating the project
   * 
   * @param name
   * @param workspacePath
   * @param projectHandel
   * @param landuseDataFile
   *          Shape file
   * @param landusePropertyName
   * @param landuseCooSystem
   * @param autogenerateLanduseCollection
   *          flag; true: generate LanduseCollection in ContextModel and RiskContextModel, false: not
   * @param waterlevelGrids
   *          Ascii files
   * @param waterlevelCooSystem
   */
  public CreateFloodRiskProjectJob( String name, IPath workspacePath, IProject projectHandel, File landuseDataFile,
      String landusePropertyName, CS_CoordinateSystem landuseCooSystem, boolean autogenerateLanduseCollection,
      Vector waterlevelGrids, CS_CoordinateSystem waterlevelCooSystem )
  {
    super( name );
    m_workspacePath = workspacePath;
    m_projectHandel = projectHandel;
    m_landuseDataFile = landuseDataFile;
    m_landusePropertyName = landusePropertyName;
    m_landuseCooSystem = landuseCooSystem;
    m_autogenerateLanduseCollection = autogenerateLanduseCollection;
    m_waterlevelGrids = waterlevelGrids;
    m_waterlevelCooSystem = waterlevelCooSystem;
  }

  /**
   * 
   * @see org.eclipse.core.internal.jobs.InternalJob#run(org.eclipse.core.runtime.IProgressMonitor)
   */
  protected IStatus run( IProgressMonitor monitor )
  {
    return createProject( monitor );
  }

  /**
   * creates the project
   * 
   * @param monitor
   * @return status of process
   *  
   */
  protected IStatus createProject( IProgressMonitor monitor )
  {
    int totalWork = 100;
    monitor.beginTask( "Erstelle Hochwasserrisiko Projekt...", totalWork );

    try
    {
      m_projectHandel.create( null );
      m_projectHandel.open( null );
      //set charSet for the new project to the UTF-8 standard
      m_projectHandel.setDefaultCharset( "UTF-8", null );
    }
    catch( CoreException e )
    {
      e.printStackTrace();
      performCancle();
      return e.getStatus();
    }

    try
    {
      //  copy all the resources to the workspace into the new created project
      copyResourcesToProject( m_workspacePath.append( m_projectHandel.getFullPath() ) );
      createEmtyFolders();
      monitor.worked( 10 );
      if( monitor.isCanceled() )
      {
        performCancle();
        return Status.CANCEL_STATUS;
      }

      //create Map(Waterlevel.gmt) with waterlevelGrids and shape of landuse
      String path = m_workspacePath.append( m_projectHandel.getFullPath() + "/Waterlevel/Waterlevel.gmt" ).toFile()
          .toString();

      typeOF = new org.kalypso.template.types.ObjectFactory();
      mapTemplateOF = new org.kalypso.template.gismapview.ObjectFactory();
      Gismapview gismapview = mapTemplateOF.createGismapview();
      LayersType layers = mapTemplateOF.createGismapviewTypeLayersType();
      ExtentType extent = typeOF.createExtentType();
      layerList = layers.getLayer();

      //copy landuseData as shape
      copyLanduseShape();
      Layer landuseLayer = createLanduseLayer( m_landuseDataFile );
      layerList.add( landuseLayer );
      layers.setActive( landuseLayer );

      IKalypsoTheme dummyLanduseTheme = createDummyLanduseTheme();
      GM_Envelope bbox = dummyLanduseTheme.getBoundingBox();
      extent.setLeft( bbox.getMin().getX() );
      extent.setBottom( bbox.getMin().getY() );
      extent.setRight( bbox.getMax().getX() );
      extent.setTop( bbox.getMax().getY() );
      extent.setSrs(m_landuseCooSystem.getName());

      monitor.worked( 40 );
      if( monitor.isCanceled() )
      {
        performCancle();
        return Status.CANCEL_STATUS;
      }

      //generate landuseData as gml
      HashSet landuseTypeSet = createLanduseDataGML();

      //create landuseCollection in contextModel and riskContextModel if
      // necessary
      if( m_autogenerateLanduseCollection )
      {
        autogenerateLanduseCollection( landuseTypeSet );
      }

      //create waterlevelGrids and defaultStyles
      Vector targetFiles = createWaterlevelGrids( monitor );
      if( targetFiles == null )
      {
        performCancle();
        return Status.CANCEL_STATUS;
      }
      createWaterlevelData( targetFiles );
      if( monitor.isCanceled() )
      {
        performCancle();
        return Status.CANCEL_STATUS;
      }

      gismapview.setExtent( extent );
      gismapview.setLayers( layers );

      Marshaller marshaller = mapTemplateOF.createMarshaller();
      marshaller.setProperty( Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE );
      FileWriter fw = new FileWriter( path );
      marshaller.marshal( gismapview, fw );
      fw.close();
    }
    catch( Exception e2 )
    {
      e2.printStackTrace();
      performCancle();
      return new Status( IStatus.ERROR, KalypsoGisPlugin.getId(), 0, e2.getMessage(), e2 );
    }

    //refresh project
    try
    {
      m_projectHandel.refreshLocal( IResource.DEPTH_INFINITE, null );
    }
    catch( CoreException e1 )
    {
      e1.printStackTrace();
      performCancle();
      return e1.getStatus();
    }

    monitor.done();
    return Status.OK_STATUS;
  }

  /**
   * copies resources to project
   * 
   * @param path
   *          project path
   * @throws IOException
   *  
   */
  private void copyResourcesToProject( IPath path ) throws IOException
  {
    final String resource = m_resourceBase;
    System.out.print( "resource: " + resource + "\n" );
    InputStream resourceAsStream = getClass().getResourceAsStream( resource );
    try
    {
      ZipUtilities.unzip( resourceAsStream, path.toFile() );
    }
    finally
    {
      IOUtils.closeQuietly( resourceAsStream );

    }
  }

  /**
   * creates the emty folders "Damage", "Risk" and "Statistic"
   */
  private void createEmtyFolders()
  {
    //Damage
    File damageDir = ( m_workspacePath.append( m_projectHandel.getFullPath().append( "/Damage" ) ) ).toFile();
    damageDir.mkdir();
    //Risk
    File riskDir = ( m_workspacePath.append( m_projectHandel.getFullPath().append( "/Risk" ) ) ).toFile();
    riskDir.mkdir();
    //Statistic
    File statisticDir = ( m_workspacePath.append( m_projectHandel.getFullPath().append( "/Statistic" ) ) ).toFile();
    statisticDir.mkdir();
  }

  /**
   * copies the landuse shapeBase files to the folder "Landuse" in project
   * 
   * @throws IOException
   *  
   */
  private void copyLanduseShape() throws IOException
  {
    String landuseSourceBase = FileUtilities.nameWithoutExtension( m_landuseDataFile.toString() );
    File targetDir = ( m_workspacePath.append( m_projectHandel.getFullPath().append( "/Landuse/" ) ) ).toFile();

    File shp = new File( landuseSourceBase + ".shp" );
    FileUtils.copyFileToDirectory( shp, targetDir );

    File dbf = new File( landuseSourceBase + ".dbf" );
    FileUtils.copyFileToDirectory( dbf, targetDir );

    File shx = new File( landuseSourceBase + ".shx" );
    FileUtils.copyFileToDirectory( shx, targetDir );

  }

  /**
   * creates a DummyTheme for landuse to calculate the extent of the created map (Waterlevel.gmt)
   * 
   * @return KalypsoFeatureTheme landuse
   * @throws GmlSerializeException
   *  
   */
  private IKalypsoTheme createDummyLanduseTheme() throws GmlSerializeException
  {
    String shapeBase = ( m_workspacePath.append( m_projectHandel.getFullPath().append(
        "/Landuse/" + FileUtilities.nameWithoutExtension( m_landuseDataFile.getName().toString() ) ) ) ).toString();
    landuseShapeWS = ShapeSerializer.deserialize( shapeBase, m_landuseCooSystem );
    return new KalypsoFeatureTheme( new CommandableWorkspace( landuseShapeWS ), "featureMember", "Landnutzung" );
  }

  /**
   * creates the gml version of the landuseData (LanduseVectorData.gml, Schema: VectorDataModel.xsd)
   * 
   * @return Set of landuse types (HashSet)
   * @throws IOException
   * @throws GmlSerializeException
   *  
   */
  private HashSet createLanduseDataGML() throws IOException, GmlSerializeException
  {

    File landuseDataGML = m_workspacePath.append( m_projectHandel.getFullPath() + "/Landuse/LanduseVectorData.gml" )
        .toFile();

    //load schema
    GMLSchema schema = GMLSchemaCatalog.getSchema( UrlCatalogFloodRisk.NS_VECTORDATAMODEL );

    String rootFeatureTypeName = "VectorDataCollection";
    String featureTypePropertyName = "FeatureMember";
    String shapeFeatureTypePropertyName = "featureMember";
    String shapeGeomPropertyName = "GEOM";

    // create feature and workspace gml
    final FeatureType[] types = schema.getFeatureTypes();

    FeatureType rootFeatureType = schema.getFeatureType( rootFeatureTypeName );
    Feature rootFeature = FeatureFactory.createFeature( rootFeatureTypeName + "0", rootFeatureType );
    FeatureTypeProperty ftp_feature = rootFeatureType.getProperty( featureTypePropertyName );

    // create features: Feature
    Feature shapeRootFeature = landuseShapeWS.getRootFeature();
    List featureList = (List)shapeRootFeature.getProperty( shapeFeatureTypePropertyName );
    String propertyName = m_landusePropertyName;
    HashSet landuseTypeSet = new HashSet();
    for( int i = 0; i < featureList.size(); i++ )
    {
      Feature feat = (Feature)featureList.get( i );
      String propertyValue = (String)feat.getProperty( propertyName );
      if( !landuseTypeSet.contains( propertyValue ) )
      {
        landuseTypeSet.add( propertyValue );
      }
      Object[] properties = new Object[]
      {
          "",
          "",
          null,
          (GM_Object)feat.getProperty( shapeGeomPropertyName ),
          propertyValue };
      Feature feature = FeatureFactory.createFeature( "Feature" + i, ( (FeatureAssociationTypeProperty)ftp_feature )
          .getAssociationFeatureType(), properties );
      rootFeature.addProperty( FeatureFactory.createFeatureProperty( ftp_feature.getName(), feature ) );
    }

    //create workspace
    GMLWorkspace workspace = new GMLWorkspace_Impl( types, rootFeature, landuseDataGML.toURL(), "", schema
        .getTargetNS(), schema.getNamespaceMap() );

    // serialize Workspace
    FileWriter fw = new FileWriter( landuseDataGML );
    GmlSerializer.serializeWorkspace( fw, workspace );
    fw.close();

    return landuseTypeSet;

  }

  /**
   * Creates a LanduseCollection with the given set of landuse types in ContextModel and RiskContextModel
   * 
   * @param landuseTypeSet
   *          Set of existing landuse types
   * @throws Exception
   *  
   */
  private void autogenerateLanduseCollection( HashSet landuseTypeSet ) throws Exception
  {
    URL contextModelURL = m_workspacePath.append( m_projectHandel.getFullPath() + "/Control/contextModell.gml" )
        .toFile().toURL();
    URL riskContextModelURL = m_workspacePath.append( m_projectHandel.getFullPath() + "/Control/riskContextModell.gml" )
        .toFile().toURL();

    String landuseFeatureType = "Landuse";
    String featureProperty = "Name";
    String parentFeatureName = "LanduseCollectionMember";
    String featurePropertyName = "LanduseMember";

    //contextModel
    GMLWorkspace contextModel = GmlSerializer.createGMLWorkspace( contextModelURL );
    FeatureType ftLanduse = contextModel.getFeatureType( landuseFeatureType );
    Feature rootFeature = contextModel.getRootFeature();
    Feature parentFeature = (Feature)rootFeature.getProperty( parentFeatureName );

    //riskContextModel
    GMLWorkspace riskContextModel = GmlSerializer.createGMLWorkspace( riskContextModelURL );
    FeatureType ftLanduse_risk = riskContextModel.getFeatureType( landuseFeatureType );
    Feature rootFeature_risk = riskContextModel.getRootFeature();
    Feature parentFeature_risk = (Feature)rootFeature_risk.getProperty( parentFeatureName );

    Iterator it = landuseTypeSet.iterator();
    while( it.hasNext() )
    {
      String landusePropertyName = (String)it.next();
      //contextModel
      Feature landuseFeature = contextModel.createFeature( ftLanduse );
      landuseFeature.setProperty( FeatureFactory.createFeatureProperty( featureProperty, landusePropertyName ) );
      contextModel.addFeatureAsComposition( parentFeature, featurePropertyName, 0, landuseFeature );
      //riskContextModel
      Feature landuseFeature_risk = riskContextModel.createFeature( ftLanduse_risk );
      landuseFeature_risk.setProperty( FeatureFactory.createFeatureProperty( featureProperty, landusePropertyName ) );
      riskContextModel.addFeatureAsComposition( parentFeature_risk, featurePropertyName, 0, landuseFeature_risk );
    }
    //save changes
    //  contextModel
    FileWriter fw = new FileWriter( contextModelURL.getFile() );
    GmlSerializer.serializeWorkspace( fw, contextModel );
    fw.close();
    //  riskContextModel
    FileWriter fw_risk = new FileWriter( riskContextModelURL.getFile() );
    GmlSerializer.serializeWorkspace( fw_risk, riskContextModel );
    fw_risk.close();
  }

  /**
   * reads the waterlevelData from ascii files and writes the data in the gml rasterdata format
   * 
   * @param monitor
   * @return targetFiles gml files with waterlevel rasterData
   * @throws Exception
   *  
   */
  private Vector createWaterlevelGrids( IProgressMonitor monitor ) throws Exception
  {
    Vector targetFiles = new Vector();
    int workedPart = 50 / m_waterlevelGrids.size();
    for( int i = 0; i < m_waterlevelGrids.size(); i++ )
    {
      File sourceFile = (File)m_waterlevelGrids.get( i );
      RectifiedGridCoverage grid = GridUtils.importGridArc( sourceFile, m_waterlevelCooSystem );
      String sourceFileNameWithoutExtension = FileUtilities.nameWithoutExtension( sourceFile.getName() );
      File waterlevelDir = ( m_workspacePath.append( m_projectHandel.getFullPath().append( "/Waterlevel" ) ) ).toFile();
      waterlevelDir.mkdir();
      File targetFile = ( m_workspacePath.append( m_projectHandel.getFullPath().append(
          "/Waterlevel/" + sourceFileNameWithoutExtension + ".gml" ) ) ).toFile();
      GridUtils.writeRasterData( targetFile, grid );
      targetFiles.add( targetFile );
      File sldFile = ( m_workspacePath.append( m_projectHandel.getFullPath().append(
          "/.styles/" + sourceFileNameWithoutExtension + ".sld" ) ) ).toFile();
      Color lightBlue = new Color( 150, 150, 255 );
      int numOfCategories = 5;
      createRasterStyle( sldFile, sourceFileNameWithoutExtension, grid, lightBlue, numOfCategories );
      layerList.add( createWaterlevelLayer( targetFile, sourceFileNameWithoutExtension ) );
      grid = null;
      monitor.worked( workedPart );
      if( monitor.isCanceled() )
        return null;
    }
    return targetFiles;
  }

  /**
   * Creates a rasterStyle (interval-value) with the given number of intervals and a given color as lightest color
   * 
   * @param resultFile
   *          style
   * @param styleName
   *          name of style
   * @param grid
   * @param color
   *          lightest color
   * @param numOfCategories
   *          number of intervals
   * @throws Exception
   *  
   */
  private void createRasterStyle( File resultFile, String styleName, RectifiedGridCoverage grid, Color color,
      int numOfCategories ) throws Exception
  {
    TreeMap colorMap = new TreeMap();
    ColorMapEntry colorMapEntry_noData = new ColorMapEntry_Impl( Color.WHITE, 0, -9999, "Keine Daten" );
    colorMap.put( new Double( -9999 ), colorMapEntry_noData );
    double min = grid.getRangeSet().getMinValue();
    double max = grid.getRangeSet().getMaxValue();
    double intervalStep = ( max - min ) / numOfCategories;
    for( int i = 0; i < numOfCategories; i++ )
    {
      double quantity = Number.round( ( min + ( i * intervalStep ) ), 4, BigDecimal.ROUND_HALF_EVEN );
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
    Rule rule = StyleFactory.createRule( symbolizers, "default", "default", "default", minScaleDenominator,
        maxScaleDenominator );
    featureTypeStyle.addRule( rule );
    FeatureTypeStyle[] featureTypeStyles = new FeatureTypeStyle[]
    { featureTypeStyle };
    Style[] styles = new Style[]
    { new UserStyle_Impl( styleName, styleName, null, false, featureTypeStyles ) };
    org.kalypsodeegree.graphics.sld.Layer[] layers = new org.kalypsodeegree.graphics.sld.Layer[]
    { SLDFactory.createNamedLayer( "deegree style definition", null, styles ) };
    StyledLayerDescriptor sld = SLDFactory.createStyledLayerDescriptor( layers, "1.0" );
    Document doc = XMLTools.parse( new StringReader( ( (StyledLayerDescriptor_Impl)sld ).exportAsXML() ) );
    final Source source = new DOMSource( doc );
    Result result = new StreamResult( resultFile );
    Transformer t = TransformerFactory.newInstance().newTransformer();
    t.setOutputProperty( "{http://xml.apache.org/xslt}indent-amount", "2" );
    t.setOutputProperty( OutputKeys.INDENT, "yes" );
    t.transform( source, result );
  }

  /**
   * creates the control file waterlevelData.gml with all given waterlevelGrids as parameters
   * 
   * @param targetFiles
   * @throws IOException
   * @throws GmlSerializeException
   *  
   */
  private void createWaterlevelData( Vector targetFiles ) throws IOException, GmlSerializeException
  {
    File waterlevelDataFile = m_workspacePath.append( m_projectHandel.getFullPath() + "/Control/waterlevelData.gml" )
        .toFile();

    //load schema
    final GMLSchema schema = GMLSchemaCatalog.getSchema( UrlCatalogFloodRisk.NS_WATERLEVELDATA );

    final FeatureType[] types = schema.getFeatureTypes();

    //create rootFeature
    String rootFeatureName = "WaterlevelData";
    FeatureType rootFeatureType = schema.getFeatureType( rootFeatureName );
    Feature rootFeature = FeatureFactory.createFeature( "WaterlevelData0", rootFeatureType );

    //create waterlevelFeature(s)
    String waterlevelFeatureName = "Waterlevel";
    String waterlevelMember = "WaterlevelMember";
    String featureProperty = "WaterlevelRasterData";
    FeatureType waterlevelFeatureType = schema.getFeatureType( waterlevelFeatureName );
    int identifier = 0;
    for( int i = 0; i < targetFiles.size(); i++ )
    {
      Feature waterlevelFeature = FeatureFactory.createFeature( waterlevelFeatureName + identifier,
          waterlevelFeatureType );
      IFile waterlevelFile = ResourceUtilities.findFileFromURL( ( (File)targetFiles.get( i ) ).toURL() );
      waterlevelFeature.setProperty( FeatureFactory.createFeatureProperty( featureProperty, waterlevelFile ) );
      rootFeature.addProperty( FeatureFactory.createFeatureProperty( waterlevelMember, waterlevelFeature ) );
      identifier = identifier + 1;
    }

    //create workspace
    GMLWorkspace workspace = new GMLWorkspace_Impl( types, rootFeature, waterlevelDataFile.toURL(), "", schema
        .getTargetNS(), schema.getNamespaceMap() );

    // serialize Workspace
    FileWriter fw = new FileWriter( waterlevelDataFile );
    GmlSerializer.serializeWorkspace( fw, workspace );
    fw.close();
  }

  /**
   * creates a landuse layer
   * 
   * @param sourceFile
   * @return landuse layer
   * @throws Exception
   *  
   */
  private org.kalypso.template.gismapview.GismapviewType.LayersType.Layer createLanduseLayer( File sourceFile )
      throws Exception
  {
    org.kalypso.template.gismapview.GismapviewType.LayersType.Layer newLayer = mapTemplateOF
        .createGismapviewTypeLayersTypeLayer();

    //set attributes for the layer
    newLayer.setName( "Landnutzung" );
    newLayer.setVisible( true );
    newLayer.setFeaturePath( "featureMember" );
    newLayer.setHref( "project:/Landuse/" + FileUtilities.nameWithoutExtension( sourceFile.getName() ) + "#"
        + m_landuseCooSystem.getName() );
    newLayer.setType( "simple" );
    newLayer.setLinktype( "shape" );
    newLayer.setActuate( "onRequest" );
    newLayer.setId( "ID_1" );

    List styleList = newLayer.getStyle();
    StyleType style = typeOF.createStyledLayerTypeStyleType();

    //set attributes for the style
    style.setLinktype( "sld" );
    style.setStyle( "Landnutzung" );
    style.setActuate( "onRequest" );
    style.setHref( "../.styles/landuse.sld" );
    style.setType( "simple" );

    //add the style to the layer
    styleList.add( style );
    return newLayer;
  }

  int id = 2;

  /**
   * creates a waterlevel layer
   * 
   * @param sourceFile
   * @param styleName
   * @return waterlevel layer
   * @throws Exception
   *  
   */
  private org.kalypso.template.gismapview.GismapviewType.LayersType.Layer createWaterlevelLayer( File sourceFile,
      String styleName ) throws Exception
  {

    org.kalypso.template.gismapview.GismapviewType.LayersType.Layer newLayer = mapTemplateOF
        .createGismapviewTypeLayersTypeLayer();

    //set attributes for the layer
    newLayer.setName( styleName );
    newLayer.setVisible( false );
    newLayer.setFeaturePath( "RectifiedGridCoverageMember" );
    newLayer.setHref( "../Waterlevel/" + sourceFile.getName() );
    newLayer.setType( "simple" );
    newLayer.setLinktype( "gml" );
    newLayer.setActuate( "onRequest" );
    newLayer.setId( "ID_" + id );
    id = id + 1;

    List styleList = newLayer.getStyle();
    StyleType style = typeOF.createStyledLayerTypeStyleType();

    //set attributes for the style
    style.setLinktype( "sld" );
    style.setStyle( styleName );
    style.setActuate( "onRequest" );
    style.setHref( "../.styles/" + styleName + ".sld" );
    style.setType( "simple" );

    //add the style to the layer
    styleList.add( style );

    return newLayer;
  }

  /**
   * deletes the project, when job is canceled
   *  
   */
  public boolean performCancle()
  {
    try
    {
      m_projectHandel.delete( true, false, null );
    }
    catch( CoreException e )
    {
      e.printStackTrace();
      return false;
    }
    return true;
  }
}