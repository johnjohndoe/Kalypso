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
import java.lang.reflect.InvocationTargetException;
import java.math.BigDecimal;
import java.net.URL;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.TreeMap;
import java.util.Vector;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.Marshaller;
import javax.xml.namespace.QName;
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
import org.kalypso.gmlschema.GMLSchema;
import org.kalypso.gmlschema.GMLSchemaCatalog;
import org.kalypso.gmlschema.KalypsoGMLSchemaPlugin;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.gmlschema.types.IMarshallingTypeHandler;
import org.kalypso.gmlschema.types.ITypeRegistry;
import org.kalypso.gmlschema.types.MarshallingTypeRegistrySingleton;
import org.kalypso.gmlschema.types.TypeRegistryException;
import org.kalypso.jwsdp.JaxbUtilities;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.KalypsoFeatureTheme;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.selection.FeatureSelectionManager2;
import org.kalypso.ogc.gml.serialize.GmlSerializeException;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ogc.gml.serialize.ShapeSerializer;
import org.kalypso.template.gismapview.Gismapview;
import org.kalypso.template.gismapview.Gismapview.Layers;
import org.kalypso.template.types.ExtentType;
import org.kalypso.template.types.StyledLayerType;
import org.kalypso.template.types.StyledLayerType.Style;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypsodeegree.graphics.sld.ColorMapEntry;
import org.kalypsodeegree.graphics.sld.FeatureTypeStyle;
import org.kalypsodeegree.graphics.sld.RasterSymbolizer;
import org.kalypsodeegree.graphics.sld.Rule;
import org.kalypsodeegree.graphics.sld.StyledLayerDescriptor;
import org.kalypsodeegree.graphics.sld.Symbolizer;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.xml.XMLTools;
import org.kalypsodeegree_impl.graphics.sld.ColorMapEntry_Impl;
import org.kalypsodeegree_impl.graphics.sld.FeatureTypeStyle_Impl;
import org.kalypsodeegree_impl.graphics.sld.RasterSymbolizer_Impl;
import org.kalypsodeegree_impl.graphics.sld.SLDFactory;
import org.kalypsodeegree_impl.graphics.sld.StyleFactory;
import org.kalypsodeegree_impl.graphics.sld.StyledLayerDescriptor_Impl;
import org.kalypsodeegree_impl.graphics.sld.UserStyle_Impl;
import org.kalypsodeegree_impl.io.shpapi.ShapeFile;
import org.kalypsodeegree_impl.model.cv.RangeSetTypeHandler;
import org.kalypsodeegree_impl.model.cv.RectifiedGridCoverage;
import org.kalypsodeegree_impl.model.cv.RectifiedGridDomainTypeHandler;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
import org.kalypsodeegree_impl.model.feature.GMLWorkspace_Impl;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.opengis.cs.CS_CoordinateSystem;
import org.w3c.dom.Document;

/**
 * CreateFloodRiskProjectJob
 * <p>
 * Job for creating a floodrisk project created by
 * 
 * @author Nadja Peiler (13.06.2005)
 */
public class CreateFloodRiskProjectJob extends Job
{
  private static final org.kalypso.template.gismapview.ObjectFactory mapTemplateOF = new org.kalypso.template.gismapview.ObjectFactory();
  private static final org.kalypso.template.types.ObjectFactory typeOF = new org.kalypso.template.types.ObjectFactory();
  private static final JAXBContext JC = JaxbUtilities.createQuiet( org.kalypso.template.gismapview.ObjectFactory.class, org.kalypso.template.types.ObjectFactory.class );
  private final String m_resourceBase = "resources/projecttemplate.zip"; //$NON-NLS-1$
  private List<StyledLayerType> m_layerList;
  private GMLWorkspace m_landuseShapeWS;
  private IPath m_workspacePath;
  private IProject m_projectHandel;
  private File m_landuseDataFile;
  private String m_landusePropertyName;
  private CS_CoordinateSystem m_landuseCooSystem;
  private boolean m_autogenerateLanduseCollection;
  private Vector m_waterlevelGrids;
  private CS_CoordinateSystem m_waterlevelCooSystem;
  private final GMLSchemaCatalog schemaCatalog = KalypsoGMLSchemaPlugin.getDefault().getSchemaCatalog();

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
  public CreateFloodRiskProjectJob( String name, IPath workspacePath, IProject projectHandel, File landuseDataFile, String landusePropertyName, CS_CoordinateSystem landuseCooSystem, boolean autogenerateLanduseCollection, Vector waterlevelGrids, CS_CoordinateSystem waterlevelCooSystem )
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
  
    /*
    final ITypeRegistry<IMarshallingTypeHandler> registry = MarshallingTypeRegistrySingleton.getTypeRegistry();
    TypeHandlerUtilities.registerXSDSimpleTypeHandler(registry);
    try
    {
      registry.registerTypeHandler( new RangeSetTypeHandler() );
      registry.registerTypeHandler( new RectifiedGridDomainTypeHandler() );
      TypeHandlerUtilities.registerTypeHandlers(registry);
    }
    catch( TypeRegistryException e )
    {
      e.printStackTrace();
    }
    //registry.toString();
     * 
     */

  
  
  
  }

  /**
   * @see org.eclipse.core.internal.jobs.InternalJob#run(org.eclipse.core.runtime.IProgressMonitor)
   */
  @Override
  protected IStatus run( IProgressMonitor monitor )
  {
    return createProject( monitor );
  }

  /**
   * creates the project
   * 
   * @param monitor
   * @return status of process
   */
  protected IStatus createProject( IProgressMonitor monitor )
  {
    int totalWork = 100;
    monitor.beginTask( WizardMessages.getString("CreateFloodRiskProjectJob.CreateProject.Title"), totalWork ); //$NON-NLS-1$

    try
    {
      m_projectHandel.create( null );
      m_projectHandel.open( null );
      // set charSet for the new project to the UTF-8 standard
      m_projectHandel.setDefaultCharset( "UTF-8", null ); //$NON-NLS-1$
    }
    catch( CoreException e )
    {
      e.printStackTrace();
      performCancle();
      return e.getStatus();
    }

    try
    {
      // copy all the resources to the workspace into the new created project
      monitor.subTask(WizardMessages.getString("CreateFloodRiskProjectJob.monitor.0")+"..."); //$NON-NLS-1$ //$NON-NLS-2$
      copyResourcesToProject( m_workspacePath.append( m_projectHandel.getFullPath() ) );
      createEmtyFolders();
      monitor.worked( 10 );
      if( monitor.isCanceled() )
      {
        performCancle();
        return Status.CANCEL_STATUS;
      }

      // create Map(Waterlevel.gmt) with waterlevelGrids and shape of landuse
      String path = m_workspacePath.append( m_projectHandel.getFullPath() + "/Waterlevel/Waterlevel.gmt" ).toFile().toString(); //$NON-NLS-1$

      final Gismapview gismapview = mapTemplateOF.createGismapview();
      final Layers layers = mapTemplateOF.createGismapviewLayers();
      ExtentType extent = typeOF.createExtentType();
      m_layerList = layers.getLayer();

      // copy landuseData as shape
      copyLanduseShape();
      final StyledLayerType landuseLayer = createLanduseLayer( m_landuseDataFile );
      m_layerList.add( landuseLayer );
      layers.setActive( landuseLayer );

      IKalypsoTheme dummyLanduseTheme = createDummyLanduseTheme();
      GM_Envelope bbox = dummyLanduseTheme.getBoundingBox();
      if(bbox == null) bbox = GeometryFactory.createGM_Envelope(0.0, 0.0, 1.0, 1.0);
      extent.setLeft( bbox.getMin().getX() );
      extent.setBottom( bbox.getMin().getY() );
      extent.setRight( bbox.getMax().getX() );
      extent.setTop( bbox.getMax().getY() );
      extent.setSrs( m_landuseCooSystem.getName() );

      monitor.worked( 40 );
      if( monitor.isCanceled() )
      {
        performCancle();
        return Status.CANCEL_STATUS;
      }

      // generate landuseData as gml
      monitor.subTask(WizardMessages.getString("CreateFloodRiskProjectJob.monitor.2")+"..."); //$NON-NLS-1$ //$NON-NLS-2$
      HashSet landuseTypeSet = createLanduseDataGML();

      // create landuseCollection in contextModel and riskContextModel if
      // necessary
      if( m_autogenerateLanduseCollection )
      {
        monitor.subTask(WizardMessages.getString("CreateFloodRiskProjectJob.monitor.4")+"..."); //$NON-NLS-1$ //$NON-NLS-2$
        autogenerateLanduseCollection( landuseTypeSet );
      }
      // create waterlevelGrids and defaultStyles
      monitor.subTask(WizardMessages.getString("CreateFloodRiskProjectJob.monitor.6")+"..."); //$NON-NLS-1$ //$NON-NLS-2$
      /*
      Vector targetFiles = createWaterlevelGrids( monitor );
      
      if( targetFiles == null )
      {
        performCancle();
        return Status.CANCEL_STATUS;
      }
      */
      monitor.subTask(WizardMessages.getString("CreateFloodRiskProjectJob.monitor.8")+"..."); //$NON-NLS-1$ //$NON-NLS-2$
      //createWaterlevelData( targetFiles );
      if( monitor.isCanceled() )
      {
        performCancle();
        return Status.CANCEL_STATUS;
      }
      
      gismapview.setExtent( extent );
      gismapview.setLayers( layers );
      
      final Marshaller marshaller = JaxbUtilities.createMarshaller( JC );
      marshaller.setProperty( Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE );
      //FileWriter fw = new FileWriter( path );
      //marshaller.marshal( gismapview, fw );
      //fw.close();
    }
    catch( Exception e2 )
    {
      e2.printStackTrace();
      performCancle();
      return new Status( IStatus.ERROR, KalypsoGisPlugin.getId(), 0, e2.getMessage(), e2 );
    }

    // refresh project
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
   */
  private void copyResourcesToProject( IPath path ) throws IOException
  {
    final String resource = m_resourceBase;
    System.out.print( "resource: " + resource + "\n" ); //$NON-NLS-1$ //$NON-NLS-2$
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
   * creates the empty folders "Damage", "Risk" and "Statistic"
   */
  private void createEmtyFolders( )
  {
    // Damage
    File damageDir = (m_workspacePath.append( m_projectHandel.getFullPath().append( "/Damage" ) )).toFile(); //$NON-NLS-1$
    damageDir.mkdir();
    // Risk
    File riskDir = (m_workspacePath.append( m_projectHandel.getFullPath().append( "/Risk" ) )).toFile(); //$NON-NLS-1$
    riskDir.mkdir();
    // Statistic
    File statisticDir = (m_workspacePath.append( m_projectHandel.getFullPath().append( "/Statistic" ) )).toFile(); //$NON-NLS-1$
    statisticDir.mkdir();
  }

  /**
   * copies the landuse shapeBase files to the folder "Landuse" in project
   * 
   * @throws IOException
   */
  private void copyLanduseShape( ) throws IOException
  {
    String landuseSourceBase = FileUtilities.nameWithoutExtension( m_landuseDataFile.toString() );
    File targetDir = (m_workspacePath.append( m_projectHandel.getFullPath().append( "/Landuse/" ) )).toFile(); //$NON-NLS-1$

    File shp = new File( landuseSourceBase + ".shp" ); //$NON-NLS-1$
    FileUtils.copyFileToDirectory( shp, targetDir );

    File dbf = new File( landuseSourceBase + ".dbf" ); //$NON-NLS-1$
    FileUtils.copyFileToDirectory( dbf, targetDir );

    File shx = new File( landuseSourceBase + ".shx" ); //$NON-NLS-1$
    FileUtils.copyFileToDirectory( shx, targetDir );

  }

  /**
   * creates a DummyTheme for landuse to calculate the extent of the created map (Waterlevel.gmt)
   * 
   * @return KalypsoFeatureTheme landuse
   * @throws GmlSerializeException
   */
  private IKalypsoTheme createDummyLanduseTheme( ) throws GmlSerializeException
  {
    String shapeBase = (m_workspacePath.append( m_projectHandel.getFullPath().append( "/Landuse/" + FileUtilities.nameWithoutExtension( m_landuseDataFile.getName().toString() ) ) )).toString(); //$NON-NLS-1$
    m_landuseShapeWS = ShapeSerializer.deserialize( shapeBase, m_landuseCooSystem );
    KalypsoFeatureTheme kft = new KalypsoFeatureTheme( new CommandableWorkspace( m_landuseShapeWS ), "featureMember", WizardMessages.getString("CreateFloodRiskProjectJob.Landuse"), new FeatureSelectionManager2(), null ); //$NON-NLS-1$ //$NON-NLS-2$
    //ShapeFile sf = new ShapeFile( shapeBase );
    //sf.getFileMBR();
    return kft;
  }

  /**
   * creates the gml version of the landuseData (LanduseVectorData.gml, Schema: VectorDataModel.xsd)
   * 
   * @return Set of landuse types (HashSet)
   * @throws IOException
   * @throws GmlSerializeException
   */
  private HashSet createLanduseDataGML( ) throws IOException, GmlSerializeException, InvocationTargetException
  {

    File landuseDataGML = m_workspacePath.append( m_projectHandel.getFullPath() + "/Landuse/LanduseVectorData.gml" ).toFile(); //$NON-NLS-1$

    // load schema
    //final GMLSchema schema = schemaCatalog.getSchema( UrlCatalogFloodRisk.NS_VECTORDATAMODEL, "3.1" );
    final GMLSchema schema = schemaCatalog.getSchema( UrlCatalogFloodRisk.NS_VECTORDATAMODEL, (String) null );

    QName rootFeatureTypeName          = new QName(UrlCatalogFloodRisk.NS_VECTORDATAMODEL, "VectorDataCollection"); //$NON-NLS-1$
    QName featureTypePropertyName      = new QName(UrlCatalogFloodRisk.NS_VECTORDATAMODEL, "FeatureMember"); //$NON-NLS-1$
    QName shapeFeatureTypePropertyName = new QName("namespace", "featureMember"); //$NON-NLS-1$ //$NON-NLS-2$
    QName shapeGeomPropertyName        = new QName("namespace", "GEOM"); //$NON-NLS-1$ //$NON-NLS-2$
    QName propertyName                 = new QName("namespace", m_landusePropertyName); //$NON-NLS-1$

    // create feature and workspace gml
    final IFeatureType[] types = schema.getAllFeatureTypes();

    IFeatureType rootFeatureType = schema.getFeatureType( rootFeatureTypeName );
    
    Feature rootFeature = FeatureFactory.createFeature( null, rootFeatureTypeName + "0", rootFeatureType, true ); //$NON-NLS-1$
    IPropertyType ftp_feature = rootFeatureType.getProperty( featureTypePropertyName );

    // create features: Feature
    Feature shapeRootFeature = m_landuseShapeWS.getRootFeature();
    
    List featureList = (List) shapeRootFeature.getProperty( shapeFeatureTypePropertyName );
    final HashSet<String> landuseTypeSet = new HashSet<String>();
    for( int i = 0; i < featureList.size(); i++ )
    {
      final Feature feat = (Feature) featureList.get( i );
      final String propertyValue = (String) feat.getProperty( propertyName );
      if( !landuseTypeSet.contains( propertyValue ) )
      {
        landuseTypeSet.add( propertyValue );
      }
      Object[] properties = new Object[] { null, null, null, null, null, (GM_Object) feat.getProperty( shapeGeomPropertyName ), propertyValue };
      //IFeatureType aaa = ((IRelationType) ftp_feature).getTargetFeatureType();
      final Feature feature = FeatureFactory.createFeature( rootFeature, "Feature" + i, ((IRelationType) ftp_feature).getTargetFeatureType(), properties ); //$NON-NLS-1$
      FeatureHelper.addProperty( rootFeature, ftp_feature, feature );
    }

    // create workspace
    final GMLWorkspace workspace = new GMLWorkspace_Impl( schema, types, rootFeature, landuseDataGML.toURL(), "", null ); //$NON-NLS-1$

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
   */
  private void autogenerateLanduseCollection( HashSet landuseTypeSet ) throws Exception
  {
    URL contextModelURL     = m_workspacePath.append( m_projectHandel.getFullPath() + "/Control/contextModell.gml" ).toFile().toURL(); //$NON-NLS-1$
    URL riskContextModelURL = m_workspacePath.append( m_projectHandel.getFullPath() + "/Control/riskContextModell.gml" ).toFile().toURL(); //$NON-NLS-1$

    QName landuseFeatureType        = new QName(UrlCatalogFloodRisk.NS_CONTEXTMODEL, "Landuse"); //$NON-NLS-1$
    QName landuseCollectionType     = new QName(UrlCatalogFloodRisk.NS_CONTEXTMODEL, "LanduseCollection"); //$NON-NLS-1$
    QName parentFeatureName         = new QName(UrlCatalogFloodRisk.NS_CONTEXTMODEL, "LanduseCollectionMember"); //$NON-NLS-1$
    QName propertyName              = new QName(UrlCatalogFloodRisk.NS_CONTEXTMODEL, "Name"); //$NON-NLS-1$
    QName propertyLanduseMember     = new QName(UrlCatalogFloodRisk.NS_CONTEXTMODEL, "LanduseMember"); //$NON-NLS-1$
    QName landuseFeatureTypeRisk    = new QName(UrlCatalogFloodRisk.NS_RISKCONTEXTMODEL, "Landuse"); //$NON-NLS-1$
    QName landuseCollectionTypeRisk = new QName(UrlCatalogFloodRisk.NS_RISKCONTEXTMODEL, "LanduseCollection"); //$NON-NLS-1$
    QName parentFeatureNameRisk     = new QName(UrlCatalogFloodRisk.NS_RISKCONTEXTMODEL, "LanduseCollectionMember"); //$NON-NLS-1$
    QName propertyNameRisk          = new QName(UrlCatalogFloodRisk.NS_RISKCONTEXTMODEL, "Name"); //$NON-NLS-1$
    QName propertyLanduseMemberRisk = new QName(UrlCatalogFloodRisk.NS_RISKCONTEXTMODEL, "LanduseMember"); //$NON-NLS-1$

    // contextModel
    GMLWorkspace contextModel = GmlSerializer.createGMLWorkspace( contextModelURL, null );
    
    final GMLSchema schema      = schemaCatalog.getSchema( UrlCatalogFloodRisk.NS_CONTEXTMODEL, (String)null );
    IFeatureType ftLanduse = schema.getFeatureType(landuseFeatureType);
    IFeatureType ftLanduseCollection = schema.getFeatureType(landuseCollectionType);
    final IPropertyType featureProperty = ftLanduse.getProperty( propertyName );
    Feature rootFeature = contextModel.getRootFeature();
    Feature parentFeature = (Feature) rootFeature.getProperty( parentFeatureName );
    IRelationType featurePropertyName = (IRelationType) ftLanduseCollection.getProperty(propertyLanduseMember);
    
    // riskContextModel
    final GMLSchema schemaRisk = schemaCatalog.getSchema( UrlCatalogFloodRisk.NS_RISKCONTEXTMODEL, (String)null );
    IFeatureType ftLanduseRisk = schemaRisk.getFeatureType( landuseFeatureTypeRisk );
    IFeatureType ftLanduseCollectionRisk = schemaRisk.getFeatureType(landuseCollectionTypeRisk);
    final IPropertyType featurePropertyRisk = ftLanduseRisk.getProperty( propertyNameRisk );
    GMLWorkspace riskContextModel = GmlSerializer.createGMLWorkspace( riskContextModelURL, null );
    Feature rootFeature_risk = riskContextModel.getRootFeature();
    Feature parentFeature_risk = (Feature) rootFeature_risk.getProperty( parentFeatureNameRisk );
    IRelationType featurePropertyNameRisk = (IRelationType) ftLanduseCollectionRisk.getProperty(propertyLanduseMemberRisk);

    Iterator it = landuseTypeSet.iterator();
    while( it.hasNext() )
    {
      final String landusePropertyName = (String) it.next();
      // contextModel
      final Feature landuseFeature = contextModel.createFeature( parentFeature, ftLanduse );
      landuseFeature.setProperty( featureProperty, landusePropertyName );
      contextModel.addFeatureAsComposition( parentFeature, featurePropertyName, 0, landuseFeature );
      // riskContextModel
      Feature landuseFeature_risk = riskContextModel.createFeature( parentFeature_risk, ftLanduseRisk );
      landuseFeature_risk.setProperty( featurePropertyRisk, landusePropertyName );
      riskContextModel.addFeatureAsComposition( parentFeature_risk, featurePropertyNameRisk, 0, landuseFeature_risk );
    }
    // save changes
    // contextModel
    FileWriter fw = new FileWriter( contextModelURL.getFile() );
    GmlSerializer.serializeWorkspace( fw, contextModel );
    fw.close();
    // riskContextModel
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
   */
  private Vector createWaterlevelGrids( IProgressMonitor monitor ) throws Exception
  {
    final Vector<File> targetFiles = new Vector<File>();
    int workedPart = 50 / m_waterlevelGrids.size();
    for( int i = 0; i < m_waterlevelGrids.size(); i++ )
    {
      File sourceFile = (File) m_waterlevelGrids.get( i );
      RectifiedGridCoverage grid = GridUtils.importGridArc( sourceFile, m_waterlevelCooSystem );
      String sourceFileNameWithoutExtension = FileUtilities.nameWithoutExtension( sourceFile.getName() );
      File waterlevelDir = (m_workspacePath.append( m_projectHandel.getFullPath().append( "/Waterlevel" ) )).toFile(); //$NON-NLS-1$
      waterlevelDir.mkdir();
      final File targetFile = (m_workspacePath.append( m_projectHandel.getFullPath().append( "/Waterlevel/" + sourceFileNameWithoutExtension + ".gml" ) )).toFile(); //$NON-NLS-1$ //$NON-NLS-2$
      GridUtils.writeRasterData( targetFile, grid );
      targetFiles.add( targetFile );
      File sldFile = (m_workspacePath.append( m_projectHandel.getFullPath().append( "/.styles/" + sourceFileNameWithoutExtension + ".sld" ) )).toFile(); //$NON-NLS-1$ //$NON-NLS-2$
      Color lightBlue = new Color( 150, 150, 255 );
      int numOfCategories = 5;
      createRasterStyle( sldFile, sourceFileNameWithoutExtension, grid, lightBlue, numOfCategories );
      m_layerList.add( createWaterlevelLayer( targetFile, sourceFileNameWithoutExtension ) );
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
   */
  private void createRasterStyle( File resultFile, String styleName, RectifiedGridCoverage grid, Color color, int numOfCategories ) throws Exception
  {
    final TreeMap<Double, ColorMapEntry> colorMap = new TreeMap<Double, ColorMapEntry>();
    ColorMapEntry colorMapEntry_noData = new ColorMapEntry_Impl( Color.WHITE, 0, -9999, WizardMessages.getString("CreateFloodRiskProjectJob.NoData") ); //$NON-NLS-1$
    colorMap.put( new Double( -9999 ), colorMapEntry_noData );
    double min = grid.getRangeSet().getMinValue();
    double max = grid.getRangeSet().getMaxValue();
    double intervalStep = (max - min) / numOfCategories;
    for( int i = 0; i < numOfCategories; i++ )
    {
      double quantity = Number.round( (min + (i * intervalStep)), 4, BigDecimal.ROUND_HALF_EVEN );
      ColorMapEntry colorMapEntry = new ColorMapEntry_Impl( color, 1, quantity, "" ); //$NON-NLS-1$
      color = color.darker();
      colorMap.put( new Double( quantity ), colorMapEntry );
    }
    ColorMapEntry colorMapEntry_max = new ColorMapEntry_Impl( Color.WHITE, 1, max, "" ); //$NON-NLS-1$
    colorMap.put( new Double( max ), colorMapEntry_max );
    RasterSymbolizer rasterSymbolizer = new RasterSymbolizer_Impl( colorMap );
    Symbolizer[] symbolizers = new Symbolizer[] { rasterSymbolizer };
    FeatureTypeStyle featureTypeStyle = new FeatureTypeStyle_Impl();
    double minScaleDenominator = 0;
    double maxScaleDenominator = 1.8;
    Rule rule = StyleFactory.createRule( symbolizers, "default", "default", "default", minScaleDenominator, maxScaleDenominator ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    featureTypeStyle.addRule( rule );
    FeatureTypeStyle[] featureTypeStyles = new FeatureTypeStyle[] { featureTypeStyle };
    org.kalypsodeegree.graphics.sld.Style[] styles = new org.kalypsodeegree.graphics.sld.Style[] { new UserStyle_Impl( styleName, styleName, null, false, featureTypeStyles ) };
    org.kalypsodeegree.graphics.sld.Layer[] layers = new org.kalypsodeegree.graphics.sld.Layer[] { SLDFactory.createNamedLayer( "deegree style definition", null, styles ) }; //$NON-NLS-1$
    StyledLayerDescriptor sld = SLDFactory.createStyledLayerDescriptor( layers, "1.0" ); //$NON-NLS-1$
    System.out.println(((StyledLayerDescriptor_Impl) sld).exportAsXML());
    Document doc = XMLTools.parse( new StringReader( ((StyledLayerDescriptor_Impl) sld).exportAsXML() ) );
    final Source source = new DOMSource( doc );
    Result result = new StreamResult( resultFile );
    Transformer t = TransformerFactory.newInstance().newTransformer();
    t.setOutputProperty( "{http://xml.apache.org/xslt}indent-amount", "2" ); //$NON-NLS-1$ //$NON-NLS-2$
    t.setOutputProperty( OutputKeys.INDENT, "yes" ); //$NON-NLS-1$
    t.transform( source, result );
  }

  /**
   * creates the control file waterlevelData.gml with all given waterlevelGrids as parameters
   * 
   * @param targetFiles
   * @throws IOException
   * @throws GmlSerializeException
   */
  private void createWaterlevelData( Vector targetFiles ) throws IOException, GmlSerializeException, InvocationTargetException
  {
    File waterlevelDataFile = m_workspacePath.append( m_projectHandel.getFullPath() + "/Control/waterlevelData.gml" ).toFile(); //$NON-NLS-1$

    // load schema
    //final GMLSchema schema = GMLSchemaCatalog.getSchema( UrlCatalogFloodRisk.NS_WATERLEVELDATA, (String)null );
    //  register typeHandler
    final ITypeRegistry<IMarshallingTypeHandler> registry = MarshallingTypeRegistrySingleton.getTypeRegistry();
    try
    {
      registry.registerTypeHandler( new RangeSetTypeHandler() );
      registry.registerTypeHandler( new RectifiedGridDomainTypeHandler() );
    }
    catch( TypeRegistryException e )
    {
      e.printStackTrace();
    }
      final GMLSchemaCatalog schemaCatalog2 = KalypsoGMLSchemaPlugin.getDefault().getSchemaCatalog();
      
    final GMLSchema schema = schemaCatalog2.getSchema( UrlCatalogFloodRisk.NS_WATERLEVELDATA, (String)null );

    //final IFeatureType[] types = schema.getAllFeatureTypes();

    // create rootFeature
    QName rootFeatureName = new QName(UrlCatalogFloodRisk.NS_WATERLEVELDATA, "WaterlevelData"); //$NON-NLS-1$
    QName rootFeatureProp = new QName(UrlCatalogFloodRisk.NS_WATERLEVELDATA, "WaterlevelMember"); //$NON-NLS-1$
    IFeatureType rootFeatureType = schema.getFeatureType( rootFeatureName );
    Feature rootFeature = FeatureFactory.createFeature( null, "WaterlevelData0", rootFeatureType, true ); //$NON-NLS-1$
    final IRelationType waterlevelMember = (IRelationType) rootFeatureType.getProperty( rootFeatureProp );
    // create waterlevelFeature(s)
    QName waterlevelFeatureName = new QName(UrlCatalogFloodRisk.NS_WATERLEVELDATA, "Waterlevel"); //$NON-NLS-1$
    QName waterlevelFeatureProp = new QName(UrlCatalogFloodRisk.NS_WATERLEVELDATA, "WaterlevelRasterData"); //$NON-NLS-1$
    IFeatureType waterlevelFeatureType = schema.getFeatureType( waterlevelFeatureName );
    final IPropertyType featureProperty = waterlevelFeatureType.getProperty( waterlevelFeatureProp );
    int identifier = 0;

    //final ITypeRegistry<IMarshallingTypeHandler> registry = MarshallingTypeRegistrySingleton.getTypeRegistry();

    //final IMarshallingTypeHandler wlTH = registry.getTypeHandlerFor(featureProperty);
    for( int i = 0; i < targetFiles.size(); i++ )
    {
      final Feature waterlevelFeature = FeatureFactory.createFeature( rootFeature, waterlevelFeatureName.getLocalPart() + identifier, waterlevelFeatureType, true );
      IFile waterlevelFile = ResourceUtilities.findFileFromURL( ((File) targetFiles.get( i )).toURL() );
      waterlevelFeature.setProperty( featureProperty, waterlevelFile );
      FeatureHelper.addProperty( rootFeature, waterlevelMember, waterlevelFeature );
      identifier = identifier + 1;
    }

    // create workspace
    IFeatureType[] types = schema.getAllFeatureTypes();
    final GMLWorkspace workspace = new GMLWorkspace_Impl( schema, types, rootFeature, waterlevelDataFile.toURL(), "", null ); //$NON-NLS-1$
    
    //IPath m_modelPath = new Path( m_workspacePath.append( m_projectHandel.getFullPath() + "/Control/waterlevelData.gml" ).toString() );
    //URL modelURL = new URL( ResourceUtilities.createURLSpec( m_modelPath ) );

    //final GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( modelURL, new UrlResolver() );
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
   */
  private StyledLayerType createLanduseLayer( final File sourceFile ) throws Exception
  {
    final StyledLayerType newLayer = typeOF.createStyledLayerType();

    // set attributes for the layer
    newLayer.setName( WizardMessages.getString("CreateFloodRiskProjectJob.Landuse") ); //$NON-NLS-1$
    newLayer.setVisible( true );
    newLayer.setFeaturePath( "featureMember" ); //$NON-NLS-1$
    newLayer.setHref( "project:/Landuse/" + FileUtilities.nameWithoutExtension( sourceFile.getName() ) + "#" + m_landuseCooSystem.getName() ); //$NON-NLS-1$ //$NON-NLS-2$
    newLayer.setType( "simple" ); //$NON-NLS-1$
    newLayer.setLinktype( "shape" ); //$NON-NLS-1$
    newLayer.setActuate( "onRequest" ); //$NON-NLS-1$
    newLayer.setId( "ID_1" ); //$NON-NLS-1$

    final List<Style> styleList = newLayer.getStyle();
    final Style style = typeOF.createStyledLayerTypeStyle();

    // set attributes for the style
    style.setLinktype( "sld" ); //$NON-NLS-1$
    style.setStyle( WizardMessages.getString("CreateFloodRiskProjectJob.Landuse") ); //$NON-NLS-1$
    style.setActuate( "onRequest" ); //$NON-NLS-1$
    style.setHref( "../.styles/landuse.sld" ); //$NON-NLS-1$
    style.setType( "simple" ); //$NON-NLS-1$

    // add the style to the layer
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
   */
  private StyledLayerType createWaterlevelLayer( File sourceFile, String styleName ) throws Exception
  {

    final StyledLayerType newLayer = typeOF.createStyledLayerType();

    // set attributes for the layer
    newLayer.setName( styleName );
    newLayer.setVisible( false );
    newLayer.setFeaturePath( "RectifiedGridCoverageMember" ); //$NON-NLS-1$
    newLayer.setHref( "../Waterlevel/" + sourceFile.getName() ); //$NON-NLS-1$
    newLayer.setType( "simple" ); //$NON-NLS-1$
    newLayer.setLinktype( "gml" ); //$NON-NLS-1$
    newLayer.setActuate( "onRequest" ); //$NON-NLS-1$
    newLayer.setId( "ID_" + id ); //$NON-NLS-1$
    id = id + 1;

    final List<Style> styleList = newLayer.getStyle();
    final Style style = typeOF.createStyledLayerTypeStyle();

    // set attributes for the style
    style.setLinktype( "sld" ); //$NON-NLS-1$
    style.setStyle( styleName );
    style.setActuate( "onRequest" ); //$NON-NLS-1$
    style.setHref( "../.styles/" + styleName + ".sld" ); //$NON-NLS-1$ //$NON-NLS-2$
    style.setType( "simple" ); //$NON-NLS-1$

    // add the style to the layer
    styleList.add( style );

    return newLayer;
  }

  /**
   * deletes the project, when job is canceled
   */
  public boolean performCancle( )
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