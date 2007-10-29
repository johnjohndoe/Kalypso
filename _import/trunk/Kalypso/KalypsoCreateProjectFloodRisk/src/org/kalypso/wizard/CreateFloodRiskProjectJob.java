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
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.StringReader;
import java.lang.reflect.InvocationTargetException;
import java.math.BigDecimal;
import java.net.URL;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.TreeMap;
import java.util.Vector;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBElement;
import javax.xml.bind.Marshaller;
import javax.xml.namespace.QName;
import javax.xml.transform.OutputKeys;
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
import org.kalypso.core.jaxb.TemplateUtilitites;
import org.kalypso.floodrisk.schema.UrlCatalogFloodRisk;
import org.kalypso.floodrisk.tools.Number;
import org.kalypso.gmlschema.GMLSchema;
import org.kalypso.gmlschema.GMLSchemaCatalog;
import org.kalypso.gmlschema.KalypsoGMLSchemaPlugin;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.grid.MinMaxRasterWalker;
import org.kalypso.jwsdp.JaxbUtilities;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.KalypsoFeatureTheme;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.serialize.GmlSerializeException;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ogc.gml.serialize.ShapeSerializer;
import org.kalypso.template.gismapview.Gismapview;
import org.kalypso.template.gismapview.Gismapview.Layers;
import org.kalypso.template.types.ExtentType;
import org.kalypso.template.types.StyledLayerType;
import org.kalypso.template.types.StyledLayerType.Style;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypsodeegree.filterencoding.Filter;
import org.kalypsodeegree.filterencoding.Operation;
import org.kalypsodeegree.graphics.sld.ColorMapEntry;
import org.kalypsodeegree.graphics.sld.FeatureTypeStyle;
import org.kalypsodeegree.graphics.sld.Fill;
import org.kalypsodeegree.graphics.sld.Geometry;
import org.kalypsodeegree.graphics.sld.PolygonSymbolizer;
import org.kalypsodeegree.graphics.sld.RasterSymbolizer;
import org.kalypsodeegree.graphics.sld.Rule;
import org.kalypsodeegree.graphics.sld.Stroke;
import org.kalypsodeegree.graphics.sld.StyledLayerDescriptor;
import org.kalypsodeegree.graphics.sld.Symbolizer;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.xml.XMLTools;
import org.kalypsodeegree_impl.filterencoding.ComplexFilter;
import org.kalypsodeegree_impl.filterencoding.Literal;
import org.kalypsodeegree_impl.filterencoding.PropertyIsLikeOperation;
import org.kalypsodeegree_impl.filterencoding.PropertyName;
import org.kalypsodeegree_impl.gml.binding.commons.RectifiedGridCoverage;
import org.kalypsodeegree_impl.graphics.sld.ColorMapEntry_Impl;
import org.kalypsodeegree_impl.graphics.sld.FeatureTypeStyle_Impl;
import org.kalypsodeegree_impl.graphics.sld.Geometry_Impl;
import org.kalypsodeegree_impl.graphics.sld.PolygonSymbolizer_Impl;
import org.kalypsodeegree_impl.graphics.sld.RasterSymbolizer_Impl;
import org.kalypsodeegree_impl.graphics.sld.SLDFactory;
import org.kalypsodeegree_impl.graphics.sld.StyleFactory;
import org.kalypsodeegree_impl.graphics.sld.StyledLayerDescriptor_Impl;
import org.kalypsodeegree_impl.graphics.sld.UserStyle_Impl;
import org.kalypsodeegree_impl.graphics.sld.Symbolizer_Impl.UOM;
import org.kalypsodeegree_impl.io.shpapi.ShapeFile;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
import org.kalypsodeegree_impl.model.feature.GMLWorkspace_Impl;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.opengis.cs.CS_CoordinateSystem;
import org.w3c.dom.Document;

/**
 * Job for creating a floodrisk project
 * 
 * @author Nadja Peiler
 * @author Dejan Antanaskovic, <a href="mailto:dejan.antanaskovic@tuhh.de">dejan.antanaskovic@tuhh.de</a>
 */
public class CreateFloodRiskProjectJob extends Job
{
  private static final org.kalypso.template.gismapview.ObjectFactory mapTemplateOF = new org.kalypso.template.gismapview.ObjectFactory();

  private static final org.kalypso.template.types.ObjectFactory typeOF = new org.kalypso.template.types.ObjectFactory();

  private static final JAXBContext JC = JaxbUtilities.createQuiet( org.kalypso.template.gismapview.ObjectFactory.class, org.kalypso.template.types.ObjectFactory.class );

  private final String m_resourceBase = "resources/projecttemplate.zip"; //$NON-NLS-1$

  private List<JAXBElement< ? extends StyledLayerType>> m_layerList;

  private GMLWorkspace m_landuseShapeWS;

  private final IPath m_workspacePath;

  private final IProject m_projectHandle;

  private final File m_landuseDataFile;

  private final String m_landusePropertyName;

  private final CS_CoordinateSystem m_landuseCooSystem;

  private final boolean m_autogenerateLanduseCollection;

  private final Vector m_waterlevelGrids;

  private final CS_CoordinateSystem m_waterlevelCooSystem;

  private final GMLSchemaCatalog schemaCatalog = KalypsoGMLSchemaPlugin.getDefault().getSchemaCatalog();

  private GM_Envelope m_boundingBox;

  private final HashSet<String> m_landuseTypeSet = new HashSet<String>();

  /**
   * Constructor, gets all information needed for creating the project
   * 
   * @param name
   * @param workspacePath
   * @param projectHandel
   * @param landuseDataFile
   *            Shape file
   * @param landusePropertyName
   * @param landuseCooSystem
   * @param autogenerateLanduseCollection
   *            flag; true: generate LanduseCollection in ContextModel and RiskContextModel, false: not
   * @param waterlevelGrids
   *            Ascii files
   * @param waterlevelCooSystem
   */
  public CreateFloodRiskProjectJob( final String name, final IPath workspacePath, final IProject projectHandle, final File landuseDataFile, final String landusePropertyName, final CS_CoordinateSystem landuseCooSystem, final boolean autogenerateLanduseCollection, final Vector waterlevelGrids, final CS_CoordinateSystem waterlevelCooSystem )
  {
    super( name );
    m_workspacePath = workspacePath;
    m_projectHandle = projectHandle;
    m_landuseDataFile = landuseDataFile;
    m_landusePropertyName = landusePropertyName;
    m_landuseCooSystem = landuseCooSystem;
    m_autogenerateLanduseCollection = autogenerateLanduseCollection;
    m_waterlevelGrids = waterlevelGrids;
    m_waterlevelCooSystem = waterlevelCooSystem;
    m_boundingBox = null;
  }

  /**
   * @see org.eclipse.core.internal.jobs.InternalJob#run(org.eclipse.core.runtime.IProgressMonitor)
   */
  @Override
  protected IStatus run( final IProgressMonitor monitor )
  {
    return createProject( monitor );
  }

  /**
   * creates the project
   * 
   * @param monitor
   * @return status of process
   */
  protected IStatus createProject( final IProgressMonitor monitor )
  {
    final int totalWork = 100;
    monitor.beginTask( WizardMessages.getString( "CreateFloodRiskProjectJob.CreateProject.Title" ), totalWork ); //$NON-NLS-1$

    try
    {
      m_projectHandle.create( null );
      m_projectHandle.open( null );
      m_projectHandle.setDefaultCharset( "UTF-8", null ); //$NON-NLS-1$
    }
    catch( final CoreException e )
    {
      e.printStackTrace();
      performCancel();
      return e.getStatus();
    }

    try
    {
      // copy all the resources to the workspace into the new created project
      monitor.subTask( WizardMessages.getString( "CreateFloodRiskProjectJob.monitor.0" ) + "..." ); //$NON-NLS-1$ //$NON-NLS-2$
      copyResourcesToProject( m_workspacePath.append( m_projectHandle.getFullPath() ) );
      createFolders();
      monitor.worked( 10 );
      if( monitor.isCanceled() )
      {
        performCancel();
        return Status.CANCEL_STATUS;
      }

      final Gismapview gismapview = mapTemplateOF.createGismapview();
      final Layers layers = mapTemplateOF.createGismapviewLayers();
      final ExtentType extent = typeOF.createExtentType();
       m_layerList = layers.getLayer();

      // copy landuseData as shape
      copyLanduseShape();

//      createDummyLanduseTheme();
      if( m_boundingBox == null )
        m_boundingBox = GeometryFactory.createGM_Envelope( 0.0, 0.0, 1.0, 1.0 );

      extent.setLeft( m_boundingBox.getMin().getX() );
      extent.setBottom( m_boundingBox.getMin().getY() );
      extent.setRight( m_boundingBox.getMax().getX() );
      extent.setTop( m_boundingBox.getMax().getY() );
      extent.setSrs( m_landuseCooSystem.getName() );

      monitor.worked( 40 );
      if( monitor.isCanceled() )
      {
        performCancel();
        return Status.CANCEL_STATUS;
      }

      // generate landuseData as gml
      monitor.subTask( WizardMessages.getString( "CreateFloodRiskProjectJob.monitor.2" ) + "..." ); //$NON-NLS-1$ //$NON-NLS-2$
//      createLanduseDataGML();

      // create landuseCollection in contextModel and riskContextModel (if user checked this option in wizard)
//      if( m_autogenerateLanduseCollection )
//      {
//        monitor.subTask( WizardMessages.getString( "CreateFloodRiskProjectJob.monitor.4" ) + "..." ); //$NON-NLS-1$ //$NON-NLS-2$
//        autogenerateLanduseCollection();
//        createLanduseStyle( m_workspacePath.toOSString() + m_projectHandle.getFullPath() + "/.styles/landuse.sld" ); //$NON-NLS-1$
//      }
      // create waterlevelGrids and defaultStyles
      monitor.subTask( WizardMessages.getString( "CreateFloodRiskProjectJob.monitor.6" ) + "..." ); //$NON-NLS-1$ //$NON-NLS-2$

      final Vector targetFiles = createWaterlevelGrids( monitor );
      if( targetFiles == null )
      {
        performCancel();
        return Status.CANCEL_STATUS;
      }

//      final StyledLayerType landuseLayer = createLanduseLayer( m_landuseDataFile );
//      JAXBElement<StyledLayerType> layerElement = TemplateUtilitites.OF_GISMAPVIEW.createLayer( landuseLayer );
//      m_layerList.add( layerElement );
//      layers.setActive( landuseLayer );

      monitor.subTask( WizardMessages.getString( "CreateFloodRiskProjectJob.monitor.8" ) + "..." ); //$NON-NLS-1$ //$NON-NLS-2$
      createWaterlevelData( targetFiles );
      if( monitor.isCanceled() )
      {
        performCancel();
        return Status.CANCEL_STATUS;
      }

      gismapview.setExtent( extent );
      gismapview.setLayers( layers );

      final Marshaller marshaller = JaxbUtilities.createMarshaller( JC );
      marshaller.setProperty( Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE );

      // create Map(Waterlevel.gmt) with waterlevelGrids and shape of landuse
      final String path = m_workspacePath.append( m_projectHandle.getFullPath() + "/Waterlevel/Waterlevel.gmt" ).toFile().toString(); //$NON-NLS-1$
      final FileWriter fw = new FileWriter( path );
      marshaller.marshal( gismapview, fw );
      fw.close();
    }
    catch( final Exception e2 )
    {
      e2.printStackTrace();
      performCancel();
      return new Status( IStatus.ERROR, KalypsoGisPlugin.getId(), 0, e2.getMessage(), e2 );
    }

    // refresh project
    try
    {
      m_projectHandle.refreshLocal( IResource.DEPTH_INFINITE, null );
    }
    catch( final CoreException e1 )
    {
      e1.printStackTrace();
      // performCancle();
      // return e1.getStatus();
    }

    monitor.done();
    return Status.OK_STATUS;
  }

  /**
   * copies resources to project
   * 
   * @param path
   *            project path
   * @throws IOException
   */
  private void copyResourcesToProject( final IPath path ) throws IOException
  {
    final String resource = m_resourceBase;
    System.out.print( "resource: " + resource + "\n" ); //$NON-NLS-1$ //$NON-NLS-2$
    final InputStream resourceAsStream = getClass().getResourceAsStream( resource );
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
  private void createFolders( )
  {
    // Damage
    final File damageDir = (m_workspacePath.append( m_projectHandle.getFullPath().append( "/Damage" ) )).toFile(); //$NON-NLS-1$
    damageDir.mkdir();
    // Risk
    final File riskDir = (m_workspacePath.append( m_projectHandle.getFullPath().append( "/Risk" ) )).toFile(); //$NON-NLS-1$
    riskDir.mkdir();
    // Statistic
    final File statisticDir = (m_workspacePath.append( m_projectHandle.getFullPath().append( "/Statistic" ) )).toFile(); //$NON-NLS-1$
    statisticDir.mkdir();
  }

  /**
   * copies the landuse shapeBase files to the folder "Landuse" in project
   * 
   * @throws IOException
   */
  private void copyLanduseShape( ) throws IOException
  {
    final String landuseSourceBase = FileUtilities.nameWithoutExtension( m_landuseDataFile.toString() );
    final File targetDir = (m_workspacePath.append( m_projectHandle.getFullPath().append( "/Landuse/" ) )).toFile(); //$NON-NLS-1$

    final File shp = new File( landuseSourceBase + ".shp" ); //$NON-NLS-1$
    FileUtils.copyFileToDirectory( shp, targetDir );

    final File dbf = new File( landuseSourceBase + ".dbf" ); //$NON-NLS-1$
    FileUtils.copyFileToDirectory( dbf, targetDir );

    final File shx = new File( landuseSourceBase + ".shx" ); //$NON-NLS-1$
    FileUtils.copyFileToDirectory( shx, targetDir );

  }

  /**
   * creates a DummyTheme for landuse to calculate the extent of the created map (Waterlevel.gmt)
   * 
   * @return KalypsoFeatureTheme landuse
   * @throws GmlSerializeException
   */
  private IKalypsoTheme createDummyLanduseTheme( ) throws GmlSerializeException, IOException
  {
    final String shapeBase = (m_workspacePath.append( m_projectHandle.getFullPath().append( "/Landuse/" + FileUtilities.nameWithoutExtension( m_landuseDataFile.getName().toString() ) ) )).toString(); //$NON-NLS-1$
    m_landuseShapeWS = ShapeSerializer.deserialize( shapeBase, m_landuseCooSystem );
    final KalypsoFeatureTheme kft = new KalypsoFeatureTheme( new CommandableWorkspace( m_landuseShapeWS ), "featureMember", WizardMessages.getString( "CreateFloodRiskProjectJob.Landuse" ), null, null ); //$NON-NLS-1$ //$NON-NLS-2$
    final ShapeFile sf = new ShapeFile( shapeBase );
    m_boundingBox = sf.getFileMBR();
    sf.close();
    return kft;
  }

  /**
   * creates the gml version of the landuseData (LanduseVectorData.gml, Schema: VectorDataModel.xsd) and populates
   * m_landuseTypeSet
   * 
   * @return Set of landuse types (HashSet)
   * @throws IOException
   * @throws GmlSerializeException
   */
  private void createLanduseDataGML( ) throws IOException, GmlSerializeException, InvocationTargetException
  {
    final File landuseDataGML = m_workspacePath.append( m_projectHandle.getFullPath() + "/Landuse/LanduseVectorData.gml" ).toFile(); //$NON-NLS-1$
    final GMLSchema schema = schemaCatalog.getSchema( UrlCatalogFloodRisk.NS_VECTORDATAMODEL, (String) null );

    final QName rootFeatureTypeName = new QName( UrlCatalogFloodRisk.NS_VECTORDATAMODEL, "VectorDataCollection" ); //$NON-NLS-1$
    final QName featureTypePropertyName = new QName( UrlCatalogFloodRisk.NS_VECTORDATAMODEL, "FeatureMember" ); //$NON-NLS-1$
    final QName shapeFeatureTypePropertyName = new QName( "namespace", "featureMember" ); //$NON-NLS-1$ //$NON-NLS-2$
    final QName shapeGeomPropertyName = new QName( "namespace", "GEOM" ); //$NON-NLS-1$ //$NON-NLS-2$
    final QName propertyName = new QName( "namespace", m_landusePropertyName ); //$NON-NLS-1$

    final IFeatureType[] types = schema.getAllFeatureTypes();
    final IFeatureType rootFeatureType = schema.getFeatureType( rootFeatureTypeName );
    final Feature rootFeature = FeatureFactory.createFeature( null, null, rootFeatureTypeName + "0", rootFeatureType, true ); //$NON-NLS-1$
    final IRelationType ftp_feature = (IRelationType) rootFeatureType.getProperty( featureTypePropertyName );
    final Feature shapeRootFeature = m_landuseShapeWS.getRootFeature();
    final List featureList = (List) shapeRootFeature.getProperty( shapeFeatureTypePropertyName );
    for( int i = 0; i < featureList.size(); i++ )
    {
      final Feature feat = (Feature) featureList.get( i );
      final String propertyValue = (String) feat.getProperty( propertyName );
      if( !m_landuseTypeSet.contains( propertyValue ) )
      {
        m_landuseTypeSet.add( propertyValue );
      }
      final Object[] properties = new Object[] { null, null, null, null, null, (GM_Object) feat.getProperty( shapeGeomPropertyName ), propertyValue };
      final Feature feature = FeatureFactory.createFeature( rootFeature, ftp_feature, "Feature" + i, ftp_feature.getTargetFeatureType(), properties ); //$NON-NLS-1$
      FeatureHelper.addProperty( rootFeature, ftp_feature, feature );
    }
    final GMLWorkspace workspace = new GMLWorkspace_Impl( schema, types, rootFeature, landuseDataGML.toURL(), "", null ); //$NON-NLS-1$
    final FileWriter fw = new FileWriter( landuseDataGML );
    GmlSerializer.serializeWorkspace( fw, workspace );
    fw.close();
  }

  /**
   * Creates a LanduseCollection with the given set of landuse types in ContextModel and RiskContextModel
   * 
   * @param landuseTypeSet
   *            Set of existing landuse types
   * @throws Exception
   */
  private void autogenerateLanduseCollection( ) throws Exception
  {
    final URL contextModelURL = m_workspacePath.append( m_projectHandle.getFullPath() + "/Control/contextModell.gml" ).toFile().toURL(); //$NON-NLS-1$
    final URL riskContextModelURL = m_workspacePath.append( m_projectHandle.getFullPath() + "/Control/riskContextModell.gml" ).toFile().toURL(); //$NON-NLS-1$

    final QName landuseFeatureType = new QName( UrlCatalogFloodRisk.NS_CONTEXTMODEL, "Landuse" ); //$NON-NLS-1$
    final QName landuseCollectionType = new QName( UrlCatalogFloodRisk.NS_CONTEXTMODEL, "LanduseCollection" ); //$NON-NLS-1$
    final QName parentFeatureName = new QName( UrlCatalogFloodRisk.NS_CONTEXTMODEL, "LanduseCollectionMember" ); //$NON-NLS-1$
    final QName propertyName = new QName( UrlCatalogFloodRisk.NS_CONTEXTMODEL, "Name" ); //$NON-NLS-1$
    final QName propertyLanduseMember = new QName( UrlCatalogFloodRisk.NS_CONTEXTMODEL, "LanduseMember" ); //$NON-NLS-1$
    final QName landuseFeatureTypeRisk = new QName( UrlCatalogFloodRisk.NS_RISKCONTEXTMODEL, "Landuse" ); //$NON-NLS-1$
    final QName landuseCollectionTypeRisk = new QName( UrlCatalogFloodRisk.NS_RISKCONTEXTMODEL, "LanduseCollection" ); //$NON-NLS-1$
    final QName parentFeatureNameRisk = new QName( UrlCatalogFloodRisk.NS_RISKCONTEXTMODEL, "LanduseCollectionMember" ); //$NON-NLS-1$
    final QName propertyNameRisk = new QName( UrlCatalogFloodRisk.NS_RISKCONTEXTMODEL, "Name" ); //$NON-NLS-1$
    final QName propertyLanduseMemberRisk = new QName( UrlCatalogFloodRisk.NS_RISKCONTEXTMODEL, "LanduseMember" ); //$NON-NLS-1$

    // contextModel
    final GMLWorkspace contextModel = GmlSerializer.createGMLWorkspace( contextModelURL, null );

    final GMLSchema schema = schemaCatalog.getSchema( UrlCatalogFloodRisk.NS_CONTEXTMODEL, (String) null );
    final IFeatureType ftLanduse = schema.getFeatureType( landuseFeatureType );
    final IFeatureType ftLanduseCollection = schema.getFeatureType( landuseCollectionType );
    final IRelationType featureProperty = (IRelationType) ftLanduse.getProperty( propertyName );
    final Feature rootFeature = contextModel.getRootFeature();
    final Feature parentFeature = (Feature) rootFeature.getProperty( parentFeatureName );
    final IRelationType featurePropertyName = (IRelationType) ftLanduseCollection.getProperty( propertyLanduseMember );

    // riskContextModel
    final GMLSchema schemaRisk = schemaCatalog.getSchema( UrlCatalogFloodRisk.NS_RISKCONTEXTMODEL, (String) null );
    final IFeatureType ftLanduseRisk = schemaRisk.getFeatureType( landuseFeatureTypeRisk );
    final IFeatureType ftLanduseCollectionRisk = schemaRisk.getFeatureType( landuseCollectionTypeRisk );
    final IPropertyType featurePropertyRisk = ftLanduseRisk.getProperty( propertyNameRisk );
    final GMLWorkspace riskContextModel = GmlSerializer.createGMLWorkspace( riskContextModelURL, null );
    final Feature rootFeature_risk = riskContextModel.getRootFeature();
    final Feature parentFeature_risk = (Feature) rootFeature_risk.getProperty( parentFeatureNameRisk );
    final IRelationType featurePropertyNameRisk = (IRelationType) ftLanduseCollectionRisk.getProperty( propertyLanduseMemberRisk );

    final Iterator it = m_landuseTypeSet.iterator();
    String landusePropertyName = null;
    Feature landuseFeature = null;
    while( it.hasNext() )
    {
      landusePropertyName = (String) it.next();
      // contextModel
      landuseFeature = contextModel.createFeature( parentFeature, featureProperty, ftLanduse );
      landuseFeature.setProperty( featureProperty, landusePropertyName );
      contextModel.addFeatureAsComposition( parentFeature, featurePropertyName, 0, landuseFeature );
      // riskContextModel
      final Feature landuseFeature_risk = riskContextModel.createFeature( parentFeature_risk, featurePropertyNameRisk, ftLanduseRisk );
      landuseFeature_risk.setProperty( featurePropertyRisk, landusePropertyName );
      riskContextModel.addFeatureAsComposition( parentFeature_risk, featurePropertyNameRisk, 0, landuseFeature_risk );
    }
    // contextModel
    final FileWriter fw = new FileWriter( contextModelURL.getFile() );
    GmlSerializer.serializeWorkspace( fw, contextModel );
    fw.close();
    // riskContextModel
    final FileWriter fw_risk = new FileWriter( riskContextModelURL.getFile() );
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
  private Vector createWaterlevelGrids( final IProgressMonitor monitor ) throws Exception
  {
//    final Vector<File> targetFiles = new Vector<File>();
//    final int workedPart = 50 / m_waterlevelGrids.size();
//    for( int i = 0; i < m_waterlevelGrids.size(); i++ )
//    {
//      final File sourceFile = (File) m_waterlevelGrids.get( i );
//      RectifiedGridCoverage grid = GridUtils.importGridArc( sourceFile, m_waterlevelCooSystem );
//      final String sourceFileNameWithoutExtension = FileUtilities.nameWithoutExtension( sourceFile.getName() );
//      final File waterlevelDir = (m_workspacePath.append( m_projectHandle.getFullPath().append( "/Waterlevel" ) )).toFile(); //$NON-NLS-1$
//      waterlevelDir.mkdir();
//      final File targetFile = (m_workspacePath.append( m_projectHandle.getFullPath().append( "/Waterlevel/" + sourceFileNameWithoutExtension + ".gml" ) )).toFile(); //$NON-NLS-1$ //$NON-NLS-2$
//
//      GridUtils.writeRasterData( targetFile, grid );
//      targetFiles.add( targetFile );
//      final String sldFileName = m_workspacePath.toOSString() + m_projectHandle.getFullPath() + "/.styles/" + sourceFileNameWithoutExtension + ".sld"; //$NON-NLS-1$ //$NON-NLS-2$
//      final Color lightBlue = new Color( 150, 150, 255 );
//      final int numOfCategories = 5;
//
//      createRasterStyle( sldFileName, sourceFileNameWithoutExtension, grid, lightBlue, numOfCategories );
//
//      StyledLayerType styledLayerType = createWaterlevelLayer( targetFile, sourceFileNameWithoutExtension );
//      JAXBElement<StyledLayerType> layerElement = TemplateUtilitites.OF_GISMAPVIEW.createLayer( styledLayerType );
//      m_layerList.add( layerElement );
//
//      grid = null;
//      monitor.worked( workedPart );
//      if( monitor.isCanceled() )
//        return null;
//    }
//    return targetFiles;
    return null;
  }

  /**
   * Creates a rasterStyle (interval-value) with the given number of intervals and a given color as lightest color
   * 
   * @param resultFile
   *            style
   * @param styleName
   *            name of style
   * @param grid
   * @param color
   *            lightest color
   * @param numOfCategories
   *            number of intervals
   * @throws Exception
   */
  private void createRasterStyle( final String resultFileName, final String styleName, final RectifiedGridCoverage grid, Color color, final int numOfCategories ) throws Exception
  {
    final TreeMap<Double, ColorMapEntry> colorMap = new TreeMap<Double, ColorMapEntry>();
    final ColorMapEntry colorMapEntry_noData = new ColorMapEntry_Impl( Color.WHITE, 0, -9999, WizardMessages.getString( "CreateFloodRiskProjectJob.NoData" ) ); //$NON-NLS-1$
    colorMap.put( new Double( -9999 ), colorMapEntry_noData );

//    final DoubleRaster dr = new RectifiedGridCoverageDoubleRaster( grid.getFeature() );
    final MinMaxRasterWalker walker = new MinMaxRasterWalker();
//    dr.walk( walker, null );
    final double min = 0.0; // We don't use walker.getMin() because min is -9999.0, and we need 0.0 water level as min
    final double max = walker.getMax();

    final double intervalStep = (max - min) / numOfCategories;
    for( int i = 0; i < numOfCategories; i++ )
    {
      final double quantity = Number.round( (min + (i * intervalStep)), 4, BigDecimal.ROUND_HALF_EVEN );
      final ColorMapEntry colorMapEntry = new ColorMapEntry_Impl( color, 0.5, quantity, "" ); //$NON-NLS-1$
      color = color.darker();
      colorMap.put( new Double( quantity ), colorMapEntry );
    }
    color = color.darker();
    final ColorMapEntry colorMapEntry_max = new ColorMapEntry_Impl( color, 0.5, max, "" ); //$NON-NLS-1$
    colorMap.put( new Double( max ), colorMapEntry_max );
    final RasterSymbolizer rasterSymbolizer = new RasterSymbolizer_Impl( colorMap );
    final Symbolizer[] symbolizers = new Symbolizer[] { rasterSymbolizer };
    final FeatureTypeStyle featureTypeStyle = new FeatureTypeStyle_Impl();
    final double minScaleDenominator = 0;
    final double maxScaleDenominator = 1.8;
    final Rule rule = StyleFactory.createRule( symbolizers, "default", "default", "default", minScaleDenominator, maxScaleDenominator ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    featureTypeStyle.addRule( rule );
    final FeatureTypeStyle[] featureTypeStyles = new FeatureTypeStyle[] { featureTypeStyle };
    final org.kalypsodeegree.graphics.sld.Style[] styles = new org.kalypsodeegree.graphics.sld.Style[] { new UserStyle_Impl( styleName, styleName, null, false, featureTypeStyles ) };
    final org.kalypsodeegree.graphics.sld.Layer[] layers = new org.kalypsodeegree.graphics.sld.Layer[] { SLDFactory.createNamedLayer( "deegree style definition", null, styles ) }; //$NON-NLS-1$
    final StyledLayerDescriptor sld = SLDFactory.createStyledLayerDescriptor( layers, "1.0" ); //$NON-NLS-1$

    // FIXME exportAsXML for RasterSymbolizer doesn't work
    // Dejan: I've changed it, it supports ColorMap for now (as we are using only ColorMap)

    // System.out.println(((StyledLayerDescriptor_Impl) sld).exportAsXML());
    final Document doc = XMLTools.parse( new StringReader( ((StyledLayerDescriptor_Impl) sld).exportAsXML() ) );
    final Source source = new DOMSource( doc );
    OutputStream os = null;
    try
    {
      os = new FileOutputStream( resultFileName );
      final StreamResult result = new StreamResult( os );
      final Transformer t = TransformerFactory.newInstance().newTransformer();
      t.setOutputProperty( "{http://xml.apache.org/xslt}indent-amount", "2" ); //$NON-NLS-1$ //$NON-NLS-2$
      t.setOutputProperty( OutputKeys.INDENT, "yes" ); //$NON-NLS-1$
      t.transform( source, result );
    }
    finally
    {
      IOUtils.closeQuietly( os );
    }
  }

  private void createLanduseStyle( final String resultFileName ) throws Exception
  {
    final HashMap<String, Color> predefinedColors = new HashMap<String, Color>();
    predefinedColors.put( "Wasser", Color.blue.brighter() );
    predefinedColors.put( "Water", Color.blue.brighter() );
    predefinedColors.put( "Wohnen", Color.orange );
    predefinedColors.put( "Gruenland", Color.green.darker() );
    predefinedColors.put( "Naturraum", Color.green.brighter() );
    predefinedColors.put( "Industrie", Color.magenta );
    predefinedColors.put( "Verkehr", Color.gray );
    predefinedColors.put( "Ackerland", Color.yellow );
    predefinedColors.put( "Wald", Color.pink );
    final Iterator iterator = m_landuseTypeSet.iterator();
    String landusePropertyName = null;
    final double minScaleDenominator = 0;
    final double maxScaleDenominator = 1000000000000000.0;
    final Stroke stroke = StyleFactory.createStroke( new Color( 0, 0, 0 ), 1.0, 0.5, null, "mitre", "butt" );
    Color color = Color.red;
    final FeatureTypeStyle featureTypeStyle = new FeatureTypeStyle_Impl();
    Fill fill = null;
    while( iterator.hasNext() )
    {
      color = color.brighter();
      landusePropertyName = (String) iterator.next();
      final Geometry geometry = new Geometry_Impl( new PropertyName( "GEOM", null ) );
      if( predefinedColors.containsKey( landusePropertyName ) )
        fill = StyleFactory.createFill( predefinedColors.get( landusePropertyName ), 0.3 );
      else
        fill = StyleFactory.createFill( color, 0.8 );
      final PolygonSymbolizer polygonSymbolizer = new PolygonSymbolizer_Impl( fill, stroke, geometry, minScaleDenominator, maxScaleDenominator, UOM.pixel );
      final Symbolizer[] symbolizers = new Symbolizer[] { polygonSymbolizer };
      final Operation operation = new PropertyIsLikeOperation( new PropertyName( m_landusePropertyName ), new Literal( landusePropertyName ), '*', '$', '/' );
      final Filter filter = new ComplexFilter( operation );
      final Rule rule = StyleFactory.createRule( symbolizers, "default", landusePropertyName, "default", minScaleDenominator, maxScaleDenominator ); //$NON-NLS-1$ //$NON-NLS-2$
      rule.setFilter( filter );
      featureTypeStyle.addRule( rule );
    }
    final FeatureTypeStyle[] featureTypeStyles = new FeatureTypeStyle[] { featureTypeStyle };
    final org.kalypsodeegree.graphics.sld.Style[] styles = new org.kalypsodeegree.graphics.sld.Style[] { new UserStyle_Impl( "Landnutzung", "Landnutzung", null, false, featureTypeStyles ) };
    final org.kalypsodeegree.graphics.sld.Layer[] layers = new org.kalypsodeegree.graphics.sld.Layer[] { SLDFactory.createNamedLayer( "deegree style definition", null, styles ) }; //$NON-NLS-1$
    final StyledLayerDescriptor sld = SLDFactory.createStyledLayerDescriptor( layers, "1.0" ); //$NON-NLS-1$
    final Document doc = XMLTools.parse( new StringReader( ((StyledLayerDescriptor_Impl) sld).exportAsXML() ) );
    final Source source = new DOMSource( doc );
    OutputStream os = null;
    try
    {
      os = new FileOutputStream( resultFileName );
      final StreamResult result = new StreamResult( os );
      final Transformer t = TransformerFactory.newInstance().newTransformer();
      t.setOutputProperty( "{http://xml.apache.org/xslt}indent-amount", "2" ); //$NON-NLS-1$ //$NON-NLS-2$
      t.setOutputProperty( OutputKeys.INDENT, "yes" ); //$NON-NLS-1$
      t.transform( source, result );
    }
    finally
    {
      IOUtils.closeQuietly( os );
    }
  }

  /**
   * creates the control file waterlevelData.gml with all given waterlevelGrids as parameters
   * 
   * @param targetFiles
   * @throws IOException
   * @throws GmlSerializeException
   */
  private void createWaterlevelData( final Vector targetFiles ) throws IOException, GmlSerializeException, InvocationTargetException
  {
    final File waterlevelDataFile = m_workspacePath.append( m_projectHandle.getFullPath() + "/Control/waterlevelData.gml" ).toFile(); //$NON-NLS-1$
    /*
     * // register typeHandler final ITypeRegistry<IMarshallingTypeHandler> registry =
     * MarshallingTypeRegistrySingleton.getTypeRegistry(); try { registry.registerTypeHandler( new RangeSetTypeHandler() );
     * registry.registerTypeHandler( new RectifiedGridDomainTypeHandler() ); } catch( TypeRegistryException e ) {
     * e.printStackTrace(); }
     */

    // load schema
    final GMLSchemaCatalog schemaCatalog2 = KalypsoGMLSchemaPlugin.getDefault().getSchemaCatalog();
    final GMLSchema schema = schemaCatalog2.getSchema( UrlCatalogFloodRisk.NS_WATERLEVELDATA, (String) null );
    final QName rootFeatureName = new QName( UrlCatalogFloodRisk.NS_WATERLEVELDATA, "WaterlevelData" ); //$NON-NLS-1$
    final QName rootFeatureProp = new QName( UrlCatalogFloodRisk.NS_WATERLEVELDATA, "WaterlevelMember" ); //$NON-NLS-1$
    final IFeatureType rootFeatureType = schema.getFeatureType( rootFeatureName );
    final Feature rootFeature = FeatureFactory.createFeature( null, null, "WaterlevelData0", rootFeatureType, true ); //$NON-NLS-1$
    final IRelationType waterlevelMember = (IRelationType) rootFeatureType.getProperty( rootFeatureProp );
    // create waterlevelFeature(s)
    final QName waterlevelFeatureName = new QName( UrlCatalogFloodRisk.NS_WATERLEVELDATA, "Waterlevel" ); //$NON-NLS-1$
    final QName waterlevelFeatureProp = new QName( UrlCatalogFloodRisk.NS_WATERLEVELDATA, "WaterlevelRasterData" ); //$NON-NLS-1$
    final IFeatureType waterlevelFeatureType = schema.getFeatureType( waterlevelFeatureName );
    final IPropertyType featureProperty = waterlevelFeatureType.getProperty( waterlevelFeatureProp );
    int identifier = 0;
    for( int i = 0; i < targetFiles.size(); i++ )
    {
      final Feature waterlevelFeature = FeatureFactory.createFeature( rootFeature, waterlevelMember, waterlevelFeatureName.getLocalPart() + identifier, waterlevelFeatureType, true );
      final IFile waterlevelFile = ResourceUtilities.findFileFromURL( ((File) targetFiles.get( i )).toURL() );
      waterlevelFeature.setProperty( featureProperty, waterlevelFile.getLocationURI().toASCIIString() );
      FeatureHelper.addProperty( rootFeature, waterlevelMember, waterlevelFeature );
      identifier = identifier + 1;
    }
    // create workspace
    final IFeatureType[] types = schema.getAllFeatureTypes();
    final GMLWorkspace workspace = new GMLWorkspace_Impl( schema, types, rootFeature, waterlevelDataFile.toURL(), "", null ); //$NON-NLS-1$
    final FileWriter fw = new FileWriter( waterlevelDataFile );
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
    newLayer.setName( WizardMessages.getString( "CreateFloodRiskProjectJob.Landuse" ) ); //$NON-NLS-1$
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
    style.setStyle( WizardMessages.getString( "CreateFloodRiskProjectJob.Landuse" ) ); //$NON-NLS-1$
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
  private StyledLayerType createWaterlevelLayer( final File sourceFile, final String styleName ) throws Exception
  {
    final StyledLayerType newLayer = typeOF.createStyledLayerType();

    // set attributes for the layer
    newLayer.setName( styleName );
    newLayer.setVisible( false );

    // no named layer is treated like root feature (RectifiedGridCoverage here)
    newLayer.setFeaturePath( "" ); //$NON-NLS-1$
    // newLayer.setFeaturePath( "RectifiedGridCoverage" ); //$NON-NLS-1$
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
  private boolean performCancel( )
  {
    try
    {
      m_projectHandle.delete( true, false, null );
      // m_projectHandel.refreshLocal( IResource.DEPTH_INFINITE, null );
    }
    catch( final CoreException e )
    {
      e.printStackTrace();
      return false;
    }
    return true;
  }
}