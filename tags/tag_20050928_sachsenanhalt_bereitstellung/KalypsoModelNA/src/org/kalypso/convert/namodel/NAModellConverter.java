/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
 
 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.convert.namodel;

import java.io.File;
import java.io.FileWriter;
import java.io.Writer;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;

import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.contribs.java.net.IUrlCatalog;
import org.kalypso.contribs.java.net.MultiUrlCatalog;
import org.kalypso.convert.namodel.manager.AsciiBuffer;
import org.kalypso.convert.namodel.manager.BodenartManager;
import org.kalypso.convert.namodel.manager.BodentypManager;
import org.kalypso.convert.namodel.manager.CatchmentManager;
import org.kalypso.convert.namodel.manager.ChannelManager;
import org.kalypso.convert.namodel.manager.HydrotopManager;
import org.kalypso.convert.namodel.manager.NetFileManager;
import org.kalypso.convert.namodel.manager.NutzungManager;
import org.kalypso.convert.namodel.manager.ParseManager;
import org.kalypso.convert.namodel.manager.RHBManager;
import org.kalypso.convert.namodel.manager.SchneeManager;
import org.kalypso.convert.namodel.schema.UrlCatalogNA;
import org.kalypso.ogc.gml.serialize.GmlSerializeException;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ogc.gml.serialize.ShapeSerializer;
import org.kalypso.ogc.gml.typehandler.DiagramTypeHandler;
import org.kalypso.ogc.sensor.deegree.ObservationLinkHandler;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureProperty;
import org.kalypsodeegree.model.feature.FeatureType;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.extension.ITypeRegistry;
import org.kalypsodeegree_impl.extension.MarshallingTypeRegistrySingleton;
import org.kalypsodeegree_impl.gml.schema.GMLSchema;
import org.kalypsodeegree_impl.gml.schema.GMLSchemaCatalog;
import org.kalypsodeegree_impl.gml.schema.GMLSchemaUtils;
import org.kalypsodeegree_impl.gml.schema.schemata.DeegreeUrlCatalog;
import org.kalypsodeegree_impl.model.cs.ConvenienceCSFactoryFull;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;
import org.kalypsodeegree_impl.model.feature.GMLWorkspace_Impl;
import org.opengis.cs.CS_CoordinateSystem;
import org.w3c.dom.Document;

/**
 * import and export of kalypso rainfall runoff models converts between custom ascii format and gml format. importing
 * ascii is always processed into a gml file-structure (includes generating of zml files).
 * 
 * export to ascii can be generated from a gml file or from a gml workspace
 * 
 * @author doemming
 */
public class NAModellConverter
{
  private GMLSchema m_modelSchema;

  private final CatchmentManager m_catchmentManager;

  private final ChannelManager m_gerinneManager;

  private final ParseManager m_parseManager;

  private final NAConfiguration m_conf;

  private final NetFileManager m_nodeManager;

  private final RHBManager m_rhbManager;

  private final HydrotopManager m_hydrotopManager;

  private final BodenartManager m_bodartManager;

  private final BodentypManager m_bodtypManager;

  private final NutzungManager m_nutzManager;

  private final SchneeManager m_schneeManager;

  public static void main( String[] args )
  {
    final IUrlCatalog catalog = new MultiUrlCatalog( new IUrlCatalog[]
    {
        new DeegreeUrlCatalog(),
        new UrlCatalogNA() } );
    GMLSchemaCatalog.init( catalog, FileUtilities.createNewTempDir( "schemaCache" ) );
    try
    {
      // general
      final ITypeRegistry registry = MarshallingTypeRegistrySingleton.getTypeRegistry();
      registry.registerTypeHandler( new ObservationLinkHandler() );
      registry.registerTypeHandler( new DiagramTypeHandler() );
      completeascii2gml();
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }
  }

  public static void completeascii2gml() throws Exception
  {
    final File gmlBaseDir = FileUtilities.createNewTempDir( "NA_gmlBaseDir" );
    File asciiBaseDir = new File(
        "D:\\FE-Projekte\\2004_SchulungIngBueros\\KalypsoSchulung\\tmp\\ex6-longterm\\solution" );

    NAConfiguration conf = NAConfiguration.getAscii2GmlConfiguration( asciiBaseDir, gmlBaseDir );
    Feature modelRootFeature = modelAsciiToFeature( conf );

    String shapeDir = "D:\\Kalypso_NA\\9-Programmtest\\Uebungsmodell\\Shapes";
    insertSHPGeometries( modelRootFeature, shapeDir );

    File modelGmlFile = new File( gmlBaseDir, "modell.gml" );
    final GMLSchema modelGmlSchema = GMLSchemaCatalog.getSchema( NaModelConstants.NS_NAMODELL );
    final Document modelSchema = modelGmlSchema.getSchema();

    FeatureType[] featureTypes = GMLSchemaUtils.getAllFeatureTypesFromSchema( modelGmlSchema );

    final GMLWorkspace modelWorkspace = new GMLWorkspace_Impl( featureTypes, modelRootFeature, null,
        " project:/.model/schema/namodell.xsd", modelSchema.getNamespaceURI(), modelGmlSchema.getNamespaceMap() );
    GmlSerializer.serializeWorkspace( new FileWriter( modelGmlFile ), modelWorkspace );

    System.out.println( "Conversion from ASCII to GML was successfull!!!" );
    System.out.println( "Outputfolder: " + gmlBaseDir );
  }

  private static void insertSHPGeometries( Feature modelFeature, String shapeDir ) throws GmlSerializeException
  {
    // load ShapeFile
    ConvenienceCSFactoryFull csFac = new ConvenienceCSFactoryFull();
    CS_CoordinateSystem cSystem = org.kalypsodeegree_impl.model.cs.Adapters.getDefault().export(
        csFac.getCSByName( "EPSG:31467" ) );

    final GMLWorkspace catchmentWorkspace = ShapeSerializer.deserialize( shapeDir + "\\Teilgebiete", cSystem );
    final List catchmentFeatures = (List)catchmentWorkspace.getRootFeature().getProperty(
        ShapeSerializer.PROPERTY_FEATURE_MEMBER );

    final GMLWorkspace channelWorkspace = ShapeSerializer.deserialize( shapeDir + "\\gewaesser", cSystem );
    final List channelFeatures = (List)channelWorkspace.getRootFeature().getProperty(
        ShapeSerializer.PROPERTY_FEATURE_MEMBER );

    final GMLWorkspace nodeWorkspace = ShapeSerializer.deserialize( shapeDir + "\\knoten", cSystem );
    final List nodeFeatures = (List)nodeWorkspace.getRootFeature()
        .getProperty( ShapeSerializer.PROPERTY_FEATURE_MEMBER );

    // insertGeometries

    System.out.println( "inserting geometries: catchments" );
    Feature catchmentCollection = (Feature)modelFeature.getProperty( "CatchmentCollectionMember" );
    List catchmentList = (List)catchmentCollection.getProperty( "catchmentMember" );
    copyProperties( catchmentFeatures, "GEOM", "TEILGEBNR", (Feature[])catchmentList.toArray( new Feature[catchmentList
        .size()] ), "Ort", "inum" );

    System.out.println( "inserting geometries: channels" );
    Feature channelCollection = (Feature)modelFeature.getProperty( "ChannelCollectionMember" );
    List channelList = (List)channelCollection.getProperty( "channelMember" );
    copyProperties( channelFeatures, "GEOM", "STRANGNR", (Feature[])channelList
        .toArray( new Feature[channelList.size()] ), "Ort", "inum" );

    System.out.println( "inserting geometries: nodes" );
    Feature nodeCollection = (Feature)modelFeature.getProperty( "NodeCollectionMember" );
    List nodeList = (List)nodeCollection.getProperty( "nodeMember" );
    copyProperties( nodeFeatures, "GEOM", "KNOTENNR", (Feature[])nodeList.toArray( new Feature[nodeList.size()] ),
        "Ort", "num" );
  }

  private static void copyProperties( final List catchmentFeatures, String orgGeomPropName, String orgIdPropName,
      Feature[] destFE, String destGeomPropName, String destIdPropName )
  {
    HashMap orgHash = new HashMap();
    for( Iterator iter = catchmentFeatures.iterator(); iter.hasNext(); )
    {
      final Feature f = (Feature)iter.next();
      String id = f.getProperty( orgIdPropName ).toString();
      orgHash.put( id, f );
    }
    for( int i = 0; i < destFE.length; i++ )
    {
      Feature destFeature = destFE[i];
      String id = destFeature.getProperty( destIdPropName ).toString();
      //            System.out.println("processing id=" + id);
      Feature orgFeaure = (Feature)orgHash.get( id );
      if( orgFeaure != null )
      {
        Object value = orgFeaure.getProperty( orgGeomPropName );
        if( value == null )
          System.out.println( "copyvalue is null: id=" + id );
        FeatureProperty fProp = FeatureFactory.createFeatureProperty( destGeomPropName, value );
        destFeature.setProperty( fProp );
      }
      else
        System.out.println( "not found in shapeFile: id=" + id );
    }
  }

  public NAModellConverter( NAConfiguration conf ) throws Exception
  {
    m_conf = conf;
    m_modelSchema = GMLSchemaCatalog.getSchema( NaModelConstants.NS_NAMODELL );
    GMLSchema m_parameterSchema = GMLSchemaCatalog.getSchema( NaModelConstants.NS_NAPARAMETER );

    m_catchmentManager = new CatchmentManager( m_modelSchema, m_conf );
    m_gerinneManager = new ChannelManager( m_modelSchema, m_conf );
    m_nodeManager = new NetFileManager( m_conf );
    m_rhbManager = new RHBManager( m_modelSchema, m_conf );
    m_hydrotopManager = new HydrotopManager( m_conf );
    m_bodartManager = new BodenartManager( m_parameterSchema, m_conf );
    m_bodtypManager = new BodentypManager( m_parameterSchema, m_conf );
    m_nutzManager = new NutzungManager( m_parameterSchema, m_conf );
    m_schneeManager = new SchneeManager( m_parameterSchema, m_conf );

    m_parseManager = new ParseManager( m_modelSchema, m_parameterSchema, conf, m_catchmentManager, m_gerinneManager,
        m_nodeManager, m_rhbManager, m_bodartManager, m_bodtypManager, m_nutzManager, m_schneeManager );
  }

  public ParseManager getParseManager()
  {
    return m_parseManager;
  }

  public void write( GMLWorkspace modelWorkspace, GMLWorkspace parameterWorkspace, GMLWorkspace hydrotopeWorkspace ,final NaNodeResultProvider nodeResultProvider)
      throws Exception
  {

    AsciiBuffer asciiBuffer = new AsciiBuffer();

    m_nodeManager.writeFile( asciiBuffer, modelWorkspace,nodeResultProvider );
    m_catchmentManager.writeFile( asciiBuffer, modelWorkspace );
    m_gerinneManager.writeFile( asciiBuffer, modelWorkspace );

    Writer writer3 = new FileWriter( m_conf.getNetFile() );
    writer3.write( asciiBuffer.getNetBuffer().toString() );
    writer3.close();

    Writer writer = new FileWriter( m_conf.getCatchmentFile() );
    writer.write( asciiBuffer.getCatchmentBuffer().toString() );
    writer.close();

    Writer writer2 = new FileWriter( m_conf.getChannelFile() );
    writer2.write( asciiBuffer.getChannelBuffer().toString() );
    writer2.close();

    Writer writer4 = new FileWriter( m_conf.getRHBFile() );
    writer4.write( asciiBuffer.getRhbBuffer().toString() );
    writer4.close();

    Writer writer9 = new FileWriter( m_conf.getZFTFile() );
    writer9.write( asciiBuffer.getZFTBuffer().toString() );
    writer9.close();
    
    

    if( hydrotopeWorkspace != null )
    {
      m_hydrotopManager.writeFile( asciiBuffer, hydrotopeWorkspace, modelWorkspace );
      Writer writer5 = new FileWriter( m_conf.getHydrotopFile() );
      writer5.write( asciiBuffer.getHydBuffer().toString() );
      writer5.close();
    }

    if( parameterWorkspace != null )
    {
      m_bodartManager.writeFile( asciiBuffer, parameterWorkspace );
      m_bodtypManager.writeFile( asciiBuffer, parameterWorkspace );
      m_nutzManager.writeFile( parameterWorkspace );
      m_schneeManager.writeFile( asciiBuffer, parameterWorkspace );
      Writer writer6 = new FileWriter( m_conf.getBodenartFile() );
      writer6.write( asciiBuffer.getBodartBuffer().toString() );
      writer6.close();

      Writer writer7 = new FileWriter( m_conf.getBodentypFile() );
      writer7.write( asciiBuffer.getBodtypBuffer().toString() );
      writer7.close();

      Writer writer8 = new FileWriter( m_conf.getSchneeFile() );
      writer8.write( asciiBuffer.getSnowBuffer().toString() );
      writer8.close();

    }

  }

  public static Feature modelAsciiToFeature( NAConfiguration conf ) throws Exception
  {
    NAModellConverter main = new NAModellConverter( conf );
    return main.getParseManager().modelAsciiToFeature();
  }

  public static Feature parameterAsciiToFeature( NAConfiguration conf ) throws Exception
  {
    NAModellConverter main = new NAModellConverter( conf );
    return main.getParseManager().parameterAsciiToFeature();
  }

  public static void featureToAscii( NAConfiguration conf, GMLWorkspace modelWorkspace,
      GMLWorkspace parameterWorkspace, GMLWorkspace hydrotopWorkspace,final NaNodeResultProvider nodeResultProvider ) throws Exception
  {
    NAModellConverter main = new NAModellConverter( conf );
    main.write( modelWorkspace, parameterWorkspace, hydrotopWorkspace,nodeResultProvider );
  }
}