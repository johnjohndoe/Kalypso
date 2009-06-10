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

import java.io.DataOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import org.kalypso.convert.namodel.i18n.Messages;
import org.kalypso.convert.namodel.manager.AsciiBuffer;
import org.kalypso.convert.namodel.manager.BodenartManager;
import org.kalypso.convert.namodel.manager.BodentypManager;
import org.kalypso.convert.namodel.manager.CatchmentManager;
import org.kalypso.convert.namodel.manager.ChannelManager;
import org.kalypso.convert.namodel.manager.HydrotopManager;
import org.kalypso.convert.namodel.manager.IdleLanduseManager;
import org.kalypso.convert.namodel.manager.NetFileManager;
import org.kalypso.convert.namodel.manager.NutzungManager;
import org.kalypso.convert.namodel.manager.ParseManager;
import org.kalypso.convert.namodel.manager.RHBManager;
import org.kalypso.convert.namodel.manager.SchneeManager;
import org.kalypso.convert.namodel.manager.SwaleAndTrenchManager;
import org.kalypso.convert.namodel.schema.binding.Landuse;
import org.kalypso.convert.namodel.schema.binding.LanduseCollection;
import org.kalypso.convert.namodel.schema.binding.suds.Greenroof;
import org.kalypso.convert.namodel.schema.binding.suds.Swale;
import org.kalypso.convert.namodel.schema.binding.suds.SwaleInfiltrationDitch;
import org.kalypso.gmlschema.GMLSchema;
import org.kalypso.gmlschema.GMLSchemaCatalog;
import org.kalypso.gmlschema.KalypsoGMLSchemaPlugin;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.ogc.gml.serialize.GmlSerializeException;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ogc.gml.serialize.ShapeSerializer;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree_impl.model.feature.GMLWorkspace_Impl;
import org.kalypsodeegree_impl.model.feature.XLinkedFeature_Impl;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.Geometry;

/**
 * import and export of kalypso rainfall runoff models converts between custom ascii format and gml format. importing
 * ascii is always processed into a gml file-structure (includes generating of zml files). export to ascii can be
 * generated from a gml file or from a gml workspace
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

  private final SwaleAndTrenchManager m_swaleAndTrenchManager;

  private final IdleLanduseManager m_idleLanduseManager;

  // public static void main( String[] args )
  // {
  // final IUrlCatalog catalog = new MultiUrlCatalog( new IUrlCatalog[] { new DeegreeUrlCatalog(), new UrlCatalogNA() }
  // );
  // GMLSchemaCatalog.init( catalog, FileUtilities.createNewTempDir( "schemaCache" ) );
  // try
  // {
  // // general
  // final ITypeRegistry registry = MarshallingTypeRegistrySingleton.getTypeRegistry();
  // registry.registerTypeHandler( new ObservationLinkHandler() );
  // registry.registerTypeHandler( new DiagramTypeHandler() );
  // completeascii2gml();
  // }
  // catch( Exception e )
  // {
  // e.printStackTrace();
  // }
  // }

  public static void completeascii2gml( final File gmlBaseDir, final File asciiBaseDir ) throws Exception
  {
    // final File gmlBaseDir = FileUtilities.createNewTempDir( "NA_gmlBaseDir" );
    // File asciiBaseDir = new File(
    // "D:\\FE-Projekte\\2004_SchulungIngBueros\\KalypsoSchulung\\tmp\\ex6-longterm\\solution" );

    NAConfiguration conf = NAConfiguration.getAscii2GmlConfiguration( asciiBaseDir, gmlBaseDir );
    Feature modelRootFeature = modelAsciiToFeature( conf );

    String shapeDir = "D:\\Kalypso_NA\\9-Modelle\\7-Rantzau\\05_GIS\\NA-Modell";
    insertSHPGeometries( modelRootFeature, shapeDir );

    File modelGmlFile = new File( gmlBaseDir, "modell.gml" ); //$NON-NLS-1$
    final GMLSchemaCatalog schemaCatalog = KalypsoGMLSchemaPlugin.getDefault().getSchemaCatalog();
    final GMLSchema modelGmlSchema = schemaCatalog.getSchema( NaModelConstants.NS_NAMODELL, (String) null );
    // final Document modelSchema = modelGmlSchema.getSchema();

    // IFeatureType[] featureTypes = GMLSchemaUtil.getAllFeatureTypesFromSchema( modelGmlSchema );
    IFeatureType[] featureTypes = modelGmlSchema.getAllFeatureTypes();

    final GMLWorkspace modelWorkspace = new GMLWorkspace_Impl( modelGmlSchema, featureTypes, modelRootFeature, modelGmlFile.toURL(), null, " project:/.model/schema/namodell.xsd", null ); //$NON-NLS-1$
    GmlSerializer.serializeWorkspace( new FileWriter( modelGmlFile ), modelWorkspace );

    System.out.println( Messages.getString( "org.kalypso.convert.namodel.NAModellConverter.3" ) ); //$NON-NLS-1$
    System.out.println( Messages.getString( "org.kalypso.convert.namodel.NAModellConverter.4" ) + gmlBaseDir ); //$NON-NLS-1$
  }

  private static void insertSHPGeometries( Feature modelFeature, String shapeDir ) throws GmlSerializeException
  {
    // load ShapeFile
    String cSystem = "EPSG:31467"; //$NON-NLS-1$

    final GMLWorkspace catchmentWorkspace = ShapeSerializer.deserialize( shapeDir + "\\rantzau_gebiete", cSystem ); //$NON-NLS-1$
    final List catchmentFeatures = (List) catchmentWorkspace.getRootFeature().getProperty( ShapeSerializer.PROPERTY_FEATURE_MEMBER );

    final GMLWorkspace channelWorkspace = ShapeSerializer.deserialize( shapeDir + "\\straenge", cSystem ); //$NON-NLS-1$
    final List channelFeatures = (List) channelWorkspace.getRootFeature().getProperty( ShapeSerializer.PROPERTY_FEATURE_MEMBER );

    final GMLWorkspace nodeWorkspace = ShapeSerializer.deserialize( shapeDir + "\\knoten", cSystem ); //$NON-NLS-1$
    final List nodeFeatures = (List) nodeWorkspace.getRootFeature().getProperty( ShapeSerializer.PROPERTY_FEATURE_MEMBER );

    // insertGeometries

    System.out.println( Messages.getString( "org.kalypso.convert.namodel.NAModellConverter.9" ) ); //$NON-NLS-1$
    Feature catchmentCollection = (Feature) modelFeature.getProperty( NaModelConstants.CATCHMENT_COLLECTION_MEMBER_PROP );
    List catchmentList = (List) catchmentCollection.getProperty( NaModelConstants.CATCHMENT_MEMBER_PROP );
    copyProperties( catchmentFeatures, "GEOM", "TGNR", (Feature[]) catchmentList.toArray( new Feature[catchmentList.size()] ), "Ort", "name" );

    System.out.println( Messages.getString( "org.kalypso.convert.namodel.NAModellConverter.14" ) ); //$NON-NLS-1$
    Feature channelCollection = (Feature) modelFeature.getProperty( NaModelConstants.CHANNEL_COLLECTION_MEMBER_PROP );
    List channelList = (List) channelCollection.getProperty( NaModelConstants.CHANNEL_MEMBER_PROP );
    copyProperties( channelFeatures, "GEOM", "STRNR", (Feature[]) channelList.toArray( new Feature[channelList.size()] ), "Ort", "name" );

    System.out.println( Messages.getString( "org.kalypso.convert.namodel.NAModellConverter.19" ) ); //$NON-NLS-1$
    Feature nodeCollection = (Feature) modelFeature.getProperty( NaModelConstants.NODE_COLLECTION_MEMBER_PROP );
    List nodeList = (List) nodeCollection.getProperty( NaModelConstants.NODE_MEMBER_PROP );
    copyProperties( nodeFeatures, "GEOM", "KTNR", (Feature[]) nodeList.toArray( new Feature[nodeList.size()] ), "Ort", "name" );
  }

  private static void copyProperties( final List catchmentFeatures, String orgGeomPropName, String orgIdPropName, Feature[] destFE, String destGeomPropName, String destIdPropName )
  {
    HashMap<String, Feature> orgHash = new HashMap<String, Feature>();
    for( Iterator iter = catchmentFeatures.iterator(); iter.hasNext(); )
    {
      final Feature f = (Feature) iter.next();
      String id = f.getProperty( orgIdPropName ).toString();
      orgHash.put( id, f );
    }
    for( int i = 0; i < destFE.length; i++ )
    {
      Feature destFeature = destFE[i];
      String id = destFeature.getProperty( destIdPropName ).toString();
      // System.out.println("processing id=" + id);
      Feature orgFeaure = orgHash.get( id );
      if( orgFeaure != null )
      {
        Object value = orgFeaure.getProperty( orgGeomPropName );
        if( value == null )
          System.out.println( Messages.getString( "org.kalypso.convert.namodel.NAModellConverter.24" ) + id ); //$NON-NLS-1$
        // FeatureProperty fProp = FeatureFactory.createFeatureProperty( destGeomPropName, value );
        destFeature.setProperty( destGeomPropName, value );
      }
      else
        System.out.println( Messages.getString( "org.kalypso.convert.namodel.NAModellConverter.25" ) + id ); //$NON-NLS-1$
    }
  }

  public NAModellConverter( NAConfiguration conf ) throws Exception
  {
    m_conf = conf;

    final GMLSchemaCatalog schemaCatalog = KalypsoGMLSchemaPlugin.getDefault().getSchemaCatalog();
    m_modelSchema = schemaCatalog.getSchema( NaModelConstants.NS_NAMODELL, (String) null );
    GMLSchema m_parameterSchema = schemaCatalog.getSchema( NaModelConstants.NS_NAPARAMETER, (String) null );

    m_catchmentManager = new CatchmentManager( m_modelSchema, m_conf );
    m_gerinneManager = new ChannelManager( m_modelSchema, m_conf );
    m_nodeManager = new NetFileManager( m_conf );
    m_rhbManager = new RHBManager( m_modelSchema, m_conf );
    m_hydrotopManager = new HydrotopManager( m_conf );
    m_swaleAndTrenchManager = new SwaleAndTrenchManager( m_modelSchema, m_conf );
    m_bodartManager = new BodenartManager( m_parameterSchema, m_conf );
    m_bodtypManager = new BodentypManager( m_parameterSchema, m_conf );
    m_nutzManager = new NutzungManager( m_parameterSchema, m_conf );
    m_schneeManager = new SchneeManager( m_parameterSchema, m_conf );
    m_idleLanduseManager = new IdleLanduseManager( m_parameterSchema, m_conf );
    m_parseManager = new ParseManager( m_modelSchema, m_parameterSchema, conf, m_catchmentManager, m_gerinneManager, m_nodeManager, m_rhbManager, m_bodartManager, m_bodtypManager, m_nutzManager, m_schneeManager, m_idleLanduseManager );
  }

  public ParseManager getParseManager( )
  {
    return m_parseManager;
  }

  public void write( GMLWorkspace modelWorkspace, GMLWorkspace parameterWorkspace, GMLWorkspace hydrotopeWorkspace, GMLWorkspace synthNWorkspace, GMLWorkspace landuseWorkspace, GMLWorkspace sudsWorkspace, final NaNodeResultProvider nodeResultProvider ) throws Exception
  {
    // TODO replace this AsciiBuffer with some no-memory-consuming structure (regular StringBuffer)
    AsciiBuffer asciiBuffer = new AsciiBuffer();

    m_nodeManager.writeFile( asciiBuffer, modelWorkspace, synthNWorkspace );
    m_catchmentManager.writeFile( asciiBuffer, modelWorkspace );
    m_gerinneManager.writeFile( asciiBuffer, modelWorkspace );
//    m_swaleAndTrenchManager.writeFile( asciiBuffer, modelWorkspace );
    writeToFile( m_conf.getNetFile(), asciiBuffer.getNetBuffer() );
    writeToFile( m_conf.getCatchmentFile(), asciiBuffer.getCatchmentBuffer() );
    writeToFile( m_conf.getChannelFile(), asciiBuffer.getChannelBuffer() );
    // writeToFile( m_conf.getSwaleAndTrenchFile(), asciiBuffer.getSwaleTrenchBuffer() );
    writeToFile( m_conf.getRHBFile(), asciiBuffer.getRhbBuffer() );
    writeToFile( m_conf.getZFTFile(), asciiBuffer.getZFTBuffer() );

    if( hydrotopeWorkspace != null )
    {
      m_hydrotopManager.writeFile( asciiBuffer, hydrotopeWorkspace, modelWorkspace, parameterWorkspace );
      writeToFile( m_conf.getHydrotopFile(), asciiBuffer.getHydBuffer() );
    }

    if( parameterWorkspace != null )
    {
      m_bodartManager.writeFile( asciiBuffer, parameterWorkspace );
      m_bodtypManager.writeFile( asciiBuffer, parameterWorkspace );
      m_schneeManager.writeFile( asciiBuffer, parameterWorkspace );
      writeToFile( m_conf.getBodenartFile(), asciiBuffer.getBodartBuffer() );
      writeToFile( m_conf.getBodentypFile(), asciiBuffer.getBodtypBuffer() );
      writeToFile( m_conf.getSchneeFile(), asciiBuffer.getSnowBuffer() );
      m_nutzManager.writeFile( parameterWorkspace );
    }

    if( landuseWorkspace != null && sudsWorkspace != null )
    {
      final TreeMap<String, TreeMap<String, List<String>>> sudsMap = new TreeMap<String, TreeMap<String,List<String>>>();
//      final FeatureList hydrotopList = (FeatureList) hydrotopeWorkspace.getRootFeature().getProperty( NaModelConstants.HYDRO_MEMBER );
      final Feature catchmentCollection = (Feature) modelWorkspace.getRootFeature().getProperty( NaModelConstants.CATCHMENT_COLLECTION_MEMBER_PROP );
      final FeatureList catchmentList = (FeatureList) catchmentCollection.getProperty( NaModelConstants.CATCHMENT_MEMBER_PROP );

      final LanduseCollection landuseCollection = (LanduseCollection) landuseWorkspace.getRootFeature();
      for( final Landuse landuse : landuseCollection.getLanduses() )
      {
        if( landuse.getSudCollection().size() > 0 )
        {
          final GM_Object landuseGeometryProperty = landuse.getGeometry();
          final Geometry landuseGeometry = JTSAdapter.export( landuseGeometryProperty );
          final GM_Object landuseInteriorPoint = JTSAdapter.wrap( landuseGeometry.getInteriorPoint() );
          final List<Feature> list = catchmentList.query( landuseGeometryProperty.getEnvelope(), null );
          for( final Feature catchment : list )
            if( catchment.getDefaultGeometryPropertyValue().contains( landuseInteriorPoint ) )
            {
              final String catchmentName = catchment.getName();
              if( !sudsMap.containsKey( catchmentName ) )
                sudsMap.put( catchmentName, new TreeMap<String, List<String>>() );

//              final List<Feature> query = hydrotopList.query( landuseGeometryProperty.getEnvelope(), null );
//              for( final Feature hydrotop : query )
//              {
//                final GM_Object interiorPoint = JTSAdapter.wrap( JTSAdapter.export( hydrotop.getDefaultGeometryPropertyValue() ).getInteriorPoint() );
//                if( landuseGeometryProperty.contains( interiorPoint ) )
//                {
                  final Feature[] suds = landuse.getSuds();
                  for( final Feature s : suds )
                  {
                    final Feature f = s instanceof XLinkedFeature_Impl ? ((XLinkedFeature_Impl) s).getFeature() : s;
                    final String key;
                    final List<String> value = new ArrayList<String>();
                    if( f instanceof SwaleInfiltrationDitch )
                    {
                      /**
                        # Mulden-Rigolen Data for the subcatchment 4500
                        # Format:
                        # Catchment_NR. MR-Element_Type
                        # Area of MR-Element [m²]  Landusetyp  Soilprofil  max.Perkolation [mm/d] Aufteilungsfaktor-Grundwasser[%]   
                        # diameter-Drainpipe[mm] kf-Drainpipe [mm/d] Slope-Drainpipe [prommille] Roughness Drainpipe [mm] width of the MR-Element[m]    Nodenumber for the Draindischarge
                        4500 30
                        580. MRS_N mrs 2.8E-8 1.0
                        200. 4270. 0.003 2. 1.8 0
                        # ende MR TG 4500
                       */
                      final SwaleInfiltrationDitch sud = (SwaleInfiltrationDitch) f;
                      key = sud.getElementType().toString();
                      final double area = landuseGeometry.getArea() * sud.getAreaPercentage();
//                      final Feature drainageNode = sud.getDrainageNode();
//                      final String drainageNodeName = (drainageNode instanceof XLinkedFeature_Impl) ? ((XLinkedFeature_Impl) drainageNode).getFeature().getName() : getDrainageNodeName( catchment );
                      
                      final Object landuseClassLink = landuse.getLanduse();
                      final String landuseClassName = (landuseClassLink instanceof XLinkedFeature_Impl) ? ((XLinkedFeature_Impl) landuseClassLink).getFeature().getName() : "MRS_N";
                      sud.getLanduseFileName();
                      
                      value.add( String.format( "%.4g %s mrs %.4g %.4g", area, landuseClassName, sud.getMaxPercRate(), sud.getPercentToGroundwater() ) );
                      value.add( String.format( "%d %d %d %.4g %.4g 0", sud.getPipeDiameter(), sud.getPipeKfValue(), sud.getPipeSlope(), sud.getPipeRoughness(), sud.getWidth() ) );
                    }
                    else if( f instanceof Swale )
                    {
                      final Swale sud = (Swale) f;
                      key = sud.getElementType().toString();
                      final double area = landuseGeometry.getArea() * sud.getAreaPercentage();
//                      final Feature drainageNode = sud.getDrainageNode();
//                      final String drainageNodeName = (drainageNode instanceof XLinkedFeature_Impl) ? ((XLinkedFeature_Impl) drainageNode).getFeature().getName() : getDrainageNodeName( catchment );
                      final String drainageNodeName = "0";
                    }
                    else if( f instanceof Greenroof )
                    {
                      final Greenroof sud = (Greenroof) f;
                      key = sud.getElementType().toString();
                      final double area = landuseGeometry.getArea() * sud.getAreaPercentage();
                    }
                    else
                      continue;
                    // only one instance of certain suds type is allowed per catchment
                    final Map<String,List<String>> map = sudsMap.get( catchmentName );
                    map.put( key, value );
                  }
//                }
//              }
            }
//          final Object landuseClassLink = landuse.getLanduse();
//          final String landuseClassName;
//          if( landuseClassLink instanceof XLinkedFeature_Impl )
//            landuseClassName = ((XLinkedFeature_Impl) landuseClassLink).getName();
//          else
//            throw new Exception( "Landuse class not found." );
        }
      }
      final StringBuffer sudsBuffer = new StringBuffer();
      for( final String catchment : sudsMap.keySet() )
      {
        sudsBuffer.append( String.format( "# Catchment_NR %s\n", catchment ) );
        final TreeMap<String, List<String>> map = sudsMap.get( catchment );
        for( final String sudsID : map.keySet() )
        {
          final List<String> list = map.get( sudsID );
          if(list==null)continue;
          sudsBuffer.append( String.format( "%s %s\n", catchment,sudsID ) );
          for( final String line : list )
            sudsBuffer.append(line).append( "\n" );
        }
        sudsBuffer.append( String.format( "# Ende TG %s\n", catchment ) );
      }
      writeToFile( m_conf.getSwaleAndTrenchFile(), sudsBuffer );
    }
  }

//  private final String getDrainageNodeName( final Feature catchment )
//  {
//    final Object channelLink = catchment.getProperty( NaModelConstants.LINK_CATCHMENT_CHANNEL );
//    if( channelLink instanceof XLinkedFeature_Impl )
//    {
//      final Feature feature = ((XLinkedFeature_Impl) channelLink).getFeature();
//      if( feature instanceof XLinkedFeature_Impl )
//      {
//        final Object downstreamNodeLink = feature.getProperty( NaModelConstants.LINK_CHANNEL_DOWNSTREAMNODE );
//        if( downstreamNodeLink instanceof XLinkedFeature_Impl )
//        {
//          final Feature downstreamNode = ((XLinkedFeature_Impl) downstreamNodeLink).getFeature();
//          if( downstreamNode != null )
//            return downstreamNode.getName();
//        }
//      }
//    }
//    return null;
//  }

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

  private final void writeToFile( final File file, final StringBuffer buffer ) throws IOException
  {
    final FileOutputStream fileOutputStream = new FileOutputStream( file );
    final DataOutputStream stream = new DataOutputStream( fileOutputStream );
    stream.writeBytes( buffer.toString() );
    stream.close();
  }

// public static void featureToAscii( NAConfiguration conf, GMLWorkspace modelWorkspace, GMLWorkspace
  // parameterWorkspace, GMLWorkspace hydrotopWorkspace, GMLWorkspace synthNWorkspace, final NaNodeResultProvider
  // nodeResultProvider ) throws Exception
// {
// NAModellConverter main = new NAModellConverter( conf );
// main.write( modelWorkspace, parameterWorkspace, hydrotopWorkspace, synthNWorkspace, nodeResultProvider );
// }
}