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
package org.kalypso.convert.namodel.test;

import java.io.File;
import java.io.FileWriter;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;

import org.kalypso.convert.namodel.NAConfiguration;
import org.kalypso.convert.namodel.NAModellConverter;
import org.kalypso.convert.namodel.NaModelConstants;
import org.kalypso.convert.namodel.i18n.Messages;
import org.kalypso.gmlschema.GMLSchema;
import org.kalypso.gmlschema.GMLSchemaCatalog;
import org.kalypso.gmlschema.KalypsoGMLSchemaPlugin;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.ogc.gml.serialize.GmlSerializeException;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ogc.gml.serialize.ShapeSerializer;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.feature.GMLWorkspace_Impl;

/**
 * import and export of kalypso rainfall runoff models converts between custom ascii format and gml format. importing
 * ascii is always processed into a gml file-structure (includes generating of zml files). export to ascii can be
 * generated from a gml file or from a gml workspace
 * 
 * @author doemming
 */
public class Test_NAModellConverter
{
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
    Feature modelRootFeature = NAModellConverter.modelAsciiToFeature( conf );

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

// public static void featureToAscii( NAConfiguration conf, GMLWorkspace modelWorkspace, GMLWorkspace
  // parameterWorkspace, GMLWorkspace hydrotopWorkspace, GMLWorkspace synthNWorkspace, final NaNodeResultProvider
  // nodeResultProvider ) throws Exception
// {
// NAModellConverter main = new NAModellConverter( conf );
// main.write( modelWorkspace, parameterWorkspace, hydrotopWorkspace, synthNWorkspace, nodeResultProvider );
// }
}