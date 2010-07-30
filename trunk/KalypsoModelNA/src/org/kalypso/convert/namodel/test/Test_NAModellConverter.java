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
import java.util.List;
import java.util.Map;
import java.util.logging.Logger;

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
  public static void completeascii2gml( final File gmlBaseDir, final File asciiBaseDir ) throws Exception
  {
    // final File gmlBaseDir = FileUtilities.createNewTempDir( "NA_gmlBaseDir" );
    // File asciiBaseDir = new File(
    // "D:\\FE-Projekte\\2004_SchulungIngBueros\\KalypsoSchulung\\tmp\\ex6-longterm\\solution" );

    final NAConfiguration conf = new NAConfiguration( asciiBaseDir, gmlBaseDir );
    final Logger anonymousLogger = Logger.getAnonymousLogger();
    final Feature modelRootFeature = NAModellConverter.modelAsciiToFeature( conf, anonymousLogger );

    final String shapeDir = "D:\\Kalypso_NA\\9-Modelle\\7-Rantzau\\05_GIS\\NA-Modell"; //$NON-NLS-1$
    insertSHPGeometries( modelRootFeature, shapeDir );

    final File modelGmlFile = new File( gmlBaseDir, "modell.gml" ); //$NON-NLS-1$
    final GMLSchemaCatalog schemaCatalog = KalypsoGMLSchemaPlugin.getDefault().getSchemaCatalog();
    final GMLSchema modelGmlSchema = schemaCatalog.getSchema( NaModelConstants.NS_NAMODELL, (String) null );

    final IFeatureType[] featureTypes = modelGmlSchema.getAllFeatureTypes();

    final GMLWorkspace modelWorkspace = new GMLWorkspace_Impl( modelGmlSchema, featureTypes, modelRootFeature, modelGmlFile.toURL(), null, " project:/.model/schema/namodell.xsd", null ); //$NON-NLS-1$
    GmlSerializer.serializeWorkspace( new FileWriter( modelGmlFile ), modelWorkspace );

    System.out.println( Messages.getString( "org.kalypso.convert.namodel.NAModellConverter.3" ) ); //$NON-NLS-1$
    System.out.println( Messages.getString( "org.kalypso.convert.namodel.NAModellConverter.4" ) + gmlBaseDir ); //$NON-NLS-1$
  }

  private static void insertSHPGeometries( final Feature modelFeature, final String shapeDir ) throws GmlSerializeException
  {
    // load ShapeFile
    final String cSystem = "EPSG:31467"; //$NON-NLS-1$

    final GMLWorkspace catchmentWorkspace = ShapeSerializer.deserialize( shapeDir + "\\rantzau_gebiete", cSystem ); //$NON-NLS-1$
    final List< ? > catchmentFeatures = (List< ? >) catchmentWorkspace.getRootFeature().getProperty( ShapeSerializer.PROPERTY_FEATURE_MEMBER );

    final GMLWorkspace channelWorkspace = ShapeSerializer.deserialize( shapeDir + "\\straenge", cSystem ); //$NON-NLS-1$
    final List< ? > channelFeatures = (List< ? >) channelWorkspace.getRootFeature().getProperty( ShapeSerializer.PROPERTY_FEATURE_MEMBER );

    final GMLWorkspace nodeWorkspace = ShapeSerializer.deserialize( shapeDir + "\\knoten", cSystem ); //$NON-NLS-1$
    final List< ? > nodeFeatures = (List< ? >) nodeWorkspace.getRootFeature().getProperty( ShapeSerializer.PROPERTY_FEATURE_MEMBER );

    // insertGeometries

    System.out.println( Messages.getString( "org.kalypso.convert.namodel.NAModellConverter.9" ) ); //$NON-NLS-1$
    final Feature catchmentCollection = (Feature) modelFeature.getProperty( NaModelConstants.CATCHMENT_COLLECTION_MEMBER_PROP );
    final List< ? > catchmentList = (List< ? >) catchmentCollection.getProperty( NaModelConstants.CATCHMENT_MEMBER_PROP );
    copyProperties( catchmentFeatures, "GEOM", "TGNR", catchmentList.toArray( new Feature[catchmentList.size()] ), "Ort", "name" ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$

    System.out.println( Messages.getString( "org.kalypso.convert.namodel.NAModellConverter.14" ) ); //$NON-NLS-1$
    final Feature channelCollection = (Feature) modelFeature.getProperty( NaModelConstants.CHANNEL_COLLECTION_MEMBER_PROP );
    final List< ? > channelList = (List< ? >) channelCollection.getProperty( NaModelConstants.CHANNEL_MEMBER_PROP );
    copyProperties( channelFeatures, "GEOM", "STRNR", channelList.toArray( new Feature[channelList.size()] ), "Ort", "name" ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$

    System.out.println( Messages.getString( "org.kalypso.convert.namodel.NAModellConverter.19" ) ); //$NON-NLS-1$
    final Feature nodeCollection = (Feature) modelFeature.getProperty( NaModelConstants.NODE_COLLECTION_MEMBER_PROP );
    final List< ? > nodeList = (List< ? >) nodeCollection.getProperty( NaModelConstants.NODE_MEMBER_PROP );
    copyProperties( nodeFeatures, "GEOM", "KTNR", nodeList.toArray( new Feature[nodeList.size()] ), "Ort", "name" ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
  }

  private static void copyProperties( final List< ? > catchmentFeatures, final String orgGeomPropName, final String orgIdPropName, final Feature[] destFE, final String destGeomPropName, final String destIdPropName )
  {
    final Map<String, Feature> orgHash = new HashMap<String, Feature>();
    for( final Object name : catchmentFeatures )
    {
      final Feature f = (Feature) name;
      final String id = f.getProperty( orgIdPropName ).toString();
      orgHash.put( id, f );
    }
    for( final Feature destFeature : destFE )
    {
      final String id = destFeature.getProperty( destIdPropName ).toString();
      // System.out.println("processing id=" + id);
      final Feature orgFeaure = orgHash.get( id );
      if( orgFeaure != null )
      {
        final Object value = orgFeaure.getProperty( orgGeomPropName );
        if( value == null )
          System.out.println( Messages.getString( "org.kalypso.convert.namodel.NAModellConverter.24" ) + id ); //$NON-NLS-1$
        // FeatureProperty fProp = FeatureFactory.createFeatureProperty( destGeomPropName, value );
        destFeature.setProperty( destGeomPropName, value );
      }
      else
        System.out.println( Messages.getString( "org.kalypso.convert.namodel.NAModellConverter.25" ) + id ); //$NON-NLS-1$
    }
  }
}