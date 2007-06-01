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
package org.kalypso.ogc.gml.serialize;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import javax.xml.namespace.QName;

import org.kalypso.commons.xml.NS;
import org.kalypso.gmlschema.EmptyGMLSchema;
import org.kalypso.gmlschema.GMLSchemaFactory;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.IValuePropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.gmlschema.types.IMarshallingTypeHandler;
import org.kalypso.gmlschema.types.ITypeHandler;
import org.kalypso.gmlschema.types.ITypeRegistry;
import org.kalypso.gmlschema.types.MarshallingTypeRegistrySingleton;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.io.shpapi.DBaseException;
import org.kalypsodeegree_impl.io.shpapi.DBaseFile;
import org.kalypsodeegree_impl.io.shpapi.ShapeFile;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;
import org.kalypsodeegree_impl.model.feature.GMLUtilities;
import org.kalypsodeegree_impl.model.feature.GMLWorkspace_Impl;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * Helper-Klasse zum lesen und schreiben von GML TODO: Problem: reading/writing a shape will change the precision/size
 * of the columns!
 * 
 * @author gernot
 */
public class ShapeSerializer
{
  public static final QName PROPERTY_FEATURE_MEMBER = new QName( "namespace", "featureMember" );

  public static final QName PROPERTY_GEOMETRY = new QName( "namespace", "GEOM" );

  private static final QName PROPERTY_NAME = new QName( "namespace", "name" );

  private static final QName PROPERTY_TYPE = new QName( "namespace", "type" );

// private static final QName PROPERTY_BBOX = new QName( "namespace", "boundingBox" );

  private static final QName ROOT_FEATURETYPE = new QName( "namespace", "featureCollection" );

  private ShapeSerializer( )
  {
    // wird nicht instantiiert
  }

  public static void serialize( final GMLWorkspace workspace, final String filenameBase ) throws GmlSerializeException
  {
    final Feature rootFeature = workspace.getRootFeature();
    final List<Feature> features = (List<Feature>) rootFeature.getProperty( ShapeSerializer.PROPERTY_FEATURE_MEMBER );

    try
    {
      final ShapeFile shapeFile = new ShapeFile( filenameBase, "rw" );
      shapeFile.writeShape( features.toArray( new Feature[features.size()] ) );
      shapeFile.close();
    }
    catch( final Exception e )
    {
      e.printStackTrace();

      throw new GmlSerializeException( "Shape konnte nicht geschrieben werden", e );
    }
  }

  /**
   * Schreibt ein Array von Features in eine Shape-Datei. Dabei werden nicht einfach alle Properties geschrieben,
   * sondern nur die �ber ein vorher festgelegt Mapping.
   * 
   * @param features
   *            Properties dieser Features werden geschrieben.
   * @param mapping
   *            keys: Spaltennamen der Shape-Datei; Values: Property-Namen des Features
   * @param geoName
   *            Der Name der Property mit der Geometry
   * @param filenameBase
   *            Der Ausgabename f�r das Shape (.shp, .dbf, und. shx)
   */
  public static void serializeFeatures( final Feature[] features, final Map mapping, final String geoName, final String filenameBase ) throws GmlSerializeException
  {
    if( features.length == 0 )
    {
      return;
    }

    final IFeatureType featureType = features[0].getFeatureType();
    final IValuePropertyType geoPt = (IValuePropertyType) featureType.getProperty( geoName );

    final IPropertyType[] ftps = new IPropertyType[mapping.size() + 1];
    // ftps[0] = FeatureFactory.createFeatureTypeProperty( "GEOM", geoPt.getValueClass(), true );
    final ITypeHandler typeHandler = geoPt.getTypeHandler();
    ftps[0] = GMLSchemaFactory.createValuePropertyType( new QName( "namespace", "GEOM" ), typeHandler.getTypeName(), typeHandler, 0, 1, false );

    int count = 1;
    for( final Iterator mIt = mapping.entrySet().iterator(); mIt.hasNext(); )
    {
      final Map.Entry entry = (Entry) mIt.next();

      final IValuePropertyType ftp = (IValuePropertyType) featureType.getProperty( (String) entry.getValue() );

      // ftps[count] = FeatureFactory.createFeatureTypeProperty( (String) entry.getKey(), ftp.getValueClass(),
      // ftp.isNullable() );
      final ITypeHandler typeHandler2 = ftp.getTypeHandler();
      ftps[count] = GMLSchemaFactory.createValuePropertyType( new QName( "namespace", (String) entry.getKey() ), typeHandler2.getTypeName(), typeHandler2, 1, 1, false );
      count++;
    }

    // final IFeatureType shapeFeatureType = FeatureFactory.createFeatureType( "shapeType", null, ftps, occurs, occurs,
    // null, new HashMap() );
    final IFeatureType shapeFeatureType = GMLSchemaFactory.createFeatureType( new QName( "namespace", "shapeType" ), ftps );

    try
    {
      final Collection<Feature> shapeFeatures = new ArrayList<Feature>( features.length );
      for( int i = 0; i < features.length; i++ )
      {
        final Feature kalypsoFeature = features[i];

        final Object[] data = new Object[ftps.length];

        data[0] = kalypsoFeature.getProperty( geoName );

        int datacount = 1;
        for( final Iterator mIt = mapping.entrySet().iterator(); mIt.hasNext(); )
        {
          final Map.Entry entry = (Entry) mIt.next();

          data[datacount++] = kalypsoFeature.getProperty( (String) entry.getValue() );
        }

        final Feature feature = FeatureFactory.createFeature( null, null, "" + i, shapeFeatureType, data );
        shapeFeatures.add( feature );
      }

      final ShapeFile shapeFile = new ShapeFile( filenameBase, "rw" );
      shapeFile.writeShape( shapeFeatures.toArray( new Feature[shapeFeatures.size()] ) );
      shapeFile.close();
    }
    catch( final Exception e )
    {
      e.printStackTrace();

      throw new GmlSerializeException( "Shape konnte nicht geschrieben werden", e );
    }
  }

  public final static Feature createWorkspaceRootFeature( final IFeatureType featureType, final Object shapeFileType )
  {
    final Feature rootFeature = ShapeSerializer.createShapeRootFeature( featureType );
    new GMLWorkspace_Impl( new EmptyGMLSchema(), new IFeatureType[] { rootFeature.getFeatureType(), featureType }, rootFeature, null, null, null );

    rootFeature.setProperty( ShapeSerializer.PROPERTY_TYPE, shapeFileType );

    return rootFeature;
  }

  /**
   * Creates to feature type for the root feature of a shape-file-based workspace.
   * 
   * @param childFeatureType
   *            The feature type for the children (i.e. the shape-objects) of the root.
   * @return A newly created feature suitable for the root of a workspace. It has the following properties:
   *         <ul>
   *         <li>name : String [1] - some meaningful name</li>
   *         <li>type : int [1] - the shape-geometry-type</li>
   *         <li>featureMember : inline-feature with type childFeatureType [0,n]</li>
   *         </ul>
   */
  public static Feature createShapeRootFeature( final IFeatureType childFeatureType )
  {
    final ITypeRegistry<IMarshallingTypeHandler> registry = MarshallingTypeRegistrySingleton.getTypeRegistry();
    final ITypeHandler stringTH = registry.getTypeHandlerForTypeName( new QName( NS.XSD_SCHEMA, "string" ) );
    final ITypeHandler intTH = registry.getTypeHandlerForTypeName( new QName( NS.XSD_SCHEMA, "int" ) );

    final IPropertyType nameProp = GMLSchemaFactory.createValuePropertyType( ShapeSerializer.PROPERTY_NAME, stringTH.getTypeName(), stringTH, 1, 1, false );
    final IPropertyType typeProp = GMLSchemaFactory.createValuePropertyType( ShapeSerializer.PROPERTY_TYPE, new QName( "org.kalypso.gml.common.shape", "shapeType" ), intTH, 1, 1, false );
    final IRelationType memberProp = GMLSchemaFactory.createRelationType( ShapeSerializer.PROPERTY_FEATURE_MEMBER, childFeatureType, 0, IPropertyType.UNBOUND_OCCURENCY, false );
    final IPropertyType[] ftps = new IPropertyType[] { nameProp, typeProp, memberProp };
    final IFeatureType collectionFT = GMLSchemaFactory.createFeatureType( ShapeSerializer.ROOT_FEATURETYPE, ftps );

    return FeatureFactory.createFeature( null, null, "root", collectionFT, true );
  }

  public final static GMLWorkspace deserialize( final String fileBase, final CS_CoordinateSystem sourceCrs ) throws GmlSerializeException
  {
    ShapeFile sf = null;

    try
    {
      sf = new ShapeFile( fileBase );
      final IFeatureType featureType = sf.getFeatureType();
      final int fileShapeType = sf.getFileShapeType();

      final Feature rootFeature = ShapeSerializer.createWorkspaceRootFeature( featureType, fileShapeType );
      final GMLWorkspace workspace = rootFeature.getWorkspace();

      final IRelationType listRelation = (IRelationType) rootFeature.getFeatureType().getProperty( ShapeSerializer.PROPERTY_FEATURE_MEMBER );
      final List list = (List) rootFeature.getProperty( listRelation );

      // die shape-api liefert stets WGS84 als Koordinatensystem, daher
      // Anpassung hier:
      final int count = sf.getRecordNum();
      for( int i = 0; i < count; i++ )
      {
        final Feature fe = sf.getFeatureByRecNo( rootFeature, listRelation, i + 1, true );
        GMLUtilities.setCrs( fe, sourceCrs );
        if( fe != null )
        {
          list.add( fe );
        }
      }

      return workspace;
    }
    catch( final Exception e )
    {
      throw new GmlSerializeException( "Fehler beim Laden der Shape-Dateien", e );
    }
    finally
    {
      if( sf != null )
      {
        sf.close();
      }
    }
  }

  /** REMARK: we return a simple collection of features with no parent. Better we would return a GMLWorkspace. */
  public static Collection<Feature> readFeaturesFromDbf( final String basename )
  {
    DBaseFile dbf = null;
    try
    {
      // todo: zur Zeit gehen wird davon aus, dass der Typ immer '1' ist
      // ist das immer so?
      dbf = new DBaseFile( basename, 1 );

      final int recordNum = dbf.getRecordNum();
      final Collection<Feature> features = new ArrayList<Feature>( recordNum );
      for( int i = 0; i < recordNum; i++ )
      {
        final Feature feature = dbf.getFRow( null, null, i + 1, true );
        features.add( feature );
      }

      return features;
    }
    catch( final IOException e )
    {
      e.printStackTrace();

      return null;
    }
    catch( final DBaseException e )
    {
      e.printStackTrace();

      return null;
    }
    finally
    {
      if( dbf != null )
      {
        dbf.close();
      }
    }
  }
}