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
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import javax.xml.namespace.QName;

import org.kalypso.commons.xml.XmlTypes;
import org.kalypso.core.i18n.Messages;
import org.kalypso.gmlschema.GMLSchema;
import org.kalypso.gmlschema.GMLSchemaFactory;
import org.kalypso.gmlschema.IGMLSchema;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.IValuePropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.gmlschema.types.IMarshallingTypeHandler;
import org.kalypso.gmlschema.types.ITypeRegistry;
import org.kalypso.gmlschema.types.MarshallingTypeRegistrySingleton;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.io.shpapi.DBaseException;
import org.kalypsodeegree_impl.io.shpapi.DBaseFile;
import org.kalypsodeegree_impl.io.shpapi.ShapeFile;
import org.kalypsodeegree_impl.io.shpapi.dataprovider.IShapeDataProvider;
import org.kalypsodeegree_impl.io.shpapi.dataprovider.StandardShapeDataProvider;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;
import org.kalypsodeegree_impl.model.feature.GMLUtilities;
import org.kalypsodeegree_impl.model.feature.GMLWorkspace_Impl;

/**
 * Helper-Klasse zum lesen und schreiben von GML TODO: Problem: reading/writing a shape will change the precision/size
 * of the columns!
 * 
 * @author gernot
 */
public class ShapeSerializer
{
  private static final String SHP_NAMESPACE_URI = DBaseFile.SHP_NAMESPACE_URI;

  private static final QName ROOT_FEATURETYPE = new QName( SHP_NAMESPACE_URI, "ShapeCollection" ); //$NON-NLS-1$

  private static final QName PROPERTY_SHAPE_TYPE = new QName( SHP_NAMESPACE_URI, "ShapeType" ); //$NON-NLS-1$

  public static final QName PROPERTY_FEATURE_MEMBER = ShapeFile.PROPERTY_FEATURE_MEMBER;

  private static final QName PROPERTY_NAME = new QName( SHP_NAMESPACE_URI, "name" ); //$NON-NLS-1$

  private static final QName PROPERTY_TYPE = new QName( SHP_NAMESPACE_URI, "type" ); //$NON-NLS-1$

  /**
   * Pseudo QNAME, placeholder for the gml-id to be written.
   */
  public static final QName QNAME_GMLID = new QName( Gml2ShapeConverter.class.getName() + "gmlid" ); //$NON-NLS-1$

  private ShapeSerializer( )
  {
    // wird nicht instantiiert
  }

  public static void serialize( final GMLWorkspace workspace, final String filenameBase, IShapeDataProvider shapeDataProvider ) throws GmlSerializeException
  {
    final Feature rootFeature = workspace.getRootFeature();
    final List<Feature> features = (List<Feature>) rootFeature.getProperty( PROPERTY_FEATURE_MEMBER );

    try
    {
      final ShapeFile shapeFile = new ShapeFile( filenameBase, "rw" ); //$NON-NLS-1$

      // if no dataProvider is set take the StandardProvider
      if( shapeDataProvider == null )
      {
        shapeDataProvider = new StandardShapeDataProvider( features.toArray( new Feature[features.size()] ) );
      }

      shapeFile.writeShape( shapeDataProvider );
      shapeFile.close();
    }
    catch( final Exception e )
    {
      e.printStackTrace();

      throw new GmlSerializeException( Messages.getString( "org.kalypso.ogc.gml.serialize.ShapeSerializer.7" ), e ); //$NON-NLS-1$
    }
  }

  /**
   * Schreibt ein Array von Features in eine Shape-Datei. Dabei werden nicht einfach alle Properties geschrieben,
   * sondern nur die über ein vorher festgelegt Mapping.
   * 
   * @param features
   *          Properties dieser Features werden geschrieben.
   * @param mapping
   *          keys: Spaltennamen der Shape-Datei; Values: Property-Namen des Features
   * @param geoName
   *          Der Name der Property mit der Geometry
   * @param filenameBase
   *          Der Ausgabename für das Shape (.shp, .dbf, und. shx)
   */
  public static void serializeFeatures( final Feature[] features, final Map<String, String> mapping, final String geoName, final String filenameBase, StandardShapeDataProvider shapeDataProvider ) throws GmlSerializeException
  {
    if( features.length == 0 )
      return;

    final IFeatureType featureType = features[0].getFeatureType();
    final IValuePropertyType geoPt = (IValuePropertyType) featureType.getProperty( geoName );

    final IPropertyType[] ftps = new IPropertyType[mapping.size() + 1];
    final IMarshallingTypeHandler typeHandler = geoPt.getTypeHandler();
    final String namespace = featureType.getQName().getNamespaceURI();
    final QName geomP = new QName( namespace, ShapeFile.GEOM );
    ftps[0] = GMLSchemaFactory.createValuePropertyType( geomP, typeHandler, 0, 1, false );

    int count = 1;
    for( final Map.Entry<String, String> entry : mapping.entrySet() )
    {
      final IValuePropertyType ftp = (IValuePropertyType) featureType.getProperty( entry.getValue() );

      // ftps[count] = FeatureFactory.createFeatureTypeProperty( (String) entry.getKey(), ftp.getValueClass(),
      // ftp.isNullable() );
      final IMarshallingTypeHandler typeHandler2 = ftp.getTypeHandler();
      ftps[count] = GMLSchemaFactory.createValuePropertyType( new QName( SHP_NAMESPACE_URI, entry.getKey() ), typeHandler2, 1, 1, false );
      count++;
    }

    // final IFeatureType shapeFeatureType = FeatureFactory.createFeatureType( "shapeType", null, ftps, occurs, occurs,
    // null, new HashMap() );
    final IFeatureType shapeFeatureType = GMLSchemaFactory.createFeatureType( PROPERTY_SHAPE_TYPE, ftps );

    try
    {
      final Collection<Feature> shapeFeatures = new ArrayList<Feature>( features.length );
      for( int i = 0; i < features.length; i++ )
      {
        final Feature kalypsoFeature = features[i];

        final Object[] data = new Object[ftps.length];

        data[0] = kalypsoFeature.getProperty( geoName );

        int datacount = 1;
        for( final Entry<String, String> entry : mapping.entrySet() )
        {
          data[datacount++] = kalypsoFeature.getProperty( entry.getValue() );
        }

        final Feature feature = FeatureFactory.createFeature( null, null, "" + i, shapeFeatureType, data ); //$NON-NLS-1$
        shapeFeatures.add( feature );
      }

      final ShapeFile shapeFile = new ShapeFile( filenameBase, "rw" ); //$NON-NLS-1$

      if( shapeDataProvider == null )
      {
        shapeDataProvider = new StandardShapeDataProvider( shapeFeatures.toArray( new Feature[shapeFeatures.size()] ) );
      }
      else
      {
        shapeDataProvider.setFeatures( shapeFeatures.toArray( new Feature[shapeFeatures.size()] ) );
      }

      shapeFile.writeShape( shapeDataProvider );

      shapeFile.close();
    }
    catch( final Exception e )
    {
      e.printStackTrace();

      throw new GmlSerializeException( Messages.getString( "org.kalypso.ogc.gml.serialize.ShapeSerializer.10" ), e ); //$NON-NLS-1$
    }
  }

  /**
   * Schreibt ein Array von Features in eine Shape-Datei. Dabei werden nicht einfach alle Properties geschrieben,
   * sondern nur die über ein vorher festgelegt Mapping.
   * 
   * @param features
   *          Properties dieser Features werden geschrieben.
   * @param mapping
   *          keys: Spaltennamen der Shape-Datei; Values: Property-Namen des Features
   * @param geoName
   *          Der Name der Property mit der Geometry
   * @param filenameBase
   *          Der Ausgabename für das Shape (.shp, .dbf, und. shx)
   */
  public static void serializeFeatures( final Feature[] features, final Map<String, QName> mapping, final QName geomProperty, final String filenameBase, IShapeDataProvider shapeDataProvider ) throws GmlSerializeException
  {
    if( features.length == 0 )
      throw new GmlSerializeException( Messages.getString( "org.kalypso.ogc.gml.serialize.ShapeSerializer.11" ) ); //$NON-NLS-1$

    final IFeatureType featureType = features[0].getFeatureType();
    final IValuePropertyType geoPt = (IValuePropertyType) featureType.getProperty( geomProperty );

    final IPropertyType[] ftps = new IPropertyType[mapping.size() + 1];
    final IMarshallingTypeHandler geoTypeHandler = geoPt.getTypeHandler();
    final String namespace = featureType.getQName().getNamespaceURI();
    final QName geomP = new QName( namespace, ShapeFile.GEOM );
    ftps[0] = GMLSchemaFactory.createValuePropertyType( geomP, geoTypeHandler, 0, 1, false );

    int count = 1;
    for( final Entry<String, QName> entry : mapping.entrySet() )
    {
      final QName qname = entry.getValue();

      if( qname == ShapeSerializer.QNAME_GMLID )
      {
        /* If it is the pseudo gml-id qname, create a string-property */
        final IMarshallingTypeHandler typeHandler = MarshallingTypeRegistrySingleton.getTypeRegistry().getTypeHandlerForTypeName( XmlTypes.XS_STRING ); //$NON-NLS-1$
        ftps[count] = GMLSchemaFactory.createValuePropertyType( new QName( SHP_NAMESPACE_URI, entry.getKey() ), typeHandler, 1, 1, false );
      }
      else
      {
        /* for each given value property create a corresponding type */
        final IValuePropertyType ftp = (IValuePropertyType) featureType.getProperty( qname );
        final IMarshallingTypeHandler typeHandler = ftp.getTypeHandler();
        ftps[count] = GMLSchemaFactory.createValuePropertyType( new QName( SHP_NAMESPACE_URI, entry.getKey() ), typeHandler, 1, 1, false );
      }
      count++;
    }

    final IFeatureType shapeFeatureType = GMLSchemaFactory.createFeatureType( PROPERTY_SHAPE_TYPE, ftps );

    try
    {
      final Collection<Feature> shapeFeatures = new ArrayList<Feature>( features.length );
      for( int i = 0; i < features.length; i++ )
      {
        final Feature kalypsoFeature = features[i];

        final Object[] data = new Object[ftps.length];

        final IPropertyType geomPT = kalypsoFeature.getFeatureType().getProperty( geomProperty );

        final Object geom = kalypsoFeature.getProperty( geomProperty );
        if( geomPT.isList() )
        {
          // HACK: if geomProperty is a list property, we just take the first element
          // as Shape does not support lists of geometries.
          final List< ? > geomList = (List< ? >) geom;
          if( !geomList.isEmpty() )
          {
            data[0] = geomList.get( 0 );
          }
        }
        else
        {
          data[0] = geom;
        }

        int datacount = 1;
        for( final Entry<String, QName> entry : mapping.entrySet() )
        {
          final QName qname = entry.getValue();
          if( qname == ShapeSerializer.QNAME_GMLID )
          {
            data[datacount++] = kalypsoFeature.getId();
          }
          else
          {
            data[datacount++] = kalypsoFeature.getProperty( qname );
          }
        }

        final Feature feature = FeatureFactory.createFeature( null, null, "" + i, shapeFeatureType, data ); //$NON-NLS-1$
        shapeFeatures.add( feature );
      }

      final ShapeFile shapeFile = new ShapeFile( filenameBase, "rw" ); //$NON-NLS-1$

      // if no dataProvider is set take the StandardProvider
      if( shapeDataProvider == null )
      {
        shapeDataProvider = new StandardShapeDataProvider( shapeFeatures.toArray( new Feature[shapeFeatures.size()] ) );
      }
      else
      {
        shapeDataProvider.setFeatures( shapeFeatures.toArray( new Feature[shapeFeatures.size()] ) );
      }

      shapeFile.writeShape( shapeDataProvider );

      shapeFile.close();
    }
    catch( final Throwable e )
    {
      e.printStackTrace();

      throw new GmlSerializeException( Messages.getString( "org.kalypso.ogc.gml.serialize.ShapeSerializer.15" ) + e.getMessage(), e ); //$NON-NLS-1$
    }
  }

  public final static Feature createWorkspaceRootFeature( final IFeatureType featureType, final Object shapeFileType )
  {
    final IGMLSchema schema = featureType.getGMLSchema();
    final IFeatureType[] featureTypes = schema.getAllFeatureTypes();
    final Feature rootFeature = ShapeSerializer.createShapeRootFeature( featureType );
    final String schemaLocation = schema instanceof GMLSchema ? ((GMLSchema) schema).getContext().toExternalForm() : null;
    new GMLWorkspace_Impl( schema, featureTypes, rootFeature, null, null, schemaLocation, null );
    rootFeature.setProperty( ShapeSerializer.PROPERTY_TYPE, shapeFileType );
    return rootFeature;
  }

  /**
   * Creates to feature type for the root feature of a shape-file-based workspace.
   * 
   * @param childFeatureType
   *          The feature type for the children (i.e. the shape-objects) of the root.
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
    final IMarshallingTypeHandler stringTH = registry.getTypeHandlerForTypeName( XmlTypes.XS_STRING ); //$NON-NLS-1$
    final IMarshallingTypeHandler intTH = registry.getTypeHandlerForTypeName( XmlTypes.XS_INT ); //$NON-NLS-1$

    final IPropertyType nameProp = GMLSchemaFactory.createValuePropertyType( ShapeSerializer.PROPERTY_NAME, stringTH, 1, 1, false );
    final IPropertyType typeProp = GMLSchemaFactory.createValuePropertyType( ShapeSerializer.PROPERTY_TYPE, intTH, 1, 1, false );
    final IRelationType memberProp = GMLSchemaFactory.createRelationType( PROPERTY_FEATURE_MEMBER, childFeatureType, 0, IPropertyType.UNBOUND_OCCURENCY, false );
    final IPropertyType[] ftps = new IPropertyType[] { nameProp, typeProp, memberProp };
    final QName fcQName = new QName( "http://www.opengis.net/gml", "_FeatureCollection" );
    final IFeatureType collectionFT = GMLSchemaFactory.createFeatureType( ShapeSerializer.ROOT_FEATURETYPE, ftps, childFeatureType.getGMLSchema(), fcQName );
    return FeatureFactory.createFeature( null, null, "root", collectionFT, true ); //$NON-NLS-1$
  }

  public final static GMLWorkspace deserialize( final String fileBase, final String sourceCrs ) throws GmlSerializeException
  {
    ShapeFile sf = null;

    try
    {
      sf = new ShapeFile( fileBase );
      final IFeatureType featureType = sf.getFeatureType();
      final int fileShapeType = sf.getFileShapeType();

      final Feature rootFeature = ShapeSerializer.createWorkspaceRootFeature( featureType, fileShapeType );
      final GMLWorkspace workspace = rootFeature.getWorkspace();

      final IRelationType listRelation = (IRelationType) rootFeature.getFeatureType().getProperty( PROPERTY_FEATURE_MEMBER );

      // die shape-api liefert stets WGS84 als Koordinatensystem, daher
      // Anpassung hier:
      final int count = sf.getRecordNum();
      for( int i = 0; i < count; i++ )
      {
        final Feature fe = sf.getFeatureByRecNo( rootFeature, listRelation, i + 1, true );
        GMLUtilities.setCrs( fe, sourceCrs );
        if( fe != null )
        {
          workspace.addFeatureAsComposition( rootFeature, listRelation, -1, fe );
        }
      }

      return workspace;
    }
    catch( final Exception e )
    {
      throw new GmlSerializeException( Messages.getString( "org.kalypso.ogc.gml.serialize.ShapeSerializer.19" ), e ); //$NON-NLS-1$
    }
    finally
    {
      if( sf != null )
      {
        try
        {
          sf.close();
        }
        catch( final IOException e )
        {
          throw new GmlSerializeException( Messages.getString( "org.kalypso.ogc.gml.serialize.ShapeSerializer.20" ), e ); //$NON-NLS-1$
        }
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
        try
        {
          dbf.close();
        }
        catch( final IOException e )
        {
          e.printStackTrace();
        }
      }
    }
  }

}