package org.kalypso.ogc.gml.serialize;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.deegree.model.feature.Feature;
import org.deegree.model.feature.FeatureType;
import org.deegree.model.feature.FeatureTypeProperty;
import org.deegree.model.feature.GMLWorkspace;
import org.deegree.model.geometry.GM_Envelope;
import org.deegree_impl.io.shpapi.DBaseException;
import org.deegree_impl.io.shpapi.DBaseFile;
import org.deegree_impl.io.shpapi.ShapeFile;
import org.deegree_impl.model.feature.FeatureAssociationTypeProperty_Impl;
import org.deegree_impl.model.feature.FeatureFactory;
import org.deegree_impl.model.feature.GMLHelper;
import org.deegree_impl.model.feature.GMLWorkspace_Impl;
import org.kalypso.util.progress.IProgressMonitor;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * Helper-Klasse zum lesen und schreiben von GML
 * 
 * @author gernot
 */
public class ShapeSerializer
{
  public static final String PROPERTY_FEATURE_MEMBER = "featureMember";

  public static final String PROPERTY_GEOMETRY = "GEOM";

  private static final String PROPERTY_NAME = "name";

  private static final String PROPERTY_BBOX = "boundingBox";

  private ShapeSerializer()
  {
  // wird nicht instantiiert
  }

  public static void serialize( final GMLWorkspace workspace, final String filenameBase )
      throws GmlSerializeException
  {
    final Feature rootFeature = workspace.getRootFeature();
    final List features = (List)rootFeature.getProperty( PROPERTY_FEATURE_MEMBER );

    try
    {
      final ShapeFile shapeFile = new ShapeFile( filenameBase, "rw" );
      shapeFile.writeShape( (Feature[])features.toArray( new Feature[features.size()] ) );
      shapeFile.close();
    }
    catch( final Exception e )
    {
      e.printStackTrace();

      throw new GmlSerializeException( "Shape konnte nicht geschrieben werden", e );
    }
  }

  /**
   * Schreibt ein Array von Features in eine Shape-Datei. Dabei werden nicht
   * einfach alle Properties geschrieben, sondern nur die über ein vorher
   * festgelegt Mapping.
   * 
   * @param features
   *          Properties dieser Features werden geschrieben.
   * @param mapping
   *          keys: Spaltennamen der Shape-Datei; Values: Property-Namen des
   *          Features
   * @param geoName
   *          Der Name der Property mit der Geometry
   * @param filenameBase
   *          Der Ausgabename für das Shape (.shp, .dbf, und. shx)
   * 
   *  
   */
  public static void serializeFeatures( final Feature[] features, final Map mapping,
      final String geoName, final String filenameBase ) throws GmlSerializeException
  {
    if( features.length == 0 )
      return;

    final FeatureType featureType = features[0].getFeatureType();

    final FeatureTypeProperty geomFeatureType = featureType.getProperty( geoName );

    final FeatureTypeProperty[] ftps = new FeatureTypeProperty[mapping.size() + 1];
    ftps[0] = FeatureFactory.createFeatureTypeProperty( "GEOM", geomFeatureType.getType(),
        geomFeatureType.isNullable() );
    final int[] occurs = new int[ftps.length];

    int count = 1;
    for( final Iterator mIt = mapping.entrySet().iterator(); mIt.hasNext(); )
    {
      final Map.Entry entry = (Entry)mIt.next();

      final FeatureTypeProperty ftp = featureType.getProperty( (String)entry.getValue() );

      occurs[count] = 1;
      ftps[count] = FeatureFactory.createFeatureTypeProperty( (String)entry.getKey(),
          ftp.getType(), ftp.isNullable() );

      count++;
    }

    final FeatureType shapeFeatureType = FeatureFactory.createFeatureType( "shapeType", null, ftps,
        occurs, occurs, null, null );

    //    final FeatureType shapeFeatureType = FeatureFactory.createFeatureType(
    // null, null, featureType
    //        .getName(), ftps );

    try
    {
      final Collection shapeFeatures = new ArrayList( features.length );
      for( int i = 0; i < features.length; i++ )
      {
        final Feature kalypsoFeature = features[i];

        final Object[] data = new Object[ftps.length];

        data[0] = kalypsoFeature.getProperty( geoName );

        int datacount = 1;
        for( final Iterator mIt = mapping.entrySet().iterator(); mIt.hasNext(); )
        {
          final Map.Entry entry = (Entry)mIt.next();

          data[datacount++] = kalypsoFeature.getProperty( (String)entry.getValue() );
        }

        final Feature feature = FeatureFactory.createFeature( "" + i, shapeFeatureType, data );

        shapeFeatures.add( feature );
      }

      final ShapeFile shapeFile = new ShapeFile( filenameBase, "rw" );
      shapeFile.writeShape( (Feature[])shapeFeatures.toArray( new Feature[shapeFeatures.size()] ) );
      shapeFile.close();
    }
    catch( final Exception e )
    {
      e.printStackTrace();

      throw new GmlSerializeException( "Shape konnte nicht geschrieben werden", e );
    }
  }

  public final static GMLWorkspace deserialize( final String fileBase,
      final CS_CoordinateSystem sourceCrs, final IProgressMonitor monitor )
      throws GmlSerializeException
  {
    try
    {
      if( monitor != null )
        monitor.beginTask( "Shape wird geladen", 100 );

      final ShapeFile sf = new ShapeFile( fileBase );
      final FeatureType featureType = sf.getFeatureByRecNo( 1 ).getFeatureType();

      final Feature rootFeature = createShapeRootFeature( featureType );
      final List list = (List)rootFeature.getProperty( PROPERTY_FEATURE_MEMBER );

      // die shape-api liefert stets WGS84 als Koordinatensystem, daher
      // Anpassung hier:
      final int count = sf.getRecordNum();
      for( int i = 0; i < count; i++ )
      {
        final Feature fe = sf.getFeatureByRecNo( i + 1, true );
        GMLHelper.setCrs( fe, sourceCrs );
        if( fe != null )
          list.add( fe );
      }

      // TODO transform it!

      sf.close();

      if( monitor != null )
        monitor.done();

      return new GMLWorkspace_Impl( new FeatureType[]
      {
          rootFeature.getFeatureType(),
          featureType }, rootFeature, null, null );
    }
    catch( final Exception e )
    {
      throw new GmlSerializeException( "Fehler beim Laden der Shape-Dateien", e );
    }
  }

  private static Feature createShapeRootFeature( final FeatureType ft )
  {
    final FeatureTypeProperty nameProp = FeatureFactory.createFeatureTypeProperty( PROPERTY_NAME,
        null, String.class.getName(), true, null );
    final FeatureTypeProperty boundingProp = FeatureFactory.createFeatureTypeProperty(
        PROPERTY_BBOX, null, GM_Envelope.class.getName(), true, null );
    final FeatureTypeProperty memberProp = new FeatureAssociationTypeProperty_Impl(
        PROPERTY_FEATURE_MEMBER, null, "FeatureAssociationType", false, ft, null );

    FeatureTypeProperty[] ftps = new FeatureTypeProperty[]
    {
        nameProp,
        boundingProp,
        memberProp };
    final FeatureType collectionFT = FeatureFactory.createFeatureType( "featureCollection", null,
        ftps, new int[]
        {
            1,
            1,
            0 }, new int[]
        {
            1,
            1,
            FeatureType.UNBOUND_OCCURENCY }, null, null );

    return FeatureFactory.createFeature( "root", collectionFT );
  }

  public static Collection readFeaturesFromDbf( final String basename )
  {
    Collection features = null;
    try
    {
      // todo: zur Zeit gehen wird davon aus, dass der Typ immer '1' ist
      // ist das immer so?
      final DBaseFile dbf = new DBaseFile( basename, 1 );

      final int recordNum = dbf.getRecordNum();
      features = new ArrayList( recordNum );
      for( int i = 0; i < recordNum; i++ )
      {
        final Feature feature = dbf.getFRow( i + 1, true );
        features.add( feature );
      }
      
      dbf.close();
    }
    catch( IOException e )
    {
      e.printStackTrace();
    }
    catch( DBaseException e )
    {
      e.printStackTrace();
    }

    return features;
  }

  //  /**
  //   * Return a Feature wich contiains a collection of features
  //   */
  //  public final static Feature deserializeFeature( final String fileBase,
  //      final CS_CoordinateSystem sourceCrs, final CS_CoordinateSystem targetCrs,
  //      final IProgressMonitor monitor ) throws GmlSerializeException
  //  {
  //    try
  //    {
  //      if( monitor != null )
  //        monitor.beginTask( "Shape wird geladen", 1000 );
  //
  //      final ShapeFile sf = new ShapeFile( fileBase );
  //      final FeatureType featureType = sf.getFeatureByRecNo( 1 ).getFeatureType();
  //      
  //      final FeatureTypeProperty collectionFTP =
  // FeatureFactory.createFeatureTypeProperty( "list", Feature.class.getName(),
  // false );
  //      final FeatureType collectionType = FeatureFactory.createFeatureType(
  // "collection", null, new FeatureTypeProperty[] { collectionFTP }, new int[]
  // { 0 }, new int[] { 0 }, null, null );
  //      
  //// final KalypsoFeatureLayer layer = new KalypsoFeatureLayer( name,
  // featureType, targetCrs );
  //
  //      final Feature collection = FeatureFactory.createFeature( "0",
  // collectionType );
  //      final Object property = collection.getProperty( "list" );
  //
  //      
  //// // die shape-api liefert stets WGS84 als Koordinatensystem, daher
  //// // Anpassung hier:
  //// final int count = sf.getRecordNum();
  //// for( int i = 0; i < count; i++ )
  //// {
  //// final Feature fe = sf.getFeatureByRecNo( i + 1 );
  //// GMLHelper.setCrs( fe, sourceCrs );
  //// if( fe != null )
  //// layer.addFeature( fe );
  //// }
  //
  //      sf.close();
  //
  //// layer.optimize();
  //
  //      
  //      if( monitor != null )
  //        monitor.done();
  //
  //      return collection;
  //    }
  //    catch( final Exception e )
  //    {
  //      throw new GmlSerializeException( "Fehler beim Laden der Shape-Dateien", e
  // );
  //    }
  //  }
}