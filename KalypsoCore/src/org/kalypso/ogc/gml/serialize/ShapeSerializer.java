package org.kalypso.ogc.gml.serialize;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;

import org.deegree.model.feature.Feature;
import org.deegree.model.feature.FeatureType;
import org.deegree.model.feature.FeatureTypeProperty;
import org.deegree_impl.io.shpapi.ShapeFile;
import org.deegree_impl.model.feature.FeatureFactory;
import org.kalypso.ogc.gml.GMLHelper;
import org.kalypso.ogc.gml.KalypsoFeatureLayer;
import org.kalypso.util.progress.IProgressMonitor;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * Helper-Klasse zum lesen und schreiben von GML
 * 
 * @author gernot
 */
public class ShapeSerializer
{
  private ShapeSerializer()
  {
  // wird nicht instantiiert
  }

  public static void serialize( final Feature[] features, final Map mapping, final String geoName, final String filenameBase ) throws GmlSerializeException
  {
    if( features.length == 0 )
      return;
    
    final FeatureType featureType = features[0].getFeatureType();

    final FeatureTypeProperty geomFeatureType = featureType.getProperty( geoName );

    final FeatureTypeProperty[] ftps = new FeatureTypeProperty[mapping.size() + 1];
    ftps[0] = FeatureFactory.createFeatureTypeProperty( "GEOM", geomFeatureType.getType(),
        geomFeatureType.isNullable() );
    int count = 1;
    for( final Iterator mIt = mapping.entrySet().iterator(); mIt.hasNext(); )
    {
      final Map.Entry entry = (Entry)mIt.next();

      final FeatureTypeProperty ftp = featureType.getProperty( (String)entry.getValue() );

      ftps[count++] = FeatureFactory.createFeatureTypeProperty( (String)entry.getKey(), ftp
          .getType(), ftp.isNullable() );
    }

    final FeatureType shapeFeatureType = FeatureFactory.createFeatureType( null, null, featureType
        .getName(), ftps );

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
  
  public final static void serialize( final KalypsoFeatureLayer layer, final Map mapping,
      final String geometryName, final String filenameBase ) throws GmlSerializeException
  {
    serialize( layer.getAllFeatures(), mapping, geometryName, filenameBase );
  }

  /**
   * @deprecated TODO: deserialize a RootedFeature
   */
  public final static KalypsoFeatureLayer deserialize( final String fileBase,
      final CS_CoordinateSystem sourceCrs, final CS_CoordinateSystem targetCrs,
      final IProgressMonitor monitor ) throws GmlSerializeException
  {
    try
    {
      if( monitor != null )
        monitor.beginTask( "Shape wird geladen", 100 );

      final ShapeFile sf = new ShapeFile( fileBase );
      final FeatureType featureType = sf.getFeatureByRecNo( 1 ).getFeatureType();
      final String name = featureType.getName();
      final KalypsoFeatureLayer layer = new KalypsoFeatureLayer( name, featureType, targetCrs, null );

      // die shape-api liefert stets WGS84 als Koordinatensystem, daher
      // Anpassung hier:
      final int count = sf.getRecordNum();
      for( int i = 0; i < count; i++ )
      {
        final Feature fe = sf.getFeatureByRecNo( i + 1, true );
        GMLHelper.setCrs( fe, sourceCrs );
        if( fe != null )
          layer.addFeature( fe );
      }

      sf.close();

      layer.optimize();

      if( monitor != null )
        monitor.done();

      return layer;
    }
    catch( final Exception e )
    {
      throw new GmlSerializeException( "Fehler beim Laden der Shape-Dateien", e );
    }
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
//      final FeatureTypeProperty collectionFTP = FeatureFactory.createFeatureTypeProperty( "list", Feature.class.getName(), false );
//      final FeatureType collectionType = FeatureFactory.createFeatureType( "collection", null, new FeatureTypeProperty[] { collectionFTP }, new int[] { 0 }, new int[] { 0 }, null, null );
//      
////      final KalypsoFeatureLayer layer = new KalypsoFeatureLayer( name, featureType, targetCrs );
//
//      final Feature collection = FeatureFactory.createFeature( "0", collectionType );
//      final Object property = collection.getProperty( "list" );
//
//      
////      // die shape-api liefert stets WGS84 als Koordinatensystem, daher
////      // Anpassung hier:
////      final int count = sf.getRecordNum();
////      for( int i = 0; i < count; i++ )
////      {
////        final Feature fe = sf.getFeatureByRecNo( i + 1 );
////        GMLHelper.setCrs( fe, sourceCrs );
////        if( fe != null )
////          layer.addFeature( fe );
////      }
//
//      sf.close();
//
////      layer.optimize();
//
//      
//      if( monitor != null )
//        monitor.done();
//
//      return collection; 
//    }
//    catch( final Exception e )
//    {
//      throw new GmlSerializeException( "Fehler beim Laden der Shape-Dateien", e );
//    }
//  }
}