package org.kalypso.ogc.gml.serialize;

import java.util.List;

import org.deegree.model.feature.Feature;
import org.deegree.model.feature.FeatureType;
import org.deegree.model.feature.FeatureTypeProperty;
import org.deegree.model.feature.GMLWorkspace;
import org.deegree.model.geometry.GM_Envelope;
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

  public static void serialize( final GMLWorkspace workspace, final String filenameBase ) throws GmlSerializeException
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
  
  public final static GMLWorkspace deserialize( final String fileBase,
      final CS_CoordinateSystem sourceCrs, final IProgressMonitor monitor ) throws GmlSerializeException
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

      return new GMLWorkspace_Impl( new FeatureType[] { rootFeature.getFeatureType(), featureType }, rootFeature, null );
    }
    catch( final Exception e )
    {
      throw new GmlSerializeException( "Fehler beim Laden der Shape-Dateien", e );
    }
  }

private static Feature createShapeRootFeature( final FeatureType ft )
{
  final FeatureTypeProperty nameProp = FeatureFactory.createFeatureTypeProperty( PROPERTY_NAME, null, String.class.getName(), true, null );
  final FeatureTypeProperty boundingProp = FeatureFactory.createFeatureTypeProperty( PROPERTY_BBOX, null, GM_Envelope.class.getName(), true, null );
  final FeatureTypeProperty memberProp = new FeatureAssociationTypeProperty_Impl( PROPERTY_FEATURE_MEMBER, null, "FeatureAssociationType", false, ft, null ); 
    
  FeatureTypeProperty[] ftps = new FeatureTypeProperty[] { nameProp, boundingProp, memberProp };
  final FeatureType collectionFT = FeatureFactory.createFeatureType( "featureCollection", null, ftps, new int[] { 1, 1, 0 }, new int[] { 1, 1, FeatureType.UNBOUND_OCCURENCY }, null, null );
  
  return FeatureFactory.createFeature( "root", collectionFT );
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