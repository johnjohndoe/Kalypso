package org.kalypso.ogc.gml.serialize;

import java.io.File;
import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;

import org.deegree.model.feature.Feature;
import org.deegree.model.feature.FeatureCollection;
import org.deegree.model.feature.FeatureType;
import org.deegree.model.feature.FeatureTypeProperty;
import org.deegree_impl.io.shpapi.ShapeFile;
import org.deegree_impl.model.feature.FeatureFactory;
import org.kalypso.ogc.gml.GMLHelper;
import org.kalypso.ogc.gml.KalypsoFeature;
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

  public final static void serialize( final KalypsoFeatureLayer layer, final Map mapping,
      final String geometryName, final File file ) throws GmlSerializeException
  {
    final FeatureType featureType = layer.getFeatureType();
    
    final FeatureTypeProperty geomFeatureType = featureType.getProperty(geometryName);
    
    final FeatureTypeProperty[] ftps = new FeatureTypeProperty[mapping.size() + 1];
    ftps[0] = FeatureFactory.createFeatureTypeProperty( "GEOM", geomFeatureType.getType(), geomFeatureType.isNullable() );
    int count = 1;
    for( final Iterator mIt = mapping.entrySet().iterator(); mIt.hasNext(); )
    {
      final Map.Entry entry = (Entry)mIt.next();
      
      final FeatureTypeProperty ftp = featureType.getProperty( (String)entry.getValue() );
      
      ftps[count++] = FeatureFactory.createFeatureTypeProperty( (String)entry.getKey(), ftp.getType(), ftp.isNullable() );
    }    
    
    final FeatureType shapeFeatureType = FeatureFactory.createFeatureType( null, null, featureType.getName(), ftps );
    
    try
    {
      final ShapeFile shapeFile = new ShapeFile( file.toString(), "rw" );

      final KalypsoFeature[] features = layer.getAllFeatures();
      final FeatureCollection fc = FeatureFactory.createFeatureCollection( "collection",
          features.length );

      for( int i = 0; i < features.length; i++ )
      {
        final KalypsoFeature kalypsoFeature = features[i];

        final Object[] data = new Object[mapping.size() + 1];

        data[0] = kalypsoFeature.getProperty( geometryName );
        
        int datacount = 1;
        for( final Iterator mIt = mapping.entrySet().iterator(); mIt.hasNext(); )
        {
          final Map.Entry entry = (Entry)mIt.next();
          
          data[datacount++] = kalypsoFeature.getProperty( (String)entry.getValue() );
        }
        
       final Feature feature = FeatureFactory.createFeature( "" + i, shapeFeatureType, data );
        
        // TODO: change CRS to WGS84 als Koordinatensystem ?

        fc.appendFeature( feature );
      }

      shapeFile.writeShape( fc );
      shapeFile.close();
    }
    catch( final Exception e )
    {
      throw new GmlSerializeException( "Shape konnte nicht geschrieben werden", e );
    }
  }

  public final static KalypsoFeatureLayer deserialize( final String fileBase,
      final CS_CoordinateSystem sourceCrs, final CS_CoordinateSystem targetCrs,
      final IProgressMonitor monitor ) throws GmlSerializeException
  {
    try
    {
      monitor.beginTask( "Shape wird geladen", 100 );

      final ShapeFile sf = new ShapeFile( fileBase );
      final FeatureType featureType = sf.getFeatureByRecNo( 1 ).getFeatureType();
      final String name = featureType.getName();
      final KalypsoFeatureLayer layer = new KalypsoFeatureLayer( name, featureType, targetCrs );

      // die shape-api liefert stets WGS84 als Koordinatensystem, daher
      // Anpassung hier:
      final int count = sf.getRecordNum();
      for( int i = 0; i < count; i++ )
      {
        final Feature fe = sf.getFeatureByRecNo( i + 1 );
        GMLHelper.setCrs( fe, sourceCrs );
        if( fe != null )
          layer.addFeature( new KalypsoFeature( fe ) );
      }

      sf.close();

      layer.optimize();

      monitor.done();

      return layer;
    }
    catch( final Exception e )
    {
      throw new GmlSerializeException( "Fehler beim Laden der Shape-Dateien", e );
    }
  }
}