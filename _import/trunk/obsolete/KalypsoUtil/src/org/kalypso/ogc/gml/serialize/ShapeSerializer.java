package org.kalypso.ogc.gml.serialize;

import java.io.File;

import org.deegree.model.feature.Feature;
import org.deegree.model.feature.FeatureCollection;
import org.deegree.model.feature.FeatureType;
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

  public final static void serialize( final KalypsoFeatureLayer layer, final String[] columns,
      final File file ) throws GmlSerializeException
  {
    try
    {
      final ShapeFile shapeFile = new ShapeFile( file.toString(), "w" );

      final KalypsoFeature[] features = layer.getAllFeatures();
      final FeatureCollection fc = FeatureFactory.createFeatureCollection( "collection",
          features.length );

      for( int i = 0; i < features.length; i++ )
      {
        // TODO: only Properties specified in 'columns'
        
        // TODO: change CRS to WGS84 als Koordinatensystem ?

        fc.appendFeature( features[i] );
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