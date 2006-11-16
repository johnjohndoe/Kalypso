package org.kalypso.gis.doubleraster.walker;

import org.deegree.model.feature.Feature;
import org.deegree.model.feature.FeatureProperty;
import org.deegree.model.geometry.GM_Position;
import org.deegree_impl.model.feature.FeatureFactory;
import org.deegree_impl.model.geometry.GeometryFactory;
import org.kalypso.gis.doubleraster.DoubleRaster;
import org.kalypso.gis.doubleraster.DoubleRasterWalker;

import com.bce.gis.index.FeatureCollectionIndex;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Summiert das Gesamtvolumen des Rasters sowie die Einzelvolumina der Features.
 * 
 * @author belger
 */
public class FeatureVolumeRasterWalker implements DoubleRasterWalker
{
  private final FeatureFactory m_featureFactory = new FeatureFactory();

  private final String m_volumeProperty;

  private final FeatureCollectionIndex m_featureIndex;

  private double m_cellArea = 0.0;

  private int m_count = 0;

  private GeometryFactory m_geometryFactory = new GeometryFactory();

  /**
   * @param featureIndex
   */
  public FeatureVolumeRasterWalker( final FeatureCollectionIndex featureIndex, final String volumeProperty )
  {
    m_featureIndex = featureIndex;
    m_volumeProperty = volumeProperty;
  }

  /**
   * @see org.kalypso.gis.doubleraster.DoubleRasterWalker#start(org.kalypso.gis.doubleraster.DoubleRaster)
   */
  public void start( final DoubleRaster r )
  {
    final Coordinate offsetX = r.getOffsetX();
    final Coordinate offsetY = r.getOffsetY();
    m_cellArea = offsetX.x * offsetY.y - offsetX.y * offsetY.x;
  }

  /**
   * @see org.kalypso.gis.doubleraster.DoubleRasterWalker#operate(int, int, com.vividsolutions.jts.geom.Coordinate)
   */
  public void operate( final int x, final int y, final Coordinate c )
  {
    final double cellVolume = m_cellArea * c.z;

    if( !Double.isNaN( cellVolume ) )
    {
      final GM_Position position = m_geometryFactory.createGM_Position( c.x, c.y );
      final Feature[] features = m_featureIndex.query( position );

      for( int i = 0; i < features.length; i++ )
      {
        final Feature f = features[i];
        if( f.getDefaultGeometryProperty().contains( position ) )
        {
          final Object oldVolumeObj = f.getProperty( m_volumeProperty );
          if( oldVolumeObj instanceof Double )
          {
            final double oldVolume = (Double) oldVolumeObj;

            final Double newVolume;
            if( Double.isNaN( oldVolume ) )
              newVolume = new Double( cellVolume );
            else
            {
              if( Math.signum( oldVolume ) != Math.signum( cellVolume ) )
              {
                m_count++;
              }

              newVolume = new Double( oldVolume + cellVolume );
            }

            final FeatureProperty fp = m_featureFactory.createFeatureProperty( m_volumeProperty, newVolume );
            f.setProperty( fp );
          }
        }
      }
    }
  }

  /**
   * @see org.kalypso.gis.doubleraster.DoubleRasterWalker#afterLine(int)
   */
  public void afterLine( final int y )
  {
    // nichts zu tun
  }

  /**
   * @see org.kalypso.gis.doubleraster.DoubleRasterWalker#getResult()
   */
  public Object getResult( )
  {
    return null;
  }
}
