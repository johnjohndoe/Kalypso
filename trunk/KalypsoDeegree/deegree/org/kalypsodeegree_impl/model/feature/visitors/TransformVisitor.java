package org.deegree_impl.model.feature.visitors;

import org.deegree.model.feature.Feature;
import org.deegree.model.feature.FeatureProperty;
import org.deegree.model.feature.FeatureTypeProperty;
import org.deegree.model.feature.FeatureVisitor;
import org.deegree.model.geometry.GM_Object;
import org.deegree_impl.model.ct.GeoTransformer;
import org.deegree_impl.model.feature.FeatureFactory;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * Transforms all visited features to another coordinate system
 * 
 * @author belger
 */
public class TransformVisitor implements FeatureVisitor
{
  private final CS_CoordinateSystem m_targetCRS;

  private GeoTransformer m_transformer;

  public TransformVisitor( final CS_CoordinateSystem targetCRS )
  {
    m_targetCRS = targetCRS;

    try
    {
      m_transformer = new GeoTransformer( targetCRS );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }
  }

  /**
   * @see org.deegree.model.feature.FeatureVisitor#visit(org.deegree.model.feature.Feature)
   */
  public boolean visit( final Feature f ) throws Throwable
  {
    final FeatureTypeProperty[] ftps = f.getFeatureType().getProperties();
    for( int i = 0; i < ftps.length; i++ )
    {
      // TODO: also handle list of geoobjects

      final FeatureTypeProperty ftp = ftps[i];
      if( ftp.isGeometryProperty() )
      {
        final GM_Object object = (GM_Object)f.getProperty( ftp.getName() );
        if( object != null )
        {
          GM_Object newGeo = m_transformer.transform( object );
          FeatureProperty fProp = FeatureFactory.createFeatureProperty( ftp.getName(), newGeo );
          f.setProperty( fProp );
        }
      }
    }

    return true;
  }

}