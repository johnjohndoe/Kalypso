package org.kalypsodeegree_impl.model.feature.visitors;

import java.util.HashMap;
import java.util.Map;

import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureVisitor;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree_impl.model.ct.GeoTransformer;
import org.kalypsodeegree_impl.tools.GeometryUtilities;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * Transforms all visited features to another coordinate system
 * 
 * @author belger
 */
public class TransformVisitor implements FeatureVisitor
{
  private GeoTransformer m_transformer;

  /** feature -> exception */
  private final Map<Feature, Throwable> m_exceptions = new HashMap<Feature, Throwable>();

  public TransformVisitor( final CS_CoordinateSystem targetCRS )
  {
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
   * Returns thrown exceptions while visiting
   */
  public Map<Feature, Throwable> getExceptions( )
  {
    return m_exceptions;
  }

  /**
   * @see org.kalypsodeegree.model.feature.FeatureVisitor#visit(org.kalypsodeegree.model.feature.Feature)
   */
  public boolean visit( final Feature f )
  {
    try
    {
      final IPropertyType[] ftps = f.getFeatureType().getProperties();
      for( int i = 0; i < ftps.length; i++ )
      {
        // TODO: also handle list of geoobjects

        final IPropertyType ftp = ftps[i];
        if( GeometryUtilities.isGeometry( ftp ) )
        {
          final GM_Object object = (GM_Object) f.getProperty( ftp );
          if( object != null )
          {
            final GM_Object newGeo = m_transformer.transform( object );
            f.setProperty( ftp, newGeo );
          }
        }
      }
    }
    catch( final Exception e )
    {
      e.printStackTrace();

      m_exceptions.put( f, e );
    }

    return true;
  }

}