package org.kalypsodeegree_impl.model.feature.visitors;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.transformation.GeoTransformer;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.FeatureVisitor;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree_impl.gml.schema.virtual.VirtualFeatureTypeProperty;
import org.kalypsodeegree_impl.model.feature.Feature_Impl;
import org.kalypsodeegree_impl.tools.GeometryUtilities;

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

  public TransformVisitor( final String targetCRS )
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
      final IFeatureType featureType = f.getFeatureType();

      final IPropertyType[] ftps = featureType.getProperties();
      for( final IPropertyType ftp : ftps )
      {
        if( ftp instanceof VirtualFeatureTypeProperty )
          continue;
        if( featureType.isVirtualProperty( ftp ) )
          continue;

        if( f instanceof Feature_Impl )
        {
          if( ((Feature_Impl) f).isFunctionProperty( ftp ) )
            continue;
        }

        boolean wasTransformed = false;

        if( GeometryUtilities.isGeometry( ftp ) )
        {
          if( ftp.isList() )
          {
            final List<GM_Object> geomList = (List<GM_Object>) f.getProperty( ftp );
            final int size = geomList.size();
            for( int i = 0; i < size; i++ )
            {
              final GM_Object geom = geomList.get( i );
              final GM_Object transformedGeom = transformProperty( geom );

              wasTransformed = wasTransformed | (geom != transformedGeom);

              geomList.set( i, transformedGeom );
            }
          }
          else
          {
            final GM_Object object = (GM_Object) f.getProperty( ftp );
            final GM_Object transformedGeom = transformProperty( object );

            wasTransformed = object != transformedGeom;

            f.setProperty( ftp, transformedGeom );
          }

          // HACK: we invalidate the complete geo-index, in order to make sure the complete bbox of the list is
          // correctly set.
          if( wasTransformed )
          {
            final Feature parent = f.getParent();
            if( parent != null )
            {
              final Object parentList = parent.getProperty( f.getParentRelation() );
              if( parentList instanceof FeatureList )
                ((FeatureList) parentList).invalidate();
            }
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

  private GM_Object transformProperty( final GM_Object object ) throws Exception
  {
    if( object == null )
      return null;

    return m_transformer.transform( object );
  }

}