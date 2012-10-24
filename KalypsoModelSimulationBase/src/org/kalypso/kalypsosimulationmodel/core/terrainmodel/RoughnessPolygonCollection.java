package org.kalypso.kalypsosimulationmodel.core.terrainmodel;

import java.util.ArrayList;
import java.util.List;

import javax.xml.namespace.QName;

import org.kalypso.kalypsosimulationmodel.schema.UrlCatalogModelSimulationBase;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Polygon;
import org.kalypsodeegree_impl.model.feature.FeatureBindingCollection;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.MultiPolygon;
import com.vividsolutions.jts.geom.Polygon;

/**
 * The default implementation of {@link IRoughnessPolygonCollection} based on {@link FeatureWrapperCollection}
 *
 * @author Dejan Antanaskovic, <a href="mailto:dejan.antanaskovic@tuhh.de">dejan.antanaskovic@tuhh.de</a>
 * @author Patrice Congo
 */
public class RoughnessPolygonCollection extends FeatureBindingCollection<IRoughnessPolygon> implements IRoughnessPolygonCollection
{
  public static final QName SIM_BASE_PROP_ROUGHNESS_LAYER_POLYGON = new QName( UrlCatalogModelSimulationBase.SIM_MODEL_NS, "roughnessLayerMember" ); //$NON-NLS-1$

  public RoughnessPolygonCollection( final Feature parentFeature )
  {
    super( parentFeature, IRoughnessPolygon.class, SIM_BASE_PROP_ROUGHNESS_LAYER_POLYGON );
  }

  @Override
  public boolean checkOverlapping( )
  {
    final List<Feature> srcPolygonsList = getFeatureList();
    try
    {
      for( int i = 0; i < srcPolygonsList.size(); i++ )
      {
        final Feature feature1 = srcPolygonsList.get( i );
        final Polygon polygon1 = (Polygon) JTSAdapter.export( feature1.getDefaultGeometryPropertyValue() );
        for( int j = i + 1; j < srcPolygonsList.size(); j++ )
        {
          final Feature feature2 = srcPolygonsList.get( j );
          final Polygon polygon2 = (Polygon) JTSAdapter.export( feature2.getDefaultGeometryPropertyValue() );
          final Geometry jtsIntersection = polygon1.intersection( polygon2 );
          if( jtsIntersection.getArea() > 0 )
          {
            return true;
          }
        }
      }
    }
    catch( final GM_Exception e )
    {
      e.printStackTrace();
    }
    return false;
  }

  @Override
  public List<IRoughnessPolygon> getOverlappedPolygons( )
  {
    final List<Feature> srcPolygonsList = getFeatureList();

    final List<IRoughnessPolygon> dstPolygonsList = new ArrayList<>();

    final int[] containsList = getEmptyList( srcPolygonsList.size() );

    try
    {
      for( int i = 0; i < srcPolygonsList.size(); i++ )
      {
        final Feature feature1 = srcPolygonsList.get( i );

        final IRoughnessPolygon roughnessPolygon1 = (IRoughnessPolygon) feature1;
        final MultiPolygon polygon1 = (MultiPolygon) JTSAdapter.export( feature1.getDefaultGeometryPropertyValue() );

        for( int j = i + 1; j < srcPolygonsList.size(); j++ )
        {
          final Feature feature2 = srcPolygonsList.get( j );
          final IRoughnessPolygon roughnessPolygon2 = (IRoughnessPolygon) feature2;
          final MultiPolygon polygon2 = (MultiPolygon) JTSAdapter.export( feature2.getDefaultGeometryPropertyValue() );

          final Geometry jtsIntersection = polygon1.intersection( polygon2 );
          if( jtsIntersection.getArea() > 0 )
          {
            if( !isInside( containsList, i ) )
            {
              dstPolygonsList.add( roughnessPolygon1 );
              addToList( containsList, i );
            }
            if( !isInside( containsList, j ) )
            {
              dstPolygonsList.add( roughnessPolygon2 );
              addToList( containsList, j );
            }
          }
        }
      }
    }
    catch( final GM_Exception e )
    {
      e.printStackTrace();
    }
    return dstPolygonsList;
  }

  @Override
  public List<IRoughnessPolygon> getRoughnessPolygons( )
  {
    return new ArrayList<>( this );
  }

  @Override
  public List<IRoughnessPolygon> selectRoughnessPolygons( final GM_Position location )
  {
    return query( location );
  }

  @Override
  public List<IRoughnessPolygon> selectRoughnessPolygons( final GM_Polygon selectionZone )
  {
    return query( selectionZone, IRoughnessPolygon.PROP_GEOMETRY, false );
  }

  private int[] getEmptyList( final int size )
  {
    final int[] list = new int[size];
    for( int i = 0; i < size; i++ )
      list[i] = -1;
    return list;
  }

  private boolean isInside( final int[] list, final int member )
  {
    for( final int element : list )
      if( element == member )
        return true;
    return false;
  }

  private int[] addToList( final int[] list, final int member )
  {
    for( int i = 0; i < list.length; i++ )
      if( list[i] == -1 )
        list[i] = member;
    return list;
  }

}
