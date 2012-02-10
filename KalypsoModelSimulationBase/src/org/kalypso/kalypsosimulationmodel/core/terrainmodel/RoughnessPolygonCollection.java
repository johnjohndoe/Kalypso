package org.kalypso.kalypsosimulationmodel.core.terrainmodel;

import java.util.ArrayList;
import java.util.List;

import javax.xml.namespace.QName;

import org.kalypso.kalypsosimulationmodel.schema.UrlCatalogModelSimulationBase;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.binding.FeatureWrapperCollection;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Surface;
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
public class RoughnessPolygonCollection extends FeatureWrapperCollection<IRoughnessPolygon> implements IRoughnessPolygonCollection
{
  public static final QName SIM_BASE_PROP_ROUGHNESS_LAYER_POLYGON = new QName( UrlCatalogModelSimulationBase.SIM_MODEL_NS, "roughnessLayerMember" ); //$NON-NLS-1$

  public RoughnessPolygonCollection( final Feature featureToBind )
  {
    this( featureToBind, IRoughnessPolygon.class, SIM_BASE_PROP_ROUGHNESS_LAYER_POLYGON );
  }

  public RoughnessPolygonCollection( final Feature featureCol, final Class<IRoughnessPolygon> fwClass, final QName featureMemberProp )
  {
    super( featureCol, fwClass, featureMemberProp );
  }

  public RoughnessPolygonCollection( final Feature parentFeature, final QName childQName, final QName featureMemberProp, final Class<IRoughnessPolygon> fwClass ) throws IllegalArgumentException
  {
    super( parentFeature, childQName, featureMemberProp, fwClass );
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRoughnessPolygonCollection#checkOverlapping()
   */
  @Override
  @SuppressWarnings("unchecked")//$NON-NLS-1$
  public boolean checkOverlapping( )
  {
    final List<Feature> srcPolygonsList = getWrappedList();
    Feature feature1, feature2;
    Polygon polygon1, polygon2;
    try
    {
      for( int i = 0; i < srcPolygonsList.size(); i++ )
      {
        feature1 = srcPolygonsList.get( i );
        polygon1 = (Polygon) JTSAdapter.export( feature1.getDefaultGeometryProperty() );
        for( int j = i + 1; j < srcPolygonsList.size(); j++ )
        {
          feature2 = srcPolygonsList.get( j );
          polygon2 = (Polygon) JTSAdapter.export( feature2.getDefaultGeometryProperty() );
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
  @SuppressWarnings("unchecked")//$NON-NLS-1$
  public List<IRoughnessPolygon> getOverlappedPolygons( )
  {
    final List<Feature> srcPolygonsList = getWrappedList();
    final List<IRoughnessPolygon> dstPolygonsList = new ArrayList<IRoughnessPolygon>();
    final int[] containsList = getEmptyList( srcPolygonsList.size() );
    IRoughnessPolygon roughnessPolygon1, roughnessPolygon2;
    Feature feature1, feature2;
    MultiPolygon polygon1, polygon2;
    try
    {
      for( int i = 0; i < srcPolygonsList.size(); i++ )
      {
        feature1 = srcPolygonsList.get( i );
        roughnessPolygon1 = new RoughnessPolygon( feature1 );
        polygon1 = (MultiPolygon) JTSAdapter.export( feature1.getDefaultGeometryProperty() );
        for( int j = i + 1; j < srcPolygonsList.size(); j++ )
        {
          feature2 = srcPolygonsList.get( j );
          roughnessPolygon2 = new RoughnessPolygon( feature2 );
          polygon2 = (MultiPolygon) JTSAdapter.export( feature2.getDefaultGeometryProperty() );
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
    // List<Feature> srcPolygonsList = getWrappedList();
    // List<IRoughnessPolygon> dstPolygonsList = new ArrayList<IRoughnessPolygon>();
    // Iterator<Feature> iterator = srcPolygonsList.listIterator();
    // while( iterator.hasNext() )
    // {
    // dstPolygonsList.add( new RoughnessPolygon( iterator.next() ) );
    // }
    // return dstPolygonsList;
    final List<IRoughnessPolygon> dstPolygonsList = new ArrayList<IRoughnessPolygon>( this );
    return dstPolygonsList;
  }

  @Override
  public List<IRoughnessPolygon> selectRoughnessPolygons( final GM_Position location )
  {
    return query( location );

    // List<Feature> srcPolygonsList = getWrappedList();
    // List<Feature> dstPolygonsList = new ArrayList<Feature>();
    // Iterator<Feature> iterator = srcPolygonsList.listIterator();
    // Feature polygon = null;
    // while( iterator.hasNext() )
    // {
    // polygon = iterator.next();
    // if( polygon.getDefaultGeometryProperty().contains( location ) )
    // dstPolygonsList.add( polygon );
    // }
    // if( dstPolygonsList.size() > 0 )
    // {
    // IRoughnessPolygon[] dstPolygonsArray = new IRoughnessPolygon[dstPolygonsList.size()];
    // for( int i = 0; i < dstPolygonsList.size(); i++ )
    // dstPolygonsArray[i] = new RoughnessPolygon( dstPolygonsList.get( i ) );
    // return dstPolygonsArray;
    // }
    // else
    // return null;

    // /*
    // * Mark from Dejan: The following code does not work correctly,
    // * it checks if ENVELOPE contains point! GM_Position
    // */
    // position = GeometryFactory.createGM_Position( point.getX(), point.getY() );
    // List<Feature> selectedPolygonsList = getWrappedList().query( position, null );
    // IRoughnessPolygon[] dstPolygonsArray = new IRoughnessPolygon[selectedPolygonsList.size()];
    // Iterator<Feature> iterator = selectedPolygonsList.listIterator();
    // int i = 0;
    // while(iterator.hasNext()){
    // dstPolygonsArray[i++] = new RoughnessPolygon(iterator.next());
    // }
    // return dstPolygonsArray;

  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRoughnessPolygonCollection#selectRoughnessPolygons(org.kalypsodeegree.model.geometry.GM_Polygon)
   */
  @Override
  public List<IRoughnessPolygon> selectRoughnessPolygons( final GM_Surface< ? > selectionZone )
  {
    return query( selectionZone, false );
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
