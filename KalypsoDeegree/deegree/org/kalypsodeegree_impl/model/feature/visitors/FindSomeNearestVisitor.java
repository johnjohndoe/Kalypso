package org.kalypsodeegree_impl.model.feature.visitors;

import java.util.SortedMap;
import java.util.TreeMap;

import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureVisitor;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.cv.RectifiedGridCoverage;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * <p>
 * Sucht von allen Features jenes, welches am nächsten zu einem Punkt liegt.
 * </p>
 * <p>
 * Falls viele Objekte durchsucht werden, sollte die Suche zuerst durch ein
 * .query auf der FeatureList bzw. dem Workspace eingeschränkt werden.
 * </p>
 * 
 * @author belger
 */
public class FindSomeNearestVisitor implements FeatureVisitor
{
  /**
   * Hilfspunkt für die Suche, wird lazy instatiiert, da wir nicht wissen, ob
   * überhaupt eine Geoemetry gefunden wird
   */
  private GM_Point point = null;

  /** the neares features so for */
  private SortedMap m_result = new TreeMap();

  /** minimum distance so far */
  private double m_minDist = Double.MAX_VALUE;

  private final double m_radius;

  private final GM_Position m_pos;

  private final String m_geometryProperty;

  private final int m_number;

  /**
   *  
   */
  public FindSomeNearestVisitor( final GM_Position pos, final double radius, final int number, String geometryProperty )
  {
    m_pos = pos;
    m_radius = radius;
    m_number = number;
    m_geometryProperty = geometryProperty;
  }

  public FindSomeNearestVisitor( final GM_Position pos, final double radius, final int number )
  {
    m_pos = pos;
    m_radius = radius;
    m_number = number;
    m_geometryProperty = null;
  }

  public Feature[] getResult()
  {
    return (Feature[])m_result.values().toArray( new Feature[m_result.size()] );
  }

  private GM_Object getGeometry( Feature feature )
  {
    final GM_Object result;
    if( feature.getFeatureType().getName().equals( RectifiedGridCoverage.getName() ) )
    {
      // TODO nadja handle this better
      final GM_Object[] geoProps = feature.getGeometryProperties();
      result = geoProps[0];
    }
    else if( m_geometryProperty != null )
      result = (GM_Object)feature.getProperty( m_geometryProperty );
    else
      result = feature.getDefaultGeometryProperty();
    return result;
  }

  private double getDistance( GM_Object geom )
  {
    if( point == null || !( point.getCoordinateSystem().equals( geom.getCoordinateSystem() ) ) )
      point = GeometryFactory.createGM_Point( m_pos, geom.getCoordinateSystem() );
    return geom.distance( point );
  }

  /**
   * @see org.kalypsodeegree.model.feature.FeatureVisitor#visit(org.kalypsodeegree.model.feature.Feature)
   */
  public boolean visit( final Feature f )
  {
    final GM_Object fGeo = getGeometry( f );
    if( fGeo == null )
      return true;
    final double dist = getDistance( fGeo );
    if( dist <= m_radius && dist <= m_minDist )
    {
      m_result.put( new Double( dist ), f );
      final int size = m_result.size();
      if( size > m_number )
      {
        m_result.remove( m_result.lastKey() );
        m_minDist = ( (Double)m_result.lastKey() ).doubleValue();
      }
    }
    return true;
  }
}