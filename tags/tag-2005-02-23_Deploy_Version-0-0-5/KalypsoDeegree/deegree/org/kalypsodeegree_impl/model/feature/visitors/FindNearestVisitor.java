package org.deegree_impl.model.feature.visitors;

import org.deegree.model.feature.Feature;
import org.deegree.model.feature.FeatureVisitor;
import org.deegree.model.geometry.GM_Object;
import org.deegree.model.geometry.GM_Point;
import org.deegree.model.geometry.GM_Position;
import org.deegree_impl.model.geometry.GeometryFactory;

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
public class FindNearestVisitor implements FeatureVisitor
{
  /**
   * Hilfspunkt für die Suche, wird lazy instatiiert, da wir nicht wissen, ob
   * überhaupt eine Geoemetry gefunden wird
   */
  private GM_Point point = null;

  /** das bislang näheste feature */
  private Feature m_result = null;

  /** bisher minimale entfernung zum Punkt */
  private double m_minDist = Double.MAX_VALUE;

  private final double m_radius;

  private final GM_Position m_pos;

  public FindNearestVisitor( final GM_Position pos, final double radius )
  {
    m_pos = pos;
    m_radius = radius;
  }

  public Feature getResult()
  {
    return m_result;
  }

  /**
   * @see org.deegree.model.feature.FeatureVisitor#visit(org.deegree.model.feature.Feature)
   */
  public boolean visit( final Feature f )
  {
    final GM_Object fGeo = f.getDefaultGeometryProperty();

    if( fGeo != null )
    {
      if( point == null )
        point = GeometryFactory.createGM_Point( m_pos, fGeo.getCoordinateSystem() );

      final double dist = fGeo.distance( point );
      if( dist < m_radius && dist < m_minDist )
      {
        m_result = f;
        m_minDist = dist;
      }
    }

    return true;
  }

}