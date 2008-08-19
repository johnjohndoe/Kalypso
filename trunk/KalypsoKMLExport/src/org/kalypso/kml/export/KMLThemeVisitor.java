/**
 *
 */
package org.kalypso.kml.export;

import org.kalypso.ogc.gml.mapmodel.IKalypsoThemePredicate;
import org.kalypso.ogc.gml.mapmodel.visitor.KalypsoThemeVisitor;

/**
 * @author kuch
 */
public class KMLThemeVisitor extends KalypsoThemeVisitor
{

  /**
   * @param containers
   * @param predicate
   */
  public KMLThemeVisitor( final IKalypsoThemePredicate predicate )
  {
    super( predicate );
  }

}
