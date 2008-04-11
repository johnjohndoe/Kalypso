/**
 *
 */
package org.kalypso.google.earth.export;

import org.kalypso.ogc.gml.mapmodel.IKalypsoThemePredicate;
import org.kalypso.ogc.gml.mapmodel.visitor.KalypsoThemeVisitor;

/**
 * @author kuch
 */
public class GoogleEarthThemeVisitor extends KalypsoThemeVisitor
{

  /**
   * @param containers
   * @param predicate
   */
  public GoogleEarthThemeVisitor( final IKalypsoThemePredicate predicate )
  {
    super( predicate );
  }

}
