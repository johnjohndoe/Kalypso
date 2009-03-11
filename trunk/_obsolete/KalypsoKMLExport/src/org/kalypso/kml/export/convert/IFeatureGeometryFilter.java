/**
 *
 */
package org.kalypso.kml.export.convert;

import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Object;

/**
 * @author Dirk Kuch
 */
public interface IFeatureGeometryFilter
{
  public GM_Object[] getGeometries( Feature f );
}
