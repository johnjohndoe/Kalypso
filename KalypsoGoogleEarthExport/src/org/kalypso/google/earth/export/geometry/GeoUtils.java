/**
 *
 */
package org.kalypso.google.earth.export.geometry;

import org.apache.commons.lang.NotImplementedException;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_MultiCurve;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Surface;

/**
 * @author kuch
 */
public class GeoUtils
{
  public enum GEOMETRY_TYPE
  {
    eMultiCurve,
    eCurve,
    eSurface,
    ePoint
  }

  /**
   * @param gmo
   * @return
   */
  public static GEOMETRY_TYPE getGeoType( final GM_Object gmo )
  {
    if( gmo instanceof GM_MultiCurve )
      return GEOMETRY_TYPE.eMultiCurve;
    else if( gmo instanceof GM_Curve )
      return GEOMETRY_TYPE.eCurve;
    else if( gmo instanceof GM_Surface )
      return GEOMETRY_TYPE.eSurface;
    else if( gmo instanceof GM_Point )
      return GEOMETRY_TYPE.ePoint;

    throw (new NotImplementedException());
  }

}
