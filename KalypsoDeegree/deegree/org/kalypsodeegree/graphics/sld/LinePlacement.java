/*--------------- Kalypso-Deegree-Header ------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
 
 
 history:
 
 Files in this package are originally taken from deegree and modified here
 to fit in kalypso. As goals of kalypso differ from that one in deegree
 interface-compatibility to deegree is wanted but not retained always. 
 
 If you intend to use this software in other ways than in kalypso 
 (e.g. OGC-web services), you should consider the latest version of deegree,
 see http://www.deegree.org .

 all modifications are licensed as deegree, 
 original copyright:
 
 Copyright (C) 2001 by:
 EXSE, Department of Geography, University of Bonn
 http://www.giub.uni-bonn.de/exse/
 lat/lon GmbH
 http://www.lat-lon.de
 
 ---------------------------------------------------------------------------------------------------*/
package org.kalypsodeegree.graphics.sld;

import org.kalypsodeegree.filterencoding.FilterEvaluationException;
import org.kalypsodeegree.model.feature.Feature;

/**
 * Incarnation of an sld:LinePlacement-element.
 * <p>
 * Contains some deegree-specific extensions:
 * <ul>
 * <li>PerpendicularOffset: may be used as defined by the OGC, but it can also be set to one of the special values
 * 'center', 'above', 'below', 'auto'
 * <li>Gap: defines the distance between two captions on the line string
 * <li>LineWidth: provides the thickness of the styled line (needed as information for the correct positioning of
 * labels above and below the line string)
 * </ul>
 * <p>
 * 
 * @author <a href="mailto:mschneider@lat-lon.de">Markus Schneider </a>
 * @version $Revision$ $Date$
 */
public interface LinePlacement
{

  final static int TYPE_ABSOLUTE = 0;

  final static int TYPE_ABOVE = 1;

  final static int TYPE_BELOW = 2;

  final static int TYPE_CENTER = 3;

  final static int TYPE_AUTO = 4;

  /**
   * The PerpendicularOffset element of a LinePlacement gives the perpendicular distance away from a line to draw a
   * label. The distance is in pixels and is positive to the left-hand side of the line string. Negative numbers mean
   * right. The default offset is 0.
   * <p>
   * deegree-specific extension: if the element has one of the values: 'center', 'above', 'below', 'auto', the return
   * value is invalid
   * <p>
   * 
   * @param feature
   *          specifies the <tt>Feature</tt> to be used for evaluation of the underlying 'sld:ParameterValueType'
   * @return the offset (only valid if type is TYPE_ABSOLUTE)
   * @throws FilterEvaluationException
   *           if the evaluation fails
   */
  double getPerpendicularOffset( Feature feature ) throws FilterEvaluationException;

  /**
   * @see #getPerpendicularOffset(Feature)
   *      <p>
   * @param perpendicularOffset
   */
  void setPerpendicularOffset( double perpendicularOffset );

  /**
   * Returns the placement type (one of the constants defined in <tt>LinePlacement</tt>).
   * <p>
   * 
   * @param feature
   * @return
   * @throws FilterEvaluationException
   */
  int getPlacementType( Feature feature ) throws FilterEvaluationException;

  /**
   * Sets the placement type (one of the constants defined in <tt>LinePlacement</tt>).
   * <p>
   * 
   * @param placementType
   */
  void setPlacementType( int placementType );

  /**
   * Provides the thickness of the styled line (needed as information for the correct positioning of labels above and
   * below the line string).
   * <p>
   * 
   * @param feature
   * @return
   * @throws FilterEvaluationException
   */
  double getLineWidth( Feature feature ) throws FilterEvaluationException;

  /**
   * Provides the thickness of the styled line (needed as information for the correct positioning of labels above and
   * below the line string).
   * <p>
   * 
   * @param lineWidth
   *          the lineWidth to be set
   */
  void setLineWidth( double lineWidth );

  /**
   * Defines the distance between two captions on the line string. One unit is the width of the label caption.
   * <p>
   * 
   * @param feature
   * @return
   * @throws FilterEvaluationException
   */
  int getGap( Feature feature ) throws FilterEvaluationException;

  /**
   * Defines the distance between two captions on the line string. One unit is the width of the label caption.
   * <p>
   * 
   * @param gap
   *          the gap to be set
   */
  void setGap( int gap );
}