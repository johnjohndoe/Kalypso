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
 * Incarnation of a sld:PointPlacement-element. For a PointPlacement, the anchor
 * point of the label and a linear displacement from the point can be specified,
 * to allow a graphic symbol to be plotted directly at the point. This might be
 * useful to label a city, for example. For a LinePlacement, a perpendicular
 * offset can be specified, to allow the line itself to be plotted also. This
 * might be useful for labelling a road or a river, for example.
 * <p>
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @author <a href="mailto:mschneider@lat-lon.de">Markus Schneider </a>
 * @version $Revision$ $Date$
 */
public interface PointPlacement
{

  /**
   * The AnchorPoint element of a PointPlacement gives the location inside of a
   * label to use for anchoring the label to the main-geometry point.
   * <p>
   * </p>
   * The coordinates are given as two floating-point numbers in the AnchorPointX
   * and AnchorPointY elements each with values between 0.0 and 1.0 inclusive.
   * The bounding box of the label to be rendered is considered to be in a
   * coorindate space from 0.0 (lower-left corner) to 1.0 (upper-right corner),
   * and the anchor position is specified as a point in this space. The default
   * point is X=0, Y=0.5, which is at the middle height of the left-hand side of
   * the label. A system may choose different anchor points to de-conflict
   * labels.
   * <p>
   * 
   * @param feature
   *          specifies the <tt>Feature</tt> to be used for evaluation of the
   *          underlying 'sld:ParameterValueType'
   * @return 2 double values: x ([0]) and y ([0])
   * @throws FilterEvaluationException
   *           if the evaluation fails
   */
  double[] getAnchorPoint( Feature feature ) throws FilterEvaluationException;

  /**
   * @see #getAnchorPoint(Feature)
   *      <p>
   * @param anchorPoint
   *          anchorPoint for the PointPlacement
   */
  void setAnchorPoint( double[] anchorPoint );

  /**
   * The Displacement element of a PointPlacement gives the X and Y
   * displacements from the main-geometry point to render a text label.
   * <p>
   * </p>
   * This will often be used to avoid over-plotting a graphic symbol marking a
   * city or some such feature. The displacements are in units of pixels above
   * and to the right of the point. A system may reflect this displacement about
   * the X and/or Y axes to de-conflict labels. The default displacement is X=0,
   * Y=0.
   * <p>
   * 
   * @param feature
   *          specifies the <tt>Feature</tt> to be used for evaluation of the
   *          underlying 'sld:ParameterValueType'
   * @return 2 double values: x ([0]) and y ([0])
   * @throws FilterEvaluationException
   *           if the evaluation fails
   */
  double[] getDisplacement( Feature feature ) throws FilterEvaluationException;

  /**
   * @see #getDisplacement(Feature)
   *      <p>
   * @param displacement
   */
  void setDisplacement( double[] displacement );

  /**
   * The Rotation of a PointPlacement gives the clockwise rotation of the label
   * in degrees from the normal direction for a font (left-to-right for Latin-
   * derived human languages at least).
   * <p>
   * 
   * @param feature
   *          specifies the <tt>Feature</tt> to be used for evaluation of the
   *          underlying 'sld:ParameterValueType'
   * @return double value describing the rotation parameter
   * @throws FilterEvaluationException
   *           if the evaluation fails
   */
  double getRotation( Feature feature ) throws FilterEvaluationException;

  /**
   * @see #getRotation(Feature)
   * @param rotation
   *          the rotation to be set for the PointPlacement
   */
  void setRotation( double rotation );

  /**
   * Returns whether the placement should be optimized or not.
   * <p>
   * 
   * @return true, if it should be optimized
   */
  boolean isAuto();

  /**
   * 
   * <p>
   * 
   * @param auto
   */
  void setAuto( boolean auto );
}