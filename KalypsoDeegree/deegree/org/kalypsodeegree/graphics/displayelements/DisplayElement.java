/*----------------    FILE HEADER  ------------------------------------------

 This file is part of deegree.
 Copyright (C) 2001 by:
 EXSE, Department of Geography, University of Bonn
 http://www.giub.uni-bonn.de/exse/
 lat/lon Fitzke/Fretter/Poth GbR
 http://www.lat-lon.de   

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

 Andreas Poth
 lat/lon Fitzke/Fretter/Poth GbR
 Meckenheimer Allee 176
 53115 Bonn
 Germany
 E-Mail: poth@lat-lon.de

 Jens Fitzke
 Department of Geography
 University of Bonn
 Meckenheimer Allee 166
 53115 Bonn
 Germany
 E-Mail: jens.fitzke@uni-bonn.de

 ---------------------------------------------------------------------------*/
package org.deegree.graphics.displayelements;

import java.awt.Graphics;

import org.deegree.graphics.transformation.GeoTransform;
import org.deegree.model.feature.Feature;

/**
 * Basic interface of all display elements. A <tt>DisplayElement</tt> is
 * associated to one feature that may have a geometry property or not (usually
 * it has).
 * <p>
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @author <a href="mailto:mschneider@lat-lon.de">Markus Schneider </a>
 * @version $Revision$ $Date$
 */
public interface DisplayElement
{

  /**
   * Returns the associated <tt>Feature</tt>.
   */
  Feature getFeature();

  /**
   * returns the id of thr feature that's associated with the DisplayElement
   */
  String getAssociateFeatureId();

  /**
   * renders the DisplayElement to the submitted graphic context
   */
  void paint( Graphics g, GeoTransform projection );

  /**
   * marks a <tt>DisplayElement</tt> as selected or not
   */
  void setSelected( boolean selected );

  /**
   * returns if the <tt>DisplayElement</tt> is selected or not
   */
  boolean isSelected();

  /**
   * marks the <tt>DisplayElement</tt> as highlighted or not
   */
  void setHighlighted( boolean highlighted );

  /**
   * returns if the <tt>DisplayElement</tt> is highlighted or not.
   */
  boolean isHighlighted();

  /**
   * Returns if the <tt>DisplayElement</tt> should be painted at the current
   * scale or not.
   */
  boolean doesScaleConstraintApply( double scale );
}