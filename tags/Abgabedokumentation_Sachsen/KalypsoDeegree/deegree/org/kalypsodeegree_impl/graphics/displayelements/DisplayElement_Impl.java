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
package org.kalypsodeegree_impl.graphics.displayelements;

import java.awt.Graphics;
import java.io.Serializable;

import org.kalypsodeegree.graphics.displayelements.DisplayElement;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.feature.Feature;

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
abstract class DisplayElement_Impl implements DisplayElement, Serializable
{

  /** Use serialVersionUID for interoperability. */
  private final static long serialVersionUID = 1226236249388451855L;

  protected Feature feature = null;

  private boolean highlighted = false;

  private boolean selected = false;

  /**
   * Creates a new DisplayElement_Impl object.
   */
  DisplayElement_Impl()
  {}

  /**
   * Creates a new DisplayElement_Impl object.
   * 
   * @param feature
   */
  DisplayElement_Impl( Feature feature )
  {
    this.feature = feature;
  }

  /**
   * Returns the associated <tt>Feature</tt>.
   */
  public Feature getFeature()
  {
    return feature;
  }

  /**
   * returns the id of the feature that's associated with the DisplayElement
   */
  public String getAssociateFeatureId()
  {
    return feature.getId();
  }

  /**
   * renders the DisplayElement to the submitted graphic context
   */
  public abstract void paint( Graphics g, GeoTransform projection );

  /**
   * marks a <tt>DisplayElement</tt> as selected or not
   */
  public void setSelected( boolean selected )
  {
    this.selected = selected;
  }

  /**
   * returns if the <tt>DisplayElement</tt> is selected or not
   */
  public boolean isSelected()
  {
    return selected;
  }

  /**
   * marks the <tt>DisplayElement</tt> as highlighted or not
   */
  public void setHighlighted( boolean highlighted )
  {
    this.highlighted = highlighted;
  }

  /**
   * returns if the <tt>DisplayElement</tt> is highlighted or not.
   */
  public boolean isHighlighted()
  {
    return highlighted;
  }

  /**
   * Returns if the <tt>DisplayElement</tt> should be painted at the current
   * scale or not.
   */
  public boolean doesScaleConstraintApply( double scale )
  {
    return true;
  }
}