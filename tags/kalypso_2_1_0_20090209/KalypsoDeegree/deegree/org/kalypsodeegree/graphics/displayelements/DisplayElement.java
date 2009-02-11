/** This file is part of kalypso/deegree.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 * history:
 * 
 * Files in this package are originally taken from deegree and modified here
 * to fit in kalypso. As goals of kalypso differ from that one in deegree
 * interface-compatibility to deegree is wanted but not retained always. 
 * 
 * If you intend to use this software in other ways than in kalypso 
 * (e.g. OGC-web services), you should consider the latest version of deegree,
 * see http://www.deegree.org .
 *
 * all modifications are licensed as deegree, 
 * original copyright:
 *
 * Copyright (C) 2001 by:
 * EXSE, Department of Geography, University of Bonn
 * http://www.giub.uni-bonn.de/exse/
 * lat/lon GmbH
 * http://www.lat-lon.de
 */
package org.kalypsodeegree.graphics.displayelements;

import java.awt.Graphics;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.feature.Feature;

/**
 * Basic interface of all display elements. A <tt>DisplayElement</tt> is associated to one feature that may have a
 * geometry property or not (usually it has).
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
  Feature getFeature( );

  /**
   * renders the DisplayElement to the submitted graphic context
   */
  void paint( Graphics g, GeoTransform projection, IProgressMonitor monitor ) throws CoreException;

  /**
   * marks a <tt>DisplayElement</tt> as selected or not
   */
  void setSelected( boolean selected );

  /**
   * returns if the <tt>DisplayElement</tt> is selected or not
   */
  boolean isSelected( );

  /**
   * marks the <tt>DisplayElement</tt> as highlighted or not
   */
  void setHighlighted( boolean highlighted );

  /**
   * returns if the <tt>DisplayElement</tt> is highlighted or not.
   */
  boolean isHighlighted( );

  /**
   * Returns if the <tt>DisplayElement</tt> should be painted at the current scale or not.
   */
  boolean doesScaleConstraintApply( double scale );
}