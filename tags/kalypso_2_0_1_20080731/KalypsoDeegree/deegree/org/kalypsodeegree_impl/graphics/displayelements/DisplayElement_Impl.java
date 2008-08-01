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
package org.kalypsodeegree_impl.graphics.displayelements;

import java.awt.Graphics;
import java.io.Serializable;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.kalypsodeegree.graphics.displayelements.DisplayElement;
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
abstract class DisplayElement_Impl implements DisplayElement, Serializable
{
  /** Use serialVersionUID for interoperability. */
  private final static long serialVersionUID = 1226236249388451855L;

  private final Feature m_feature;

  private boolean m_highlighted = false;

  private boolean m_selected = false;

  /**
   * Creates a new DisplayElement_Impl object.
   * 
   * @param feature
   */
  DisplayElement_Impl( final Feature feature )
  {
    m_feature = feature;
  }

  /**
   * Returns the associated <tt>Feature</tt>.
   */
  public Feature getFeature( )
  {
    return m_feature;
  }

  /**
   * renders the DisplayElement to the submitted graphic context
   */
  public abstract void paint( Graphics g, GeoTransform projection, final IProgressMonitor monitor ) throws CoreException;

  /**
   * marks a <tt>DisplayElement</tt> as selected or not
   */
  public void setSelected( final boolean selected )
  {
    m_selected = selected;
  }

  /**
   * returns if the <tt>DisplayElement</tt> is selected or not
   */
  public boolean isSelected( )
  {
    return m_selected;
  }

  /**
   * TODO: Remove this, it is awful! Use two different display elements instead...
   * <p>
   * marks the <tt>DisplayElement</tt> as highlighted or not
   */
  public void setHighlighted( final boolean highlighted )
  {
    m_highlighted = highlighted;
  }

  /**
   * returns if the <tt>DisplayElement</tt> is highlighted or not.
   */
  public boolean isHighlighted( )
  {
    return m_highlighted;
  }

  /**
   * Returns if the <tt>DisplayElement</tt> should be painted at the current scale or not.
   */
  public boolean doesScaleConstraintApply( final double scale )
  {
    return true;
  }
}