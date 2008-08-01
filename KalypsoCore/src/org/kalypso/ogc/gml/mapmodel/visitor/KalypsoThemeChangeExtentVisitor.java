/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 * 
 *  and
 *  
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 * 
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 * 
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 * 
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 * 
 *  Contact:
 * 
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *   
 *  ---------------------------------------------------------------------------*/
package org.kalypso.ogc.gml.mapmodel.visitor;

import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.mapmodel.IKalypsoThemeVisitor;
import org.kalypsodeegree.model.geometry.GM_Envelope;

/**
 * This class is a vistor for changing the extends of a kalypso theme.<br>
 * It can be used to to something with a requested width, height and extent.
 * 
 * @author Holger Albert
 */
public class KalypsoThemeChangeExtentVisitor implements IKalypsoThemeVisitor
{
  /**
   * This variable stores the requested width.
   */
  private int m_width;

  /**
   * This variable stores the requested height.
   */
  private int m_height;

  /**
   * This variable stores the requested extent.
   */
  private GM_Envelope m_extent;

  /**
   * The constructor.
   * 
   * @param width
   *            The requested width.
   * @param height
   *            The requested height.
   * @param extent
   *            The requested bounding box.
   */
  public KalypsoThemeChangeExtentVisitor( int width, int height, GM_Envelope extent )
  {
    m_width = width;
    m_height = height;
    m_extent = extent;
  }

  /**
   * @see org.kalypso.ogc.gml.mapmodel.IKalypsoThemeVisitor#visit(org.kalypso.ogc.gml.IKalypsoTheme)
   */
  public boolean visit( IKalypsoTheme theme )
  {
    /* Notify the theme of the new extent. */
    theme.setExtent( m_width, m_height, m_extent );

    return true;
  }
}