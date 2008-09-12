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
package org.kalypsodeegree_impl.graphics.sld;

import org.kalypsodeegree.graphics.sld.LabelPlacement;
import org.kalypsodeegree.graphics.sld.LinePlacement;
import org.kalypsodeegree.graphics.sld.PointPlacement;
import org.kalypsodeegree.xml.Marshallable;
import org.kalypsodeegree_impl.tools.Debug;

/**
 * Used to position a label relative to a point or a line string. For a point, you can specify the anchor point of the
 * label and a linear displacement from the point (so that you can also plot a graphic symbol at the point). For a
 * line-string placement, you can specify a perpendicular offset (so you can draw a stroke on the line). MORE PARAMETERS
 * ARE PROBABLY NEEDED HERE.
 * <p>
 * 
 * @author <a href="mailto:k.lupp@web.de">Katharina Lupp </a>
 * @author <a href="mailto:mschneider@lat-lon.de">Markus Schneider </a>
 * @version $Revision$ $Date$
 */
public class LabelPlacement_Impl implements LabelPlacement, Marshallable
{
  private LinePlacement linePlacement = null;

  private PointPlacement pointPlacement = null;

  /**
   * constructor initializing the class with the <LabelPlacement>
   */
  public LabelPlacement_Impl( PointPlacement pointPlacement )
  {
    setPointPlacement( pointPlacement );
  }

  /**
   * constructor initializing the class with the <LabelPlacement>
   */
  public LabelPlacement_Impl( LinePlacement linePlacement )
  {
    setLinePlacement( linePlacement );
  }

  /**
   * For a PointPlacement, the anchor point of the label and a linear displacement from the point can be specified, to
   * allow a graphic symbol to be plotted directly at the point. This might be useful to label a city, for example.
   * 
   * @return the pointplacement for the label
   */
  public PointPlacement getPointPlacement()
  {
    return pointPlacement;
  }

  /**
   * sets <PointPlacement>
   * 
   * @param pointPlacement
   *          the pointplacement for the label
   */
  public void setPointPlacement( PointPlacement pointPlacement )
  {
    this.pointPlacement = pointPlacement;
    linePlacement = null;
  }

  /**
   * For a LinePlacement, a perpendicular offset can be specified, to allow the line itself to be plotted also. This
   * might be useful for labelling a road or a river, for example.
   * 
   * @return the lineplacement for the label
   */
  public LinePlacement getLinePlacement()
  {
    return linePlacement;
  }

  /**
   * sets <LinePlacement>
   * 
   * @param linePlacement
   *          the lineplacement for the label
   */
  public void setLinePlacement( LinePlacement linePlacement )
  {
    this.linePlacement = linePlacement;
    pointPlacement = null;
  }

  /**
   * exports the content of the Font as XML formated String
   * 
   * @return xml representation of the Font
   */
  public String exportAsXML()
  {
    Debug.debugMethodBegin();

    StringBuffer sb = new StringBuffer( 1000 );
    sb.append( "<LabelPlacement>" );
    if( pointPlacement != null )
    {
      sb.append( ( (Marshallable)pointPlacement ).exportAsXML() );
    }
    else if( linePlacement != null )
    {
      sb.append( ( (Marshallable)linePlacement ).exportAsXML() );
    }
    sb.append( "</LabelPlacement>" );

    Debug.debugMethodEnd();
    return sb.toString();
  }

}