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

package org.kalypsodeegree.gml;

/**
 * 
 * 
 * <p>
 * ----------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @version 07.02.2001
 *          <p>
 */
public interface GMLLineString extends GMLGeometry
{
  /*****************************************************************************
   * @link aggregationByValue
   * @clientCardinality 0..
   ****************************************************************************/
  /* #GMLCoord lnkGMLCoord; */

  /**
   * @link aggregationByValue
   * @clientCardinality 0..1
   */
  /* #GMLCoordinates lnkGMLCoordinates; */

  /**
   * returns the coordinates (location) of the LineString as as array of
   * GMLCoord
   */
  public GMLCoord[] getCoords();

  /**
   * @see #getCoords()
   */
  public void setCoords( GMLCoord[] coord ) throws GMLException;

  /**
   * @see #getCoords()
   */
  public void appendCoord( GMLCoord coord ) throws GMLException;

  /**
   * returns the coordinate (location) of the point as GMLCoordinates
   */
  public GMLCoordinates getCoordinates();

  /**
   * @see #getCoords()
   */
  public void setCoordinates( GMLCoordinates coordinates );
}
/*
 * Changes to this class. What the people haven been up to:
 * 
 * $Log$
 * Revision 1.5  2005/03/08 11:01:10  doemming
 * *** empty log message ***
 *
 * Revision 1.4  2005/01/18 12:50:41  doemming
 * *** empty log message ***
 *
 * Revision 1.3  2004/10/07 14:09:01  doemming
 * *** empty log message ***
 *
 * Revision 1.1  2004/09/02 23:56:51  doemming
 * *** empty log message ***
 * Revision 1.3 2004/08/31 12:45:01 doemming ***
 * empty log message *** Revision 1.2 2004/04/27 15:40:15 poth no message
 * 
 * Revision 1.1.1.1 2002/09/25 16:01:46 poth no message
 * 
 * Revision 1.2 2002/08/19 15:59:20 ap no message
 * 
 * Revision 1.1 2002/04/04 16:17:15 ap no message
 * 
 * Revision 1.3 2001/11/23 10:40:53 axel as: CVS change-log comment added
 * 
 *  
 */
