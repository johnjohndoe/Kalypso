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

package org.deegree.graphics;

import org.deegree.model.geometry.GM_Envelope;
import org.deegree.model.geometry.GM_Position;

/**
 * 
 * <p>
 * ------------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @version $Revision$ $Date$
 */
public interface Selector
{

  /**
   * adds a Theme to the Selector that shall be notified if something happens.
   */
  void addTheme( Theme theme );

  /**
   * @see Selector#addTheme(Theme)
   */
  void removeTheme( Theme theme );

  /**
   * selects all features (display elements) that are located within the
   * submitted bounding box.
   * 
   * @return ids of the selected features (display elements)
   */
  String[] select( GM_Envelope boundingbox );

  /**
   * selects all features (display elements) that intersects the submitted
   * point. if a feature is already selected it remains selected.
   * 
   * @return ids of the selected features (display elements)
   */
  String[] select( GM_Position position );

  /**
   * selects all features (display elements) that are located within the circle
   * described by the position and the radius. if a feature is already selected
   * it remains selected.
   * 
   * @return ids of the selected features (display elements)
   */
  String[] select( GM_Position position, double radius );

  /**
   * selects all features (display elements) that are specified by the submitted
   * ids. if a feature is already selected it remains selected.
   * 
   * @return ids of the selected features (display elements)
   */
  String[] select( String[] ids );

  /**
   * invertes the current selection.
   */
  String[] invertSelection();

  /**
   * marks all features as unselected
   */
  void reset();

}