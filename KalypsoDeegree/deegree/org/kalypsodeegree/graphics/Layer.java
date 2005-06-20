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
package org.kalypsodeegree.graphics;

import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * A Layer is a collection of <tt>Feature</tt> s or rasters building a thematic 'unit' waterways or country borders
 * for example. <tt>Feature</tt> s or raster can be added or removed from the layer. A <tt>Feature</tt> or raster
 * can e changed by a modul of the application using the layer because only references to <tt>Feature</tt> s or
 * rasters are stored within a layer.
 * 
 * <p>
 * ------------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @version $Revision$ $Date$
 */
public interface Layer
{
  /**
   * returns the name of the layer
   */
  String getName();

  /**
   * returns the BoundingBox (Envelope) of Layer. This is the BoundingBox of the layers data. The BoundingBox of the
   * View maybe larger or smaler
   */
  GM_Envelope getBoundingBox();

  /**
   * returns the coordinate reference system of the MapView
   */
  CS_CoordinateSystem getCoordinatesSystem();

  /**
   * sets the coordinate reference system of the MapView. If a new crs is set all geometries of GeometryFeatures will be
   * transformed to the new coordinate reference system.
   */
  void setCoordinatesSystem( CS_CoordinateSystem crs ) throws Exception;

  /**
   * adds an eventcontroller to the MapView that's reponsible for handling events that targets the map. E.g.: zooming,
   * panning, selecting a feature etc.
   */
  void addEventController( LayerEventController obj );

  /**
   * @see Layer#addEventController(LayerEventController)
   */
  void removeEventController( LayerEventController obj );
}