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
package org.deegree.graphics;

import org.deegree.model.geometry.GM_Envelope;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * A Layer is a collection of <tt>Feature</tt> s or rasters building a
 * thematic 'unit' waterways or country borders for example. <tt>Feature</tt>
 * s or raster can be added or removed from the layer. A <tt>Feature</tt> or
 * raster can e changed by a modul of the application using the layer because
 * only references to <tt>Feature</tt> s or rasters are stored within a layer.
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
   * returns the BoundingBox (Envelope) of Layer. This is the BoundingBox of the
   * layers data. The BoundingBox of the View maybe larger or smaler
   */
  GM_Envelope getBoundingBox();

  /**
   * returns the coordinate reference system of the MapView
   */
  CS_CoordinateSystem getCoordinatesSystem();

  /**
   * sets the coordinate reference system of the MapView. If a new crs is set
   * all geometries of GeometryFeatures will be transformed to the new
   * coordinate reference system.
   */
  void setCoordinatesSystem( CS_CoordinateSystem crs ) throws Exception;

  /**
   * adds an eventcontroller to the MapView that's reponsible for handling
   * events that targets the map. E.g.: zooming, panning, selecting a feature
   * etc.
   */
  void addEventController( LayerEventController obj );

  /**
   * @see Layer#addEventController(LayerEventController)
   */
  void removeEventController( LayerEventController obj );
}