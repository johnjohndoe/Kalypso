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
package org.deegree_impl.graphics;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.deegree.graphics.Layer;
import org.deegree.graphics.LayerEventController;
import org.deegree.model.geometry.GM_Envelope;
import org.deegree_impl.model.cs.ConvenienceCSFactory;
import org.deegree_impl.model.cs.CoordinateSystem;
import org.deegree_impl.model.geometry.GeometryFactory;
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
abstract class Layer_Impl implements Layer
{

  protected CS_CoordinateSystem cs = null;

  protected CoordinateSystem crs = null;

  protected GM_Envelope boundingbox = null;

  protected List eventController = Collections.synchronizedList( new ArrayList() );

  private String name = null;

  /**
   * creates a layer with EPSG:4326 as default coordinate system
   */
  Layer_Impl( String name ) throws Exception
  {
    this.name = name;

    crs = ConvenienceCSFactory.getInstance().getCSByName( "EPSG:4326" );
    cs = org.deegree_impl.model.cs.Adapters.getDefault().export( crs );

  }

  /**
   * Creates a new Layer_Impl object.
   * 
   * @param name
   * @param crs
   * 
   * @throws Exception
   */
  Layer_Impl( String name, CS_CoordinateSystem crs ) throws Exception
  {
    this.name = name;

    cs = crs;
    this.crs = org.deegree_impl.model.cs.Adapters.getDefault().wrap( crs );

    boundingbox = GeometryFactory.createGM_Envelope( 9E99, 9E99, -9E99, -9E99 );
  }

  /**
   * returns the name of the layer
   */
  public String getName()
  {
    return name;
  }

  /**
   * returns the BoundingBox (Envelope) of Layer. This is the BoundingBox of the
   * layers data. The BoundingBox of the View maybe larger or smaler
   */
  public GM_Envelope getBoundingBox()
  {
    return boundingbox;
  }

  /**
   * returns the coordinate reference system of the MapView
   */
  public CS_CoordinateSystem getCoordinatesSystem()
  {
    return org.deegree_impl.model.cs.Adapters.getDefault().export( crs );
  }

  /**
   * adds an eventcontroller to the MapView that's reponsible for handling
   * events that targets the map. E.g.: zooming, panning, selecting a feature
   * etc.
   */
  public void addEventController( LayerEventController obj )
  {
    eventController.add( obj );
    obj.addLayer( this );
  }

  /**
   * @see org.deegree_impl.graphics.Layer_Impl#addEventController(LayerEventController)
   */
  public void removeEventController( LayerEventController obj )
  {
    eventController.remove( obj );
    obj.removeLayer( this );
  }
}