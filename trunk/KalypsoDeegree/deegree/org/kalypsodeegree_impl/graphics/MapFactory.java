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

import org.deegree.graphics.Layer;
import org.deegree.graphics.MapView;
import org.deegree.graphics.Theme;
import org.deegree.graphics.sld.UserStyle;
import org.deegree.model.feature.FeatureCollection;
import org.deegree.model.geometry.GM_Envelope;
import org.opengis.cs.CS_CoordinateSystem;
import org.opengis.gc.GC_GridCoverage;

/**
 * Factory class for creating <tt>MapView</tt>s,<tt>Layer</tt> s and
 * <tt>Theme</tt>s.
 * 
 * <p>
 * ------------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @version $Revision$ $Date$
 */
public class MapFactory
{
  /**
   * creates an empty feature layer with EPSG:4326 as default coordinate
   * reference system. All data that will be added to the layer will be
   * converted to the EPSG:4326 coordinate reference system if no other CRS is
   * set.
   */
  public static synchronized Layer createFeatureLayer( String name ) throws Exception
  {
    return new FeatureLayer_Impl( name );
  }

  /**
   * creates an empty feature layer. All data that will be added to the layer
   * will be converted to the submitted coordinate reference system if no other
   * CRS is set.
   */
  public static synchronized Layer createFeatureLayer( String name, CS_CoordinateSystem crs )
      throws Exception
  {
    return new FeatureLayer_Impl( name, crs );
  }

  /**
   * creates a complete feature layer. If the CRS of the geometries contained
   * within the submitted feature collection are not the same as the submitted
   * CRS all geometries will be converted to the submitted CRS.
   */
  public static synchronized Layer createFeatureLayer( String name, CS_CoordinateSystem crs,
      FeatureCollection fc ) throws Exception
  {
    return new FeatureLayer_Impl( name, crs, fc );
  }

  /**
   * creates a raster layer. The required CRS is contained within the
   * <tt>GC_GridCoverage</tt> object
   */
  public static synchronized Layer createRasterLayer( String name, GC_GridCoverage raster )
      throws Exception
  {
    return new RasterLayer_Impl( name, raster );
  }

  /**
   * creates a theme with a name, a Layer containing the themes data and an
   * array of styles to be known by the <tt>Theme</tt>
   */
  public static synchronized Theme createTheme( String name, Layer layer, UserStyle[] styles )
  {
    return new Theme_Impl( name, layer, styles );
  }

  /**
   * creates a theme with a name, a Layer containing the themes data and an
   * array of styles to be known by the <tt>Theme</tt>
   */
  public static synchronized Theme createTheme( String name, Layer layer )
  {
    return new Theme_Impl( name, layer, new UserStyle[]
    { null } );
  }

  /**
   * creates a <tt>MapView</tt> with a name and a boundingbox describing the
   * area coverd by the <tt>MapView</tt>. The <tt>MapView</tt> uses
   * EPSG:4326 as default coordinate reference system. All data (Themes, Layers)
   * passed to the <tt>MapView</tt> will be converted to this CRS.
   */
  public static synchronized MapView createMapView( String name, GM_Envelope boundingbox )
  {
    return new MapView_Impl( name, boundingbox );
  }

  /**
   * creates a <tt>MapView</tt> with a name, a boundingbox describing the area
   * coverd by the <tt>MapView</tt> and a CRS. All data (Themes, Layers)
   * passed to the <tt>MapView</tt> will be converted to this CRS.
   */
  public static synchronized MapView createMapView( String name, GM_Envelope boundingbox,
      CS_CoordinateSystem crs )
  {
    return new MapView_Impl( name, boundingbox, crs );
  }

  /**
   * creates a <tt>MapView</tt> with a name, a boundingbox describing the area
   * coverd by the <tt>MapView</tt>, a CRS and n <tt>Theme</tt>s.
   */
  public static synchronized MapView createMapView( String name, GM_Envelope boundingbox,
      CS_CoordinateSystem crs, Theme[] themes ) throws Exception
  {
    MapView mv = new MapView_Impl( name, boundingbox, crs );

    for( int i = 0; i < themes.length; i++ )
    {
      mv.addTheme( themes[i] );
    }

    return mv;
  }

  /**
   * creates a <tt>MapView</tt> with a name, a boundingbox describing the area
   * coverd by the <tt>MapView</tt>, a CRS and n <tt>Layer</tt>s. For each
   * submitted <tt>Layer</tt> a Theme having the same name as the Layer will
   * be created.
   */
  public static synchronized MapView createMapView( String name, GM_Envelope boundingbox,
      CS_CoordinateSystem crs, Layer[] layers ) throws Exception
  {
    MapView mv = new MapView_Impl( name, boundingbox, crs );

    for( int i = 0; i < layers.length; i++ )
    {
      Theme theme = createTheme( layers[i].getName(), layers[i] );
      mv.addTheme( theme );
    }

    return mv;
  }
}