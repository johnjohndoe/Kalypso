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

import java.awt.Graphics;

import org.deegree.graphics.optimizers.Optimizer;
import org.deegree.graphics.transformation.GeoTransform;
import org.deegree.model.geometry.GM_Envelope;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * This interface describes the data modell of the map it self. It is build from
 * themes containing DisplayElements to be rendered. Themes can be added and
 * removed. Existing themes can be re-arragned by changing their order.
 * 
 * <p>
 * ------------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @version $Revision$ $Date$
 */
public interface MapView
{
  /**
   * @clientCardinality 1
   * @link aggregationByValue
   */
  /* #ThemeCollection lnkThemeCollection; */

  /**
   * @link aggregation
   * @clientCardinality 1..*
   */
  /* #Theme lnkTheme; */

  /**
   * returns the name of the
   */
  String getName();

  /**
   * returns the Theme that matches the submitted name
   */
  Theme getTheme( String name );

  /**
   * returns the Theme that matches the submitted index
   */
  Theme getTheme( int index );

  /**
   * returns the Themes in correct order. The first Theme (index == 0) shall be
   * rendered at first (bottom most).
   */
  Theme[] getAllThemes();

  /**
   * Returns the current scale of the MapView.
   */
  double getScale();

  /**
   * Returns the current scale of the MapView.
   */
  double getScale( Graphics g );

  /**
   * adds a theme to the MapView
   */
  void addTheme( Theme theme ) throws Exception;

  /**
   * removes a theme from the MapView
   */
  void removeTheme( Theme theme );

  /**
   * removes the theme that matches the submitted name from the MapView
   */
  void removeTheme( String name );

  /**
   * removes the theme that matches the submitted index from the MapView
   */
  void removeTheme( int index );

  /**
   * removes all themes from the MapView.
   */
  void clear();

  /**
   * swaps the positions of the submitted themes
   */
  void swapThemes( Theme first, Theme second );

  /**
   * move a theme up for one index position (index = oldindex + 1)
   */
  void moveUp( Theme theme );

  /**
   * move a theme down for one index position (index = oldindex - 1)
   */
  void moveDown( Theme theme );

  /**
   * enables or disables a theme that is part of the MapView. A theme that has
   * been disabled won't be rendered and usually doesn't react to events
   * targeted to the MapView, but still is part of the MapView.
   */
  void enableTheme( Theme theme, boolean enable );

  /**
   * returns true if the passed theme is set to be enabled
   */
  boolean isThemeEnabled( Theme theme );

  /**
   * activates a theme. Usually the activated theme is perferred to react to
   * events (this doesn't mean that other themes are not allowed to react to
   * events).
   */
  void activateTheme( Theme theme );

  /**
   * returns true if the passed theme is the one that is set to be activated
   */
  boolean isThemeActivated( Theme theme );

  /**
   * returns the amount of themes within the MapView.
   */
  int getSize();

  /**
   * adds an eventcontroller to the MapView that's reponsible for handling
   * events that targets the map. E.g.: zooming, panning, selecting a feature
   * etc.
   */
  void addEventController( MapEventController obj );

  /**
   * @see MapView#addEventController(MapEventController)
   */
  void removeEventController( MapEventController obj );

  /**
   * A selector is a class that offers methods for selecting and deselecting
   * single DisplayElements or groups of DisplayElements. A selector may offers
   * methods like 'select all DisplayElements within a specified bounding box'
   * or 'select all DisplayElements thats area is larger than 120 km²' etc.
   */
  void addSelector( Selector obj );

  /**
   * @see MapView#addSelector(Selector)
   */
  void removeSelector( Selector obj );

  /**
   * returns the BoundingBox (Envelope) of the MapView. This isn't nessecary the
   * BoundingBox of the data that will be rendered. It's the boundingBox of the
   * the visible area of the map measured in its coordinate reference system.
   */
  GM_Envelope getBoundingBox();

  /**
   * @see MapView#getBoundingBox()
   */
  void setBoundingBox( GM_Envelope boundingbox );

  /**
   * returns the coordinate reference system of the MapView
   */
  CS_CoordinateSystem getCoordinatesSystem();

  /**
   * sets the coordinate reference system of the map;
   */
  void setCoordinateSystem( CS_CoordinateSystem crs ) throws Exception;

  /**
   * renders the map to the submitted graphic context
   */
  void paint( Graphics g ) throws RenderException;

  /**
   * renders the features marked as selected of all themes contained within the
   * MapView
   */
  void paintSelected( Graphics g ) throws RenderException;

  /**
   * renders the features marked as highlighted of all themes contained within
   * the MapView
   */
  void paintHighlighted( Graphics g ) throws RenderException;

  /**
   * A Highlighter is a class that is responsible for managing the highlight
   * capabilities for one or more Themes.
   */
  void addHighlighter( Highlighter highlighter );

  /**
   * @see MapView#addHighlighter(Highlighter)
   */
  void removeHighlighter( Highlighter highlighter );

  /**
   * Returns the <tt>GeoTransform</tt> that is associated to this MapView.
   * 
   * @return the associated <tt>GeoTransform</tt> -instance
   */
  GeoTransform getProjection();

  /**
   * Adds an <tt>Optimizer</tt>.
   * 
   * @param optimizer
   */
  void addOptimizer( Optimizer optimizer );

  /**
   * Returns the <tt>Optimizer</tt>s.
   * 
   * @return
   */
  Optimizer[] getOptimizers();

  /**
   * Sets the <tt>Optimizer<tt>s.
   * @param optimizers
   */
  void setOptimizers( Optimizer[] optimizers );
}