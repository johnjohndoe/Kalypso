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
package org.deegree.services.wts;

import java.util.Calendar;

import javax.media.j3d.Background;
import javax.media.j3d.Group;
import javax.media.j3d.Light;
import javax.media.j3d.Shape3D;

import org.deegree.model.geometry.GM_Envelope;
import org.deegree.model.geometry.GM_Position;

/**
 * the interface defines a scene that will be rendered by a WTS
 * 
 * <p>
 * ----------------------------------------------------------------------
 * </p>
 * 
 * @author Andreas Poth
 * @version $Revision$ $Date$
 *          <p>
 */
public interface WTSScene
{
  /**
   * sets the terrain of the WTS-Scene as Shape3D object containing an
   * appearence
   */
  void setTerrain( Shape3D feature );

  /**
   * sets the terrain of the WTS-Scene as set of several independ Shape3D
   * objects containing an appearence
   */
  void setTerrain( Shape3D[] features );

  /**
   * 
   * 
   * @return
   */
  /**
   * returns the features that constructs the terrain model of the scene
   */
  Shape3D[] getTerrain();

  /**
   * sets the features that shall be rendered.
   */
  void setFeatures( Group[] features );

  /**
   * adds a feature that shall be rendered.
   */
  void addFeature( Group feature );

  /**
   * 
   * 
   * @return
   */
  /**
   * returns the feature of the scene that are not part of the terrain
   */
  Group[] getFeatures();

  /**
   * sets the <tt>Background</tt> object of the scene
   */
  void setBackground( Background background );

  /**
   * sets the background of the scene as <tt>Object</tt>
   */
  void setBackground( Object background );

  /**
   * returns the background object of the scene. this may be a
   * <tt>Background</tt>, a <tt>Shape3D</tt> or <tt>null</tt> if no
   * background is defined.
   */
  Object getBackground();

  /**
   * defines the position of the viewer and the point he looks at.
   */
  void setViewPoint( ViewPoint viewPoint );

  /**
   * gets the position of the viewer, the directions he looks and his field of
   * view in radians
   *  
   */
  ViewPoint getViewPoint();

  /**
   * set the date and the time for determining time depending the light
   * conditions of the scene
   */
  void setDate( Calendar calendar );

  /**
   * returns the date and day time defined for the scene
   */
  Calendar getDate();

  /**
   * sets the atmospheric conditions for the rendering. e.g. if a clear day
   * (maybe summer late morning) is assumed there will be very sharp shadows. if
   * vice versa a cloudy day (let's say autumn late afternoon) with some poor
   * rain and a bit fog is assumed there won't be sharp shadows but some kind of
   * 'gray curtain' over the scene.
   */
  void setAtmosphericConditions( AtmosphericCondition[] conditions );

  /**
   * adds a atmospheric condition to the scene.
   * <p>
   * 
   * @see #setAtmosphericConditions(AtmosphericCondition[])
   */
  void addAtmosphericCondition( AtmosphericCondition conditions );

  /**
   * 
   * 
   * @return
   */
  /**
   * returns the athmospheric conditions of the scene
   */
  AtmosphericCondition[] getAtmosphericConditions();

  /**
   * sets the lights of the scene. this can be ambient, directional and point
   * light.
   */
  void setLights( Light[] lights );

  /**
   * adds a light to the scene. this can be ambient, directional and point
   * light.
   */
  void addLight( Light light );

  /**
   * 
   * 
   * @return
   */
  /**
   * returns the lights of the scene
   */
  Light[] getLights();

  /**
   * returns the envelope od the scene.
   */
  GM_Envelope getEnvelope();

  /**
   * 
   * 
   * @return
   */
  /**
   * returns the four corner coordinates of frame the viewer sees next to him
   * and that contains data
   */
  GM_Position[] getFrontBorderFrame();

  /**
   * 
   * 
   * @return
   */
  /**
   * returns the four corner coordinates of farest frame the viewer sees and
   * that contains data
   */
  GM_Position[] getBackFrame();
}