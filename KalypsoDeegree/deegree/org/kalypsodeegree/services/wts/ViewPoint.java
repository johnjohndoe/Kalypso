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
/*
 * ViewPoint.java
 * 
 * Created on 20. November 2002, 17:27
 */
package org.deegree.services.wts;

import javax.vecmath.Point3d;

/**
 * 
 * @author katharina
 */
public interface ViewPoint
{
  /**
   * defines the position of the observer, the directions he looks and his field
   * of view in radians
   */
  void setObserverPosition( Point3d observerPosition );

  /**
   * returns the position of the observer, the directions he looks and his field
   * of view in radians
   */
  Point3d getObserverPosition();

  /**
   * defines the point the observer looks at
   */
  void setTarget( Point3d targetPoint );

  /**
   * returns the point the observer looks at
   */
  Point3d getTarget();

  /**
   * defines the angle of view of the observer in radians
   */
  void setAoV( double aov );

  /**
   * defines the angle of view of the observer in radians
   */
  double getAoV();

  /**
   * defines vertical direction in radians the observer looks
   */
  void setVDirection( double vdir );

  /**
   * returns vertical direction in radians the observer looks
   */
  double getVDirection();

  /**
   * defines the horizontal direction in radians the observer looks
   */
  void setHDirection( double hdir );

  /**
   * returns horizontal direction in radians the observer looks
   */
  double getHDirection();

  /**
   * 
   * 
   * @return
   */
  /**
   * gets footprint or rather the field of view
   */
  Point3d[] getFootprint();

  /**
   * 
   * 
   * @param height
   * 
   * @return
   */
  /**
   * gets footprint or rather the field of view in reference to the elevation
   * level
   */
  Point3d[] getFootprint( double height );
}