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

package org.kalypsodeegree.model.geometry;

/**
 * Defining the different kind of surface interpolation known by the iso geometry modell.
 * <p>
 * -----------------------------------------------------
 * </p>
 * 
 * @author Andreas Poth
 * @version $Revision$ $Date$
 *          <p>
 */

public interface GM_SurfaceInterpolation
{

  public static final int NONE = 0;

  public static final int PLANAR = 1;

  public static final int SPHERICAL = 2;

  public static final int ELLIPTICAL = 3;

  public static final int CONIC = 4;

  public static final int TIN = 5;

  public static final int BILINEAR = 6;

  public static final int BIQUADRATIC = 7;

  public static final int BICUBIC = 8;

  public static final int POLYNOMIALSPLINE = 9;

  public static final int RATIONALSPLINE = 10;

  public static final int TRIANGULATEDSOLINE = 11;

  int getValue( );

  public Object clone( );
}