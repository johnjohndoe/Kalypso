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
package org.kalypsodeegree_impl.model.geometry;

import java.io.Serializable;

import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_SurfaceInterpolation;

/**
 * default implementation of the GM_SurfaceInterpolation interface from the package jago.model.
 * ------------------------------------------------------------
 * 
 * @version 11.6.2001
 * @author Andreas Poth
 */
public class GM_SurfaceInterpolation_Impl implements GM_SurfaceInterpolation, Serializable
{
  /** Use serialVersionUID for interoperability. */
  private final static long serialVersionUID = -3728721225837686088L;

  private final int m_surfaceInterpolation = GM_SurfaceInterpolation.NONE;

  /**
   * Creates a new GM_SurfaceInterpolation_Impl object.
   */
  public GM_SurfaceInterpolation_Impl( )
  {
  }

  /**
   * Creates a new GM_SurfaceInterpolation_Impl object.
   * 
   * @param surfaceInterpolation
   * @throws GM_Exception
   */
  public GM_SurfaceInterpolation_Impl( final int surfaceInterpolation ) throws GM_Exception
  {
    if( (surfaceInterpolation > GM_SurfaceInterpolation.TRIANGULATEDSOLINE) || (surfaceInterpolation < GM_SurfaceInterpolation.NONE) )
    {
      throw new GM_Exception( "invalid surface interpolation" );
    }
  }

  public int getValue( )
  {
    return m_surfaceInterpolation;
  }

  /**
   * returns a deep copy of the geometry
   */
  @Override
  public Object clone( )
  {
    try
    {
      return new GM_SurfaceInterpolation_Impl( getValue() );
    }
    catch( final Exception ex )
    {
      System.out.println( "GM_SurfaceInterpolation_Impl.clone: " + ex );
    }

    throw (new IllegalStateException());
  }

  /**
   * checks if this surface is completly equal to the submitted geometry.
   */
  @Override
  public boolean equals( final Object other )
  {
    return (other instanceof GM_SurfaceInterpolation_Impl) && (((GM_SurfaceInterpolation) other).getValue() == m_surfaceInterpolation);
  }
}