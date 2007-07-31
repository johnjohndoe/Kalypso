/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 * 
 *  and
 *  
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 * 
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 * 
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 * 
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 * 
 *  Contact:
 * 
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *   
 *  ---------------------------------------------------------------------------*/
package test.org.kalypso.kalypsomodel1d2d;

import junit.framework.TestCase;

import org.kalypso.kalypsomodel1d2d.ops.EdgeOps;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * @author antanas
 * 
 */
public class TestOrientation extends TestCase
{
  public void testOrientation( )
  {
    try
    {
      double[] ccwExterior = { 0, 0, 1, 0, 0, 1, 0, 0 };
      double[] cwExterior = { 0, 0, 0, 1, 1, 0, 0, 0 };
      double[] ccwExterior1 = { 0, 0, 1, 0.5, 0, 1, 0, 0 };
      double[] cwExterior1 = { 0, 0, 0.3, 1, 1, 0, 0, 0 };
      GM_Surface ccwSurface = GeometryFactory.createGM_Surface( ccwExterior, new double[0][], 2, TestWorkspaces.getGaussKrueger() );
      GM_Surface cwSurface = GeometryFactory.createGM_Surface( cwExterior, new double[0][], 2, TestWorkspaces.getGaussKrueger() );
      GM_Surface ccwSurface1 = GeometryFactory.createGM_Surface( ccwExterior, new double[0][], 2, TestWorkspaces.getGaussKrueger() );
      GM_Surface cwSurface1 = GeometryFactory.createGM_Surface( cwExterior, new double[0][], 2, TestWorkspaces.getGaussKrueger() );

      System.out.println( "ccwO" + EdgeOps.getOrientation( ccwSurface ) + " cwO" + EdgeOps.getOrientation( cwSurface ) );
      assertEquals( "CW", EdgeOps.ORIENTATION_RIGHT, EdgeOps.getOrientation( cwSurface ) );
      assertEquals( "CW", EdgeOps.ORIENTATION_RIGHT, EdgeOps.getOrientation( cwSurface1 ) );

      assertEquals( "CCW", EdgeOps.ORIENTATION_LEFT, EdgeOps.getOrientation( ccwSurface ) );
      assertEquals( "CCW", EdgeOps.ORIENTATION_LEFT, EdgeOps.getOrientation( ccwSurface1 ) );

      System.out.println( "ccwO" + ccwSurface.getOrientation() + " cwO" + cwSurface.getOrientation() );
      System.out.println( "ccwS" + ccwSurface.getArea() + " cwS" + cwSurface.getArea() );

    }
    catch( Exception e )
    {
      e.printStackTrace();
    }
  }
}
