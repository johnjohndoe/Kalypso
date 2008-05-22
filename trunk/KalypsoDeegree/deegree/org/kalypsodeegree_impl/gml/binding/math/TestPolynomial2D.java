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
package org.kalypsodeegree_impl.gml.binding.math;

import junit.framework.TestCase;

/**
 * @author congo
 */
public class TestPolynomial2D extends TestCase
{

  public void testLoadingFromGML( )
  {
//    GMLWorkspace workspace = null;
//
//    workspace = GmlSerializer.createGMLWorkspace( TestWorkspaces.URL_POLYNOMIAL2D, null );
//
//    Feature pol2dFeature = workspace.getRootFeature();
//    assertEquals( KalypsoModelSimulationBaseConsts.SIM_BASE_F_POLYNOMIAL2D, pol2dFeature.getFeatureType().getQName() );
//    Polynomial2D pol2d = new Polynomial2D( pol2dFeature );
//    // test orders
//    assertEquals( 3, pol2d.getDegreeX() );
//    assertEquals( 1, pol2d.getDegreeY() );
//    // test coefs
//    double[] expectedArray = { 0, 1, 2, 3, 4, 5, 6, 7 };
//    double[] actualArray = pol2d.getCoefficients();
//    assertTrue( "Ceofficients unexpected", Arrays.equals( expectedArray, actualArray ) );
  }
}
