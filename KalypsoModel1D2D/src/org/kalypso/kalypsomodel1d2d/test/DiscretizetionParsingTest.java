/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
package org.kalypso.kalypsomodel1d2d.test;

import org.kalypso.gml.test.GmlParsingTester;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DComplexElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFENetItem;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.PolyElement;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * @author Felipe Maximino
 */
public class DiscretizetionParsingTest extends GmlParsingTester
{
  private static final double DELTA = 0.01;

  private static final GM_Point point1 = GeometryFactory.createGM_Point( 0.0, 0.0, 0.0, "EPSG:31467" ); //$NON-NLS-1$

  private static final GM_Point point2 = GeometryFactory.createGM_Point( 0.0, 1.0, 1.0, "EPSG:31467" ); //$NON-NLS-1$

  public void testTinyDiscretization( ) throws Exception
  {
    final GMLWorkspace workspace = readGml( "/etc/testdata/data/tinyDiscretization.gml" ); //$NON-NLS-1$
    assertNotNull( workspace );

    final Feature rootFeature = workspace.getRootFeature();
    assertNotNull( rootFeature );

    final IFEDiscretisationModel1d2d discrModel = (IFEDiscretisationModel1d2d)rootFeature.getAdapter( IFEDiscretisationModel1d2d.class );
    assertNotNull( discrModel );

    final IFE1D2DNode[] nodes = discrModel.getNodes();
    assertEquals( 6, nodes.length );
    final IFE1D2DEdge[] edges = discrModel.getEdges();
    assertEquals( 9, edges.length );
    final IFE1D2DElement[] elements = discrModel.getElements();
    assertEquals( 3, elements.length );
    final IFE1D2DComplexElement<IFENetItem>[] complexElements = discrModel.getComplexElements();
    assertEquals( 1, complexElements.length );

    // assert one node
    final IFE1D2DNode node = nodes[0];
    assertNotNull( node );
    assertTrue( node.getPoint().distance( point1 ) < DELTA );

    // getAdjacentNode
    final IFE1D2DNode adjacentNode = node.getAdjacentNodes().get( 0 );
    assertNotNull( adjacentNode );
    assertTrue( adjacentNode.getPoint().distance( point2 ) < DELTA );

    final IFE1D2DEdge edge1 = edges[0];
    assertNotNull( edge1 );
    final IFE1D2DEdge edge2 = edges[1];
    assertNotNull( edge2 );

    // assert the edges of a polyElement
    final PolyElement polyEl1 = (PolyElement)elements[0];
    assertNotNull( polyEl1 );
    assertTrue( polyEl1.containsEdge( edge1 ) );
    assertTrue( polyEl1.containsEdge( edge2 ) );

    // assert complex Element
    final IFE1D2DComplexElement complexEl1 = complexElements[0];
    assertNotNull( complexEl1 );
    assertTrue( complexEl1.getElements()[0].equals( polyEl1 ) );
  }
}
