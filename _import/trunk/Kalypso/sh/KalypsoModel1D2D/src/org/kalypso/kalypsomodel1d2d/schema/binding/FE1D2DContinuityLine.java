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
package org.kalypso.kalypsomodel1d2d.schema.binding;

import javax.xml.namespace.QName;

import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsosimulationmodel.core.IFeatureWrapperCollection;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * Default implementation for {@link IFE1D2DContinuityLine} for
 * binding a feature of the type wb1d2d:ContinuityLine
 * 
 * @author Patrice Congo
 */
public class FE1D2DContinuityLine 
                  extends FE1D2D_2DElement 
                  implements IFE1D2DContinuityLine<
                                    IFE1D2DComplexElement, IFE1D2DEdge>
{
  /**
   * Create a new continuity line binding the provided feature.
   * @param featureToBind the feature to bind. null values are illegal
   * @throws IllegalArgumentException if the passed featureToBind 
   *    parameter is null
   */
  public FE1D2DContinuityLine( 
                final Feature featureToBind )
                throws IllegalArgumentException
  {
    super( featureToBind );
  }

  /**
   * Creates a new continuity line that bind a feature created and linked 
   * as property of the provided parent feature.
   * 
   * @param parentFeature the parent feature of the new to created
   *        continnuity line feature
   *         
   * @throws IllegalArgumentException if the passed parentfeature or the
   * the property QName is null
   */
  public FE1D2DContinuityLine( 
                Feature parentFeature, 
                QName propQName )
                throws IllegalArgumentException
  {
    super( 
        parentFeature, 
        propQName, 
        Kalypso1D2DSchemaConstants.WB1D2D_F_FE1D2DContinuityLine );
  }

  /**
   * Creates a new continuity line that bind a feature created and linked 
   * as property of the provided parent feature.
   * The type of the feature to create is specified by the 
   * newFeatureQName Q-name
   * 
   * @param parentFeature the parent feature of the new to created
   *        continnuity line feature
   *         
   * @throws IllegalArgumentException if the passed parentfeature or the
   * the property QName is null
   */
  public FE1D2DContinuityLine( 
                Feature parentFeature, 
                QName propQName, 
                QName newFeatureQName ) 
                throws IllegalArgumentException
  {
    super( parentFeature, propQName, newFeatureQName );
  }
  
  /**
   * Creates a continuity line with a specified GML ID.
   * The parent feature respectively its link to the newly 
   * created continuity line are specified as parameters.
   * @param parentFeature the parent feature
   * @param propQName the qname of the property linking the
   *    parent feature to the continuity line 
   */
  public FE1D2DContinuityLine( 
                        Feature parentFeature,
                        QName propQName,
                        String gmlID)
  {
    this(
      org.kalypso.kalypsosimulationmodel.core.Util.createNodeById( 
          Kalypso1D2DSchemaConstants.WB1D2D_F_NODE,
          parentFeature, 
          propQName, 
          gmlID ));
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DContinuityLine#recalculateGeometry()
   */
  @Override
  public GM_Object recalculateElementGeometry( ) throws GM_Exception
  {
    final FE1D2DEdge[] edges = getEdgesAsArray();

    if( edges.length == 0 )
      return null;

    final IFE1D2DNode[] nodes = new IFE1D2DNode[edges.length + 1];

    if( edges.length == 1 )
    {
      final IFeatureWrapperCollection<IFE1D2DNode> edgeNodes = edges[0].getNodes();
      nodes[0] = edgeNodes.get( 0 );
      nodes[1] = edgeNodes.get( 1 );
    }

    for( int i = 0; i < edges.length - 1; i++ )
    {
      final FE1D2DEdge edge0 = edges[i];
      final FE1D2DEdge edge1 = edges[i + 1];

      final FE1D2DNode[] edge0Nodes = edge0.getNodesAsArray();
      final FE1D2DNode[] edge1Nodes = edge1.getNodesAsArray();

      final FE1D2DNode edge0node0 = edge0Nodes[0];
      final FE1D2DNode edge0node1 = edge0Nodes[1];
      final FE1D2DNode edge1node0 = edge1Nodes[0];
      final FE1D2DNode edge1node1 = edge1Nodes[1];

      /* Always take the node which does not fit to the next edge */
      if( edge0node1.equals( edge1node0 ) )
        nodes[i] = edge0node0;
      else if( edge0node1.equals( edge1node1 ) )
        nodes[i] = edge0node0;
      else if( edge0node0.equals( edge1node0 ) )
        nodes[i] = edge0node1;
      else if( edge0node0.equals( edge1node1 ) )
        nodes[i] = edge0node1;
      else
        throw new IllegalStateException();

      if( i == edges.length - 2 )
      {
        if( nodes[i].equals( edge0node0 ) )
          nodes[i + 1] = edge0node1;
        else
          nodes[i + 1] = edge0node0;

        if( nodes[i + 1].equals( edge1node0 ) )
          nodes[i + 2] = edge1node1;
        else
          nodes[i + 2] = edge1node0;
      }
    }

    /* Positions from nodes */
    final GM_Position[] poses = new GM_Position[nodes.length];

    if( nodes.length < 2 )
      return null;

    // REMARK: we assume here, that all nodes live in the same coordinate
    // system.
    final CS_CoordinateSystem crs = nodes[0].getPoint().getCoordinateSystem();

    for( int i = 0; i < poses.length; i++ )
    {
      final GM_Point point = nodes[i].getPoint();
      final GM_Position position = point.getPosition();
      poses[i] = GeometryFactory.createGM_Position( position.getX(), position.getY() );
    }

    return GeometryFactory.createGM_Curve( poses, crs );
  }
}
