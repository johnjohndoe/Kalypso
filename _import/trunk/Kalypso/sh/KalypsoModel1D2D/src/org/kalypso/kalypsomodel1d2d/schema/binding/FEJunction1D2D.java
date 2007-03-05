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

import java.util.ArrayList;
import java.util.List;

import javax.xml.namespace.QName;

import org.kalypso.kalypsomodel1d2d.geom.ModelGeometryBuilder;
import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsosimulationmodel.core.IFeatureWrapperCollection;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Object;

@SuppressWarnings("unchecked")
/**
 * Default implementation for {@link IFEJunction1D2D} for
 * binding a feature of the type wb1d2d:Junction1D2D
 * 
 * @author Patrice Congo
 */
public class FEJunction1D2D 
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
  public FEJunction1D2D( 
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
  public FEJunction1D2D( 
                Feature parentFeature, 
                QName propQName )
                throws IllegalArgumentException
  {
    super( 
        parentFeature, 
        propQName, 
        Kalypso1D2DSchemaConstants.WB1D2D_F_JUNCTION1D2D );
  }

  /**
   * Creates a new continuity line that bind a feature linked 
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
  public FEJunction1D2D( 
                Feature parentFeature, 
                QName propQName, 
                QName newFeatureQName ) 
                throws IllegalArgumentException
  {
    super( parentFeature, propQName, newFeatureQName );
  }
  
  /**
   * Creates a Junction with a specified GML ID.
   * The parent feature respectively its link to the newly 
   * created continuity line are specified as parameters.
   * @param parentFeature the parent feature
   * @param propQName the qname of the property linking the
   *    parent feature to the continuity line 
   */
  public FEJunction1D2D( 
                        Feature parentFeature,
                        QName propQName,
                        String gmlID)
  {
    this(
      org.kalypso.kalypsosimulationmodel.core.Util.createFeatureWithId( 
          Kalypso1D2DSchemaConstants.WB1D2D_F_JUNCTION1D2D,
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
    return ModelGeometryBuilder.computeContiniutyLineGeometry( this );
  }
  
  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.FE1D2D_2DElement#getNodes()
   */
  
  @Override
  public List<IFE1D2DNode> getNodes( )
  {
    IFeatureWrapperCollection<IFE1D2DEdge> edges = super.getEdges();
    List<IFE1D2DNode> nodes= new ArrayList<IFE1D2DNode>(edges.size()+1);
    IFE1D2DNode lastAddedNode=null;
    
    for(IFE1D2DEdge<IFE1D2DElement, IFE1D2DNode> edge:edges)
    {
      IFE1D2DNode<IFE1D2DEdge> node0=edge.getNode( 0 );
      IFE1D2DNode<IFE1D2DEdge> node1=edge.getNode( 1 );
      
      if(node0.equals( lastAddedNode ))
      {
        //skip because expected
      }
      else
      {
        if(lastAddedNode==null)
        {
          //first node
          nodes.add( node0 );
          lastAddedNode=node0;
        }
        else
        {
          //bad list not following each other
          throw new RuntimeException("Junction line node is bad:"+edges);
        }
      }
      nodes.add( node1 );
      lastAddedNode=node1;
    }
    return nodes;
  }
  
}
