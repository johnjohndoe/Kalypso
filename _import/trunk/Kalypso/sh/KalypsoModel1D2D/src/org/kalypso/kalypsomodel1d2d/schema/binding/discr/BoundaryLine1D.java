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
package org.kalypso.kalypsomodel1d2d.schema.binding.discr;

import javax.xml.namespace.QName;

import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsosimulationmodel.core.IFeatureWrapperCollection;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * Default implementation of {@link IBoundaryLine}
 * 
 * @author Patrice Congo
 *
 */

@SuppressWarnings("unchecked")
public class BoundaryLine1D<    
                CT extends IFE1D2DComplexElement, 
                ET extends IFE1D2DEdge> 
                extends BoundaryLine<CT, ET>
                implements IBoundaryLine1D<CT, ET>
{
  public BoundaryLine1D( Feature featureToBind )
  {
    this(
        featureToBind,
        Kalypso1D2DSchemaConstants.WB1D2D_F_BOUNDARY_LINE1D,
        (Class<CT>)IFE1D2DComplexElement.class,
        (Class<ET>)IFE1D2DEdge.class );
  }
  
  /**
   * Creates a new boundary line element binding the specified feature
   * @param featureToBind the feature to bind
   * @param featureQName the required feature q-name
   * @param complexElementClass the target binding class for
   *            containing complex element
   * @param edgeClass the target binding class for edges
   */
  public BoundaryLine1D( 
              Feature featureToBind, 
              QName featureQName, 
              Class<CT> complexElementClass, 
              Class<ET> edgeClass )
  {
    super( featureToBind, featureQName, complexElementClass, edgeClass );
  }

  /**
   * Creates a boundary line element with a specified GML ID.
   * The parent feature respectively its link to the newly 
   * created continuity line are specified as parameters.
   * @param parentFeature the parent feature
   * @param propQName the q-name of the property linking the
   *    parent feature to the continuity line 
   * @param gmlID the gmlID the newly created id will have
   * @param featureQName the q-name denoting the type of the feature
   * @param complexElementClass the target binding class for containing
   *           complex elements
   * @param edgeClass the target binding class for 
   */
  public BoundaryLine1D( 
              Feature parentFeature, 
              QName propQName, 
              String gmlID, 
              QName featureQName, 
              Class<CT> complexElementClass, 
              Class<ET> edgeClass )
  {
    super( 
        parentFeature, 
        propQName, 
        gmlID, 
        featureQName, 
        complexElementClass, 
        edgeClass );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.discr.IBoundaryLine1D#isAtEdgeEnd()
   */
  public boolean isAtEdgeEnd( )
  {
    final Feature feature = getFeature();
    final Object property = feature.getProperty( Kalypso1D2DSchemaConstants.WB1D2D_PROP_IS_AT_EDGE_END );
    if( property == null )
    {
      return false;
    }
    else if( property instanceof Boolean )
    {
      return ((Boolean)property).booleanValue();
    }
    else
    {
      throw new RuntimeException("Boolean expected but found:"+property.getClass());
    }
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.discr.IBoundaryLine1D#setAtEdgeEnd(boolean)
   */
  public void setAtEdgeEnd( boolean isAtEdgeEnd )
  {
    final Feature feature = getFeature();
    feature.setProperty( 
        Kalypso1D2DSchemaConstants.WB1D2D_PROP_IS_AT_EDGE_END, 
        Boolean.valueOf( isAtEdgeEnd ) );
    
  }
  
  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.discr.LineElement#recalculateElementGeometry()
   */
  @Override
  public GM_Object recalculateElementGeometry( ) throws GM_Exception
  {
    IFeatureWrapperCollection<ET> edges2 = getEdges();
    if( edges2.size() == 0 )
    {
      return null;
    }
    final GM_Point targetNode;
    
    ET edge1D = edges2.get( 0 );
    double edgeOrthoVectX;
    double edgeOrthVectY;
    
    if( isAtEdgeEnd() )
    {
      IFE1D2DNode node1 = edge1D.getNode( 1 );
      IFE1D2DNode node0 = edge1D.getNode( 0 );
      targetNode = node1.getPoint();
      GM_Point startPoint = node0.getPoint();
      edgeOrthoVectX = targetNode.getX()-startPoint.getX();
      edgeOrthVectY = -(targetNode.getY()-startPoint.getY());
    }
    else
    {
      IFE1D2DNode node1 = edge1D.getNode( 1 );
      IFE1D2DNode node0 = edge1D.getNode( 0 );
      targetNode = node0.getPoint();
      GM_Point startPoint = node1.getPoint();
      edgeOrthoVectX = targetNode.getX()-startPoint.getX();
      edgeOrthVectY = -(targetNode.getY()-startPoint.getY());
    }
    
    //get
    final double targetX = targetNode.getX();
    final double targetY = targetNode.getY();
    
    double[] poses =  
          {
        targetX+edgeOrthoVectX, targetY+edgeOrthVectY,
        targetX-edgeOrthoVectX, targetY-edgeOrthVectY} ;
    GM_Curve curve = GeometryFactory.createGM_Curve( 
        poses, 2, targetNode.getCoordinateSystem() );
    return curve;
  }
}
