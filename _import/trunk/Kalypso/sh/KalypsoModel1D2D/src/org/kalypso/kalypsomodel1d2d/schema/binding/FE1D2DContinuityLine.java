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

import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsosimulationmodel.core.IFeatureWrapperCollection;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.opengis.cs.CS_CoordinateSystem;

@SuppressWarnings("unchecked")
/**
 * Default implementation for {@link IFE1D2DContinuityLine} for
 * binding a feature of the type wb1d2d:ContinuityLine
 * 
 * @author Patrice Congo
 */
public class FE1D2DContinuityLine<    
                              CT extends IFE1D2DComplexElement, 
                              ET extends IFE1D2DEdge> 
                  extends Element2D<CT,ET> 
                  implements IFE1D2DContinuityLine<CT,ET> 
{
//  private static final QName QNAME_PROP_GEOMETRY = new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "geometry" );

  public FE1D2DContinuityLine( 
      final Feature featureToBind, 
      QName featureQName,
      Class<CT> complexElementClass,
      Class<ET> edgeClass)
  {
    super(featureToBind, featureQName, complexElementClass,edgeClass);
  }
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
    this( 
        featureToBind, 
        Kalypso1D2DSchemaConstants.WB1D2D_F_FE1D2DContinuityLine,
        (Class<CT>)IFE1D2DComplexElement.class,
        (Class<ET>)IFE1D2DEdge.class);
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
    this( 
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
//    super( parentFeature, propQName, newFeatureQName );
    this(Util.createFeatureAsProperty(
        parentFeature, 
        propQName,
        newFeatureQName
        ));
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
      org.kalypso.kalypsosimulationmodel.core.Util.createFeatureWithId( 
          Kalypso1D2DSchemaConstants.WB1D2D_F_FE1D2DContinuityLine,
          parentFeature, 
          propQName, 
          gmlID ));
  }

  /**
   * TODO: comment: why is not the super implementation used?
   * 
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.FE1D2D_2DElement#getNodes()
   */
  @Override
  public List<IFE1D2DNode> getNodes( )
  {
    IFeatureWrapperCollection<ET> edges = super.getEdges();
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
          throw new RuntimeException("Continuity line node is bad:"+edges);
        }
      }
      nodes.add( node1 );
      lastAddedNode=node1;
    }
    return nodes;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DContinuityLine#recalculateGeometry()
   */
  public GM_Object recalculateElementGeometry( ) throws GM_Exception
  {
      List<IFE1D2DNode> nodes=getNodes();
      
      final int SIZE=nodes.size();
      
      final GM_Position[] poses = new GM_Position[SIZE];

      if( SIZE <= 1 )
      {
        return null;
      }

      final CS_CoordinateSystem crs = 
        nodes.get(0).getPoint().getCoordinateSystem();

      for( int i = 0; i < poses.length; i++ )
      {
        final GM_Point point = nodes.get( i ).getPoint();
        poses[i] = point.getPosition();
      }
      
      return GeometryFactory.createGM_Curve( poses, crs );
  }
  
//  /**
//   * @see org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DContinuityLine#getLine()
//   */
//  public GM_Curve getGeometry( )
//  {
//    return (GM_Curve) getWrappedFeature().getProperty( QNAME_PROP_GEOMETRY );
//  }

//  /**
//   * @see org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DContinuityLine#setGeometry(org.kalypsodeegree.model.geometry.GM_Curve)
//   */
//  public void setGeometry( final GM_Curve curve )
//  {
//    getWrappedFeature().setProperty( QNAME_PROP_GEOMETRY, curve );
//  }
  
}
