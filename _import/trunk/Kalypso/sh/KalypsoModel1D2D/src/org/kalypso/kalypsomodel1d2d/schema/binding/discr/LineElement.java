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

import java.util.ArrayList;
import java.util.List;

import javax.xml.namespace.QName;

import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.opengis.cs.CS_CoordinateSystem;

@SuppressWarnings("unchecked")
/**
 * Default implementation for {@link ILineElement}
 * 
 * @author Patrice Congo
 * 
 * TODO: the LineElement implements ILineElement and EXTENDS Element2D, while ILineElement DOES NOT, this is most
 * probably a bug as this implies that ILineElement inherits from IElement2D
 */
public class LineElement<CT extends IFE1D2DComplexElement, ET extends IFE1D2DEdge> extends Element2D<CT, ET> implements ILineElement<CT, ET>
{
  public LineElement( final Feature featureToBind, final QName featureQName, final Class<CT> complexElementClass, final Class<ET> edgeClass )
  {
    super( featureToBind, featureQName, complexElementClass, edgeClass );
  }

  /**
   * Creates a continuity line with a specified GML ID. The parent feature respectively its link to the newly created
   * continuity line are specified as parameters.
   * 
   * @param parentFeature
   *            the parent feature
   * @param propQName
   *            the q-name of the property linking the parent feature to the continuity line
   */
  public LineElement( final Feature parentFeature, final QName propQName, final String gmlID, final QName featureQName, final Class<CT> complexElementClass, final Class<ET> edgeClass )
  {
    this( FeatureHelper.createFeatureWithId( featureQName, parentFeature, propQName, gmlID ), featureQName, complexElementClass, edgeClass );
  }

  /**
   * TODO: comment: why is not the super implementation used?
   * 
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.FE1D2D_2DElement#getNodes()
   */
  @Override
  public List<IFE1D2DNode> getNodes( )
  {
    final IFeatureWrapperCollection<ET> edges = super.getEdges();
    final List<IFE1D2DNode> nodes = new ArrayList<IFE1D2DNode>( edges.size() + 1 );
    IFE1D2DNode lastAddedNode = null;

    for( final IFE1D2DEdge<IFE1D2DElement, IFE1D2DNode> edge : edges )
    {
      final IFE1D2DNode<IFE1D2DEdge> node0 = edge.getNode( 0 );
      final IFE1D2DNode<IFE1D2DEdge> node1 = edge.getNode( 1 );

      if( node0.equals( lastAddedNode ) )
      {
        // skip because expected
      }
      else
      {
        if( lastAddedNode == null )
        {
          // first node
          nodes.add( node0 );
          lastAddedNode = node0;
        }
        else
        {
          // bad list not following each other
          throw new RuntimeException( "Continuity line node is bad:" + edges );
        }
      }
      nodes.add( node1 );
      lastAddedNode = node1;
    }
    return nodes;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DContinuityLine#recalculateGeometry()
   */
  public GM_Object recalculateElementGeometry( ) throws GM_Exception
  {
    final List<IFE1D2DNode> nodes = getNodes();

    final int SIZE = nodes.size();

    final GM_Position[] poses = new GM_Position[SIZE];

    if( SIZE <= 1 )
    {
      return null;
    }

    final CS_CoordinateSystem crs = nodes.get( 0 ).getPoint().getCoordinateSystem();

    for( int i = 0; i < poses.length; i++ )
    {
      final GM_Point point = nodes.get( i ).getPoint();
      poses[i] = point.getPosition();
    }

    return GeometryFactory.createGM_Curve( poses, crs );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DElement#getRoughnessClsID()
   */
  public String getRoughnessClsID( )
  {
    return getFeature().getProperty( IFE1D2DElement.PROP_ROUGHNESS_CLS_ID ).toString();
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DElement#getRoughnessCorrectionAxAy()
   */
  public Double getRoughnessCorrectionAxAy( )
  {
    return (Double) getFeature().getProperty( IFE1D2DElement.PROP_ROUGHNESS_CORRECTION_AXAY );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DElement#getRoughnessCorrectionDP()
   */
  public Double getRoughnessCorrectionDP( )
  {
    return (Double) getFeature().getProperty( IFE1D2DElement.PROP_ROUGHNESS_CORRECTION_DP );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DElement#getRoughnessCorrectionKS()
   */
  public Double getRoughnessCorrectionKS( )
  {
    return (Double) getFeature().getProperty( IFE1D2DElement.PROP_ROUGHNESS_CORRECTION_KS );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DElement#getRoughnessStyle()
   */
  public String getRoughnessStyle( )
  {
    return getFeature().getProperty( IFE1D2DElement.PROP_ROUGHNESS_STYLE ).toString();
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DElement#setRoughnessClsID(java.lang.String)
   */
  public void setRoughnessClsID( String value )
  {
    getFeature().setProperty( IFE1D2DElement.PROP_ROUGHNESS_CLS_ID, value );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DElement#setRoughnessCorrectionAxAy(java.lang.String)
   */
  public void setRoughnessCorrectionAxAy( final Double value )
  {
    getFeature().setProperty( IFE1D2DElement.PROP_ROUGHNESS_CORRECTION_AXAY, value );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DElement#setRoughnessCorrectionDP(java.lang.String)
   */
  public void setRoughnessCorrectionDP( final Double value )
  {
    getFeature().setProperty( IFE1D2DElement.PROP_ROUGHNESS_CORRECTION_DP, value );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DElement#setRoughnessCorrectionKS(java.lang.String)
   */
  public void setRoughnessCorrectionKS( final Double value )
  {
    getFeature().setProperty( IFE1D2DElement.PROP_ROUGHNESS_CORRECTION_KS, value );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DElement#setRoughnessStyle(java.lang.String)
   */
  public void setRoughnessStyle( String value )
  {
    getFeature().setProperty( IFE1D2DElement.PROP_ROUGHNESS_STYLE, value );
  }
}
