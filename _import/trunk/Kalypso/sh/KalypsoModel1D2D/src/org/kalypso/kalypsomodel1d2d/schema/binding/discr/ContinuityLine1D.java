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

import org.deegree.framework.xml.GeometryUtils;
import org.eclipse.core.runtime.CoreException;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.kalypsodeegree_impl.tools.GeometryUtilities;

public class ContinuityLine1D extends FELine implements IContinuityLine1D
{
  public ContinuityLine1D( Feature featureToBind )
  {
    this( featureToBind, IContinuityLine1D.QNAME );
  }

  public ContinuityLine1D( Feature featureToBind, QName featureQName )
  {
    super( featureToBind, featureQName );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.discr.FELine#createFullNodesList(java.util.List)
   */
  @Override
  public List<IFE1D2DNode> createFullNodesList( final List<IFE1D2DNode> nodes ) throws CoreException
  {
    if( nodes.size() != 1 )
      throw new CoreException( StatusUtilities.createErrorStatus( "Exactly one node is required." ) );
    final IFE1D2DNode continulityLineNode = nodes.get( 0 );
    final FeatureList nodeList = (FeatureList) getFeature().getProperty( IFELine.PROP_NODES );
    nodeList.clear();
    nodeList.add( continulityLineNode.getWrappedFeature().getId() );
    IFE1D2DNode neighbour1 = null;
    IFE1D2DNode neighbour2 = null;
    final IFeatureWrapperCollection<IFeatureWrapper2> containers = continulityLineNode.getContainers();
    for( final IFeatureWrapper2 container : containers )
    {
      if( container instanceof IFE1D2DEdge )
      {
        final List<IFE1D2DNode> edgeNodes = ((IFE1D2DEdge) container).getNodes();
        for( final IFE1D2DNode edgeNode : edgeNodes )
        {
          if( edgeNode.equals( continulityLineNode ) )
            continue;
          else if( neighbour1 == null )
            neighbour1 = edgeNode;
          else if( neighbour2 == null )
            neighbour2 = edgeNode;
        }
      }
    }
    try
    {
      recalculateGeometry( continulityLineNode, neighbour1, neighbour2 );
    }
    catch( GM_Exception e )
    {
      throw new CoreException( StatusUtilities.createErrorStatus( "Cannot create geometry. Reason: " + e.getLocalizedMessage() ) );
    }
    nodeList.invalidate();
    getWrappedFeature().invalidEnvelope();
    return m_nodes;
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.discr.IFENetItem#recalculateElementGeometry()
   */
  public GM_Object recalculateElementGeometry( ) throws GM_Exception
  {
    try
    {
      final ArrayList<IFE1D2DNode> list = new ArrayList<IFE1D2DNode>();
      list.addAll( getNodes() );
      createFullNodesList( list );
    }
    catch( CoreException e )
    {
      e.printStackTrace();
    }
    return (GM_Object) getFeature().getProperty( IFELine.PROP_GEOMETRY );
  }

  private void recalculateGeometry( final IFE1D2DNode node, final IFE1D2DNode neighbour1, final IFE1D2DNode neighbour2 ) throws CoreException, GM_Exception
  {
    if( neighbour1 == null && neighbour2 == null )
      throw new CoreException( StatusUtilities.createErrorStatus( "Continuity line 1D cannot be created; there are no edges containing selected node." ) );
    final GM_Object geometry;
    if( neighbour1 == null )
      geometry = getGeometry( node, neighbour2 );
    else if( neighbour2 == null )
      geometry = getGeometry( node, neighbour1 );
    else
      geometry = getGeometry( node, neighbour1, neighbour2 );
    setGeometry( geometry );
  }

  private GM_Object getGeometry( final IFE1D2DNode node, final IFE1D2DNode neighbour ) throws GM_Exception
  {
    final GM_Point p1 = node.getPoint();
    final GM_Point p2 = neighbour.getPoint();
    final double x1 = p1.getX() - (p2.getY() - p1.getY());
    final double y1 = p1.getY() + (p2.getX() - p1.getX());
    final double x2 = p1.getX() + (p2.getY() - p1.getY());
    final double y2 = p1.getY() - (p2.getX() - p1.getX());
    final GM_Position[] positions = new GM_Position[] { GeometryFactory.createGM_Position( x1, y1 ), GeometryFactory.createGM_Position( x2, y2 ) };
    return GeometryFactory.createGM_Curve( positions, p1.getCoordinateSystem() );
  }

  private GM_Object getGeometry( final IFE1D2DNode node, final IFE1D2DNode neighbour1, final IFE1D2DNode neighbour2 ) throws GM_Exception
  {
    GM_Point pointtocreatefrom = null;
    final GM_Point p1 = node.getPoint();
    final GM_Point p2 = neighbour1.getPoint();
    final GM_Point p3 = neighbour2.getPoint();
    double xcenter = 0.5 * (p3.getX() + p2.getX());
    double ycenter = 0.5 * (p3.getY() + p2.getY());
    final GM_Point centerpoint = GeometryFactory.createGM_Point( GeometryFactory.createGM_Position( xcenter, ycenter ), node.getPoint().getCoordinateSystem() );
    double xoffset = p1.getX() - centerpoint.getX();
    double yoffset = p1.getY() - centerpoint.getY();

    final double length2 = Math.sqrt(Math.pow( xcenter-p2.getX(), 2.0 ) + Math.pow( ycenter-p2.getY(), 2.0 )  );
    final double length3 = Math.sqrt(Math.pow( xcenter-p3.getX(), 2.0 ) + Math.pow( ycenter-p3.getY(), 2.0 )  );
    
    if (length3 >= length2)
      pointtocreatefrom = p2;
    else
      pointtocreatefrom = p3;
      
    double x1 = centerpoint.getX() - (pointtocreatefrom.getY() - centerpoint.getY()) + xoffset;
    double y1 = centerpoint.getY() + (pointtocreatefrom.getX() - centerpoint.getX()) + yoffset;
    double x2 = centerpoint.getX() + (pointtocreatefrom.getY() - centerpoint.getY()) + xoffset;
    double y2 = centerpoint.getY() - (pointtocreatefrom.getX() - centerpoint.getX()) + yoffset;
    
    final GM_Position[] positions = new GM_Position[] { GeometryFactory.createGM_Position( x1, y1 ), GeometryFactory.createGM_Position( x2, y2 ) };
    return GeometryFactory.createGM_Curve( positions, p1.getCoordinateSystem() );
  }
}
