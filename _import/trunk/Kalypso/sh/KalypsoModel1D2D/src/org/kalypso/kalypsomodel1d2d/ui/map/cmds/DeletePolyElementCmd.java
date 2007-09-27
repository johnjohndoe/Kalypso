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
package org.kalypso.kalypsomodel1d2d.ui.map.cmds;

import java.util.List;

import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DComplexElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IPolyElement;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * Command For deleting element
 * 
 * @author Patrice Congo
 */
public class DeletePolyElementCmd implements IDiscrModel1d2dChangeCommand
{
  private final IFEDiscretisationModel1d2d m_model1d2d;

  private final IPolyElement element2D;

  private GM_Point[] elementPoints;

  private IFE1D2DComplexElement[] complexElements;

  public DeletePolyElementCmd( final IFEDiscretisationModel1d2d model1d2d, final Feature element2DFeature )
  {
    m_model1d2d = model1d2d;
    this.element2D = (IPolyElement) element2DFeature.getAdapter( IPolyElement.class );
  }

  /**
   * @see org.kalypso.commons.command.ICommand#getDescription()
   */
  public String getDescription( )
  {
    return "Delete element 2D";
  }

  /**
   * @see org.kalypso.commons.command.ICommand#isUndoable()
   */
  public boolean isUndoable( )
  {
    return true;
  }

  /**
   * @see org.kalypso.commons.command.ICommand#process()
   */
  public void process( ) throws Exception
  {
    final String elementID = element2D.getGmlID();

    final List<IFE1D2DNode> nodes = element2D.getNodes();

    elementPoints = makeElementPoints( nodes );

    complexElements = getElementComplexElement();

    final IFE1D2DEdge edges[] = makeElementEdges();

    deleteElement( elementID, edges, complexElements );

    deleteEdges( edges, elementID );
  }

  private void deleteEdges( final IFE1D2DEdge[] edges, final String elementID )
  {
    final RemoveEdgeWithoutContainerOrInvCmd remEdgeCmd = new RemoveEdgeWithoutContainerOrInvCmd( m_model1d2d, null );
    for( final IFE1D2DEdge edge : edges )
    {
      try
      {
        remEdgeCmd.setEdgeToDel( edge );
        remEdgeCmd.process();
      }
      catch( final Exception e )
      {
        e.printStackTrace();
      }
    }
  }

  private void deleteElement( final String elementID, final IFE1D2DEdge[] edges, final IFE1D2DComplexElement[] complexElements )
  {
    // delete link to complex elements
    for( final IFE1D2DComplexElement complexElement : complexElements )
    {
      complexElement.getElements().remove( elementID );
    }

    // delete link to edges
    for( final IFE1D2DEdge edge : edges )
    {
      final IFeatureWrapperCollection containers = edge.getContainers();
      boolean isRemoved = containers.remove( elementID );
      // TODO Patrice find a better way to do this, may be try delete again
      if( !isRemoved && containers.contains( elementID ) )
      {
        throw new RuntimeException( "Could not remove element as edge container" );
      }
    }

    // delete element from model
    m_model1d2d.getElements().remove( element2D );

  }

  private IFE1D2DEdge[] makeElementEdges( )
  {
    final IFeatureWrapperCollection edges = element2D.getEdges();
    final IFE1D2DEdge[] edgeArray = (IFE1D2DEdge[]) edges.toArray( new IFE1D2DEdge[] {} );
    return edgeArray;
  }

  private IFE1D2DComplexElement[] getElementComplexElement( )
  {
    final IFeatureWrapperCollection containers = element2D.getContainers();
    final IFE1D2DComplexElement[] cElements = (IFE1D2DComplexElement[]) containers.toArray( new IFE1D2DComplexElement[] {} );
    return cElements;
  }

  private GM_Point[] makeElementPoints( final List<IFE1D2DNode> nodes )
  {
    final int SIZE = nodes.size();
    final GM_Point[] points = new GM_Point[SIZE];
    GM_Position nodePos;
    for( int i = SIZE - 1; i > 0; i-- )
    {
      final GM_Point point = nodes.get( i ).getPoint();
      nodePos = point.getPosition();
      final double[] xyz = nodePos.getAsArray();
      final int dimension = xyz.length;// getDimension();
      if( dimension == 2 )
      {
        points[i] = GeometryFactory.createGM_Point( xyz[0], xyz[1], point.getCoordinateSystem() );
      }
      else if( dimension == 3 )
      {
        points[i] = GeometryFactory.createGM_Point( xyz[0], xyz[1], xyz[2], point.getCoordinateSystem() );
      }
      else
      {
        throw new RuntimeException( "Unsupported gm point dimension:" + dimension + " " + nodePos );
      }
    }
    return points;
  }

  /**
   * @see org.kalypso.commons.command.ICommand#redo()
   */
  public void redo( ) throws Exception
  {

  }

  /**
   * @see org.kalypso.commons.command.ICommand#undo()
   */
  public void undo( ) throws Exception
  {

  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.map.cmds.IDiscrModel1d2dChangeCommand#getChangedFeature()
   */
  public IFeatureWrapper2[] getChangedFeature( )
  {
    return new IFeatureWrapper2[] { element2D };
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.map.cmds.IDiscrModel1d2dChangeCommand#getDiscretisationModel1d2d()
   */
  public IFEDiscretisationModel1d2d getDiscretisationModel1d2d( )
  {
    return m_model1d2d;
  }

}
