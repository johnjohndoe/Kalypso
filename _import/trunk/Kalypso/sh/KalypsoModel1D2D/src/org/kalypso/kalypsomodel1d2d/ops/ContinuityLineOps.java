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
package org.kalypso.kalypsomodel1d2d.ops;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import javax.xml.namespace.QName;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Platform;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IBoundaryLine1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IEdgeInv;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IElement1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DComplexElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ILineElement;
import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.Point;

/**
 * (static) helper functions for the {@link org.kalypso.kalypsomodel1d2d.schema.binding.FE1D2DContinuityLine} class.
 * 
 * @author Gernot Belger
 * @author Thomas Jung
 */
@SuppressWarnings("unchecked")
public class ContinuityLineOps
{
  private ContinuityLineOps( )
  {
    // do not instantiate
  }

  public static <T extends ILineElement> T boundaryLine1DFromPoint( final QName lineElementQName, final Class<T> adapterTargetClass, final GM_Point point, final IFEDiscretisationModel1d2d model ) throws CoreException
  {
    try
    {
      final IElement1D find1DElement = model.find1DElement( point, 0.1 );
      if( find1DElement == null )
      {
        throw new RuntimeException( "Could not found an 1d element" );
      }
      final IFE1D2DEdge edge1D = find1DElement.getEdge();

      boolean targetAtEdgeEnd = true;

      IFeatureWrapperCollection<IFE1D2DElement> elements = model.getElements();
      IBoundaryLine1D bline1D = elements.addNew( lineElementQName, IBoundaryLine1D.class );
      bline1D.addEdge( edge1D.getGmlID() );
      bline1D.setAtEdgeEnd( targetAtEdgeEnd );

      edge1D.addContainer( bline1D.getGmlID() );

      return (T) bline1D;
    }
    catch( Exception e )
    {
      e.printStackTrace();
      throw new RuntimeException( "Could not create boundary line from curve", e );
    }
  }

  public static <T extends ILineElement> T lineElementFromCurve( final QName lineElementQName, final Class<T> adapterTargetClass, final GM_Curve curve, final IFEDiscretisationModel1d2d model ) throws CoreException
  {
    if( Kalypso1D2DSchemaConstants.WB1D2D_F_BOUNDARY_LINE1D.equals( lineElementQName ) )
    {
      final IStatus status = StatusUtilities.createWarningStatus( "2D Continuity line could not be created, 1D line passed as parameter [1]." );
      throw new CoreException( status );
    }
    final boolean doTrace = Boolean.parseBoolean( Platform.getDebugOption( "KalypsoModel1D2D/debug/ops/continuity/routing" ) );
    // foreach segment of curve:
    try
    {
      if( doTrace )
        System.out.println( "START: ContinuityLine from Curve" );

      final List<IFE1D2DEdge<IFE1D2DElement, IFE1D2DNode>> edgeList = new ArrayList<IFE1D2DEdge<IFE1D2DElement, IFE1D2DNode>>();

      final LineString lineString = (LineString) JTSAdapter.export( curve );
      for( int i = 0; i < lineString.getNumPoints() - 1; i++ )
      {
        if( doTrace )
          System.out.println( "Processing line segment: " + i );

        final Point startPoint = lineString.getPointN( i );
        final Point endPoint = lineString.getPointN( i + 1 );

        // TODO: wenn die punkte ausserhalb liegen, die Verbindung aber das Netzt schneidet, w‰re es
        // eigentlich schˆner, den ersten/letzten Schnittpunkt mit dem Netz als start und Endknoten zu verwenden

        // TODO: eventuell das original segment st¸ckeln, um komische wegf¸hrungen zu vermeiden (d.h. n‰cher an der
        // linie beiben)
        final IFE1D2DNode startNode = NodeOps.findNode( (GM_Point) JTSAdapter.wrap( startPoint ), model );
        final IFE1D2DNode endNode = NodeOps.findNode( (GM_Point) JTSAdapter.wrap( endPoint ), model );
        if( startNode != null && endNode != null )
        {
          if( doTrace )
          {
            System.out.println( "Found start node: " + startNode.getWrappedFeature().getId() );
            System.out.println( "Found end node: " + endNode.getWrappedFeature().getId() );
          }

          final IFE1D2DEdge<IFE1D2DElement, IFE1D2DNode>[] edges = ModelOps.routing( startNode, endNode );
          edgeList.addAll( Arrays.asList( edges ) );
        }
        else
        {
          final IStatus status = StatusUtilities.createWarningStatus( "No node(s) for drawn line found." );
          throw new CoreException( status );
        }
      }

      // TODO: check number of edges
      if( edgeList.isEmpty() )
      {
        // TODO: error message?
        final IStatus status = StatusUtilities.createWarningStatus( "Continuity line could not be created: no edges found." );
        throw new CoreException( status );
      }
      else
      {
        IFE1D2DEdge<IFE1D2DElement, IFE1D2DNode> firstEdge = edgeList.get( 0 );
        if( firstEdge instanceof IEdgeInv )
          firstEdge = ((IEdgeInv) firstEdge).getInverted();
        if( TypeInfo.is1DEdge( firstEdge ) )
        {
          final IStatus status = StatusUtilities.createWarningStatus( "2D Continuity line could not be created, 1D line passed as parameter [2]." );
          throw new CoreException( status );
        }
        else
        {
          // final IFE1D2DContinuityLine contiLine = model.createContinuityLine();
          final IFeatureWrapperCollection<IFE1D2DElement> elements = model.getElements();
          T lineElement = elements.addNew( lineElementQName, adapterTargetClass );
          final IFeatureWrapperCollection edgesList = lineElement.getEdges();// WrappedFeature().getProperty(
          // Kalypso1D2DSchemaConstants.WB1D2D_PROP_DIRECTEDEDGE
          // );
          final String cLineGmlID = lineElement.getGmlID();

          for( final IFE1D2DEdge<IFE1D2DElement, IFE1D2DNode> edge : edgeList )
          {
            edgesList.addRef( edge );
            edge.getContainers().getWrappedList().add( cLineGmlID );
          }
          return lineElement;
        }

      }

    }
    catch( final GM_Exception e )
    {
      final IStatus status = StatusUtilities.statusFromThrowable( e );
      throw new CoreException( status );
    }
  }

  /**
   * To get the middle node of this line. the middle node is the node with the index: <code>
   * Math.ceil( nodes.size()/2.0 )
   * </code>
   * 
   * @param lineElement
   *            the line which middle node is to be computed
   * @return an {@link IFE1D2DNode} representing the middle node of the given line element
   * 
   */
  public static final IFE1D2DNode getMiddleNode( ILineElement lineElement )
  {

    Assert.throwIAEOnNullParam( lineElement, "lineElement" );
    if( lineElement instanceof IBoundaryLine1D )
    {
      // boundary line is based on a single 1d node so its target
      // point is used as middle node
      IFE1D2DEdge edge1D = ((IBoundaryLine1D<IFE1D2DComplexElement, IFE1D2DEdge>) lineElement).getEdges().get( 0 );
      if( ((IBoundaryLine1D) lineElement).isAtEdgeEnd() )
      {
        return edge1D.getNode( 1 );
      }
      else
      {
        return edge1D.getNode( 0 );
      }
    }
    else
    {
      List nodes = lineElement.getNodes();
      int middle = (int) Math.ceil( nodes.size() / 2.0 );
      return (IFE1D2DNode) nodes.get( middle );
    }
  }

}
