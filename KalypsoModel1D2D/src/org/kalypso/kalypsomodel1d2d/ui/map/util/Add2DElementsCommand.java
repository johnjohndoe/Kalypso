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
package org.kalypso.kalypsomodel1d2d.ui.map.util;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.kalypso.commons.command.ICommand;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.FE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IPolyElement;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Ring;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree.model.geometry.GM_SurfacePatch;
import org.kalypsodeegree_impl.tools.GeometryUtilities;

/**
 * General command that adds 2D-Elements into the net.<br/>
 * FIXME: clean up this ugly code!
 * 
 * @author Gernot Belger
 */
public class Add2DElementsCommand implements ICommand
{
  private static final double SNAP_DISTANCE = 0.02;

  private static boolean[] NOT_CREATED = new boolean[1];

  private final List<GM_Position> m_setNotInsertedNodes = new ArrayList<GM_Position>();

  private final Map<GM_Position, IFE1D2DNode< ? >> m_nodesNameConversionMap = new HashMap<GM_Position, IFE1D2DNode< ? >>();

  private GM_Envelope m_gmExistingEnvelope;

  private final GMLWorkspace m_discretisationModel;

  private final List<GM_Ring> m_elements;

  public Add2DElementsCommand( final GMLWorkspace discretisationModel, final List<GM_Ring> elements )
  {
    m_discretisationModel = discretisationModel;
    m_elements = elements;
  }

  @Override
  public boolean isUndoable( )
  {
    return false;
  }


  @Override
  public void redo( )
  {
    throw new UnsupportedOperationException();
  }

  @Override
  public void undo( ) throws Exception
  {
    throw new UnsupportedOperationException();
  }

  @Override
  public String getDescription( )
  {
    return null;
  }

  @Override
  public void process( ) throws Exception
  {
    final IFEDiscretisationModel1d2d discModel = (IFEDiscretisationModel1d2d) m_discretisationModel.getRootFeature();

    m_gmExistingEnvelope = discModel.getNodes().getBoundingBox();

    final List<Feature> lListAdded = new ArrayList<Feature>();
    for( final GM_Ring ring : m_elements )
    {
      lListAdded.addAll( createElementsFromRing( discModel, ring ) );
    }
    // Logger.getLogger( TempGrid.class.getName() ).log( Level.INFO, "new elements created: " + lListAdded ); //$NON-NLS-1$

    if( lListAdded.size() > 0 )
    {
      // TODO: fire events for other children: nodes, edges
      final FeatureStructureChangeModellEvent changeEvent = new FeatureStructureChangeModellEvent( m_discretisationModel, discModel, lListAdded.toArray( new Feature[lListAdded.size()] ), FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD );
      m_discretisationModel.fireModellEvent( changeEvent );
      // Logger.getLogger( TempGrid.class.getName() ).log( Level.INFO, "Model event fired: " + changeEvent ); //$NON-NLS-1$
    }
  }

  private List<Feature> createElementsFromRing( final IFEDiscretisationModel1d2d discModel, final GM_Ring ring )
  {
    final List<Feature> lListRes = new ArrayList<Feature>();
    final List<IFE1D2DEdge< ? , ? >> lListEdges = new ArrayList<IFE1D2DEdge< ? , ? >>();
    final List<GM_Point> lListPoses = new ArrayList<GM_Point>();

    checkPosesForCreationOfElement( discModel, ring, lListPoses );

    if( lListPoses.size() < 3 )
      return lListRes;

    if( !lListPoses.get( 0 ).equals( lListPoses.get( lListPoses.size() - 1 ) ) )
    {
      lListPoses.add( lListPoses.get( 0 ) );
    }

    IPolyElement< ? , ? > element2d = discModel.find2DElement( GeometryUtilities.centroidFromRing( ring.getPositions(), ring.getCoordinateSystem() ), SNAP_DISTANCE );

    if( element2d != null )
    {
      return new ArrayList<Feature>();
    }
    GM_Surface< ? extends GM_SurfacePatch> newSurface = null;
    List<IFE1D2DElement> lListFoundPolyElements = null;
    try
    {
      newSurface = org.kalypsodeegree_impl.model.geometry.GeometryFactory.createGM_Surface( ring.getPositions(), null, ring.getCoordinateSystem() );
      lListFoundPolyElements = discModel.getElements().query( newSurface, IFE1D2DElement.PROP_GEOMETRY, false );
    }
    catch( final GM_Exception e )
    {
      // FIXME:
      e.printStackTrace();
    }

    if( lListFoundPolyElements != null && lListFoundPolyElements.size() > 0 )
    {
      for( final IFE1D2DElement< ? , ? > lEle : lListFoundPolyElements )
      {
        if( lEle instanceof IPolyElement )
        {
          final GM_Surface<GM_SurfacePatch> eleGeom = ((IPolyElement< ? , ? >) lEle).getGeometry();
          if( eleGeom.intersects( newSurface ) )
          {
            try
            {
              final GM_Object intersection = eleGeom.intersection( newSurface );
              System.out.println( "intersection: " + intersection ); //$NON-NLS-1$
              if( intersection instanceof GM_Surface )
                return new ArrayList<Feature>();
            }
            catch( final Exception e )
            {
            }
          }
        }
      }
    }

    lListRes.addAll( createNodesAndEdges( discModel, lListEdges, lListPoses ) );

    element2d = discModel.getElements().addNew( IPolyElement.QNAME, IPolyElement.class );
    lListRes.add( element2d );
    for( final IFE1D2DEdge< ? , ? > lEdge : lListEdges )
    {
      // add edge to element and element to edge
      final String elementId = element2d.getId();
      element2d.addEdge( lEdge.getId() );
      lEdge.addContainer( elementId );
    }

    return lListRes;
  }

  private void checkPosesForCreationOfElement( final IFEDiscretisationModel1d2d discModel, final GM_Ring ring, final List<GM_Point> lListPoses )
  {
    for( final GM_Position lPosition : ring.getPositions() )
    {
      final GM_Point nodeLocation = org.kalypsodeegree_impl.model.geometry.GeometryFactory.createGM_Point( lPosition, ring.getCoordinateSystem() );
      final IFE1D2DNode node = discModel.findNode( nodeLocation, SNAP_DISTANCE );
      if( node == null )
      {
        if( m_gmExistingEnvelope != null && m_gmExistingEnvelope.contains( lPosition ) )
        {
          final IPolyElement lFoundElement = discModel.find2DElement( nodeLocation, SNAP_DISTANCE );
          if( lFoundElement != null )
          {
            // do not insert nodes that are placed on existing model(overlapped elements)
            m_setNotInsertedNodes.add( lPosition );
            Logger.getLogger( TempGrid.class.getName() ).log( Level.WARNING, "removed node ", nodeLocation.toString() ); //$NON-NLS-1$
          }
          else
          {
            lListPoses.add( nodeLocation );
          }
        }
        else
        {
          lListPoses.add( nodeLocation );
        }
      }
      else
      {
        lListPoses.add( nodeLocation );
        m_nodesNameConversionMap.put( lPosition, node );
      }
    }
  }

  private List<Feature> createNodesAndEdges( final IFEDiscretisationModel1d2d discModel, final List<IFE1D2DEdge< ? , ? >> lListEdges, final List<GM_Point> lListPoses )
  {
    final List<Feature> lListRes = new ArrayList<Feature>();
    final Map<GM_Position, IFE1D2DNode< ? >> lNodesNameConversionMap = new HashMap<GM_Position, IFE1D2DNode< ? >>();
    /* Create nodes */
    final List<IFE1D2DNode< ? >> nodes = new ArrayList<IFE1D2DNode< ? >>();
    for( int i = 0; i < lListPoses.size() - 1; i++ )
    {
      final GM_Point lPoint = lListPoses.get( i );

      IFE1D2DNode< ? > actNode = m_nodesNameConversionMap.get( lPoint.getPosition() );
      if( actNode == null )
      {
        actNode = lNodesNameConversionMap.get( lPoint.getPosition() );
      }
      if( actNode == null )
      {
        actNode = discModel.createNode( lPoint, -1, NOT_CREATED );
        if( actNode == null )
          return new ArrayList<Feature>();

        lNodesNameConversionMap.put( lPoint.getPosition(), actNode );
        lListRes.add( actNode );
      }

      nodes.add( actNode );
    }

    /* Create edges */
    for( int i = 0; i < nodes.size(); i++ )
    {
      final IFE1D2DNode< ? > node1 = nodes.get( i );
      final IFE1D2DNode< ? > node2 = nodes.get( (i + 1) % nodes.size() );

      final IFE1D2DEdge< ? , ? > existingEdge = discModel.findEdge( node1, node2 );
      final IFE1D2DEdge< ? , ? > edge;
      if( existingEdge == null )
      {
        edge = FE1D2DEdge.createFromModel( discModel, node1, node2 );
        lListRes.add( edge );
      }
      else
        edge = existingEdge;
      lListEdges.add( edge );
    }

    return lListRes;
  }
}
