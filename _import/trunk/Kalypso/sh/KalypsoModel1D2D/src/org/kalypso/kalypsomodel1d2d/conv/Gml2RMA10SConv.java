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
package org.kalypso.kalypsomodel1d2d.conv;

import java.io.File;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.Formatter;
import java.util.LinkedHashMap;
import java.util.Locale;

import org.apache.commons.io.IOUtils;
import org.kalypso.kalypsomodel1d2d.ops.EdgeOps;
import org.kalypso.kalypsomodel1d2d.ops.TypeInfo;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IEdgeInv;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IElement1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DComplexElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsosimulationmodel.core.IFeatureWrapperCollection;
import org.kalypso.kalypsosimulationmodel.core.roughness.IRoughnessCls;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRoughnessEstimateSpec;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRoughnessPolygonCollection;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.ITerrainModel;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * Converts discretisation model to bce2d model
 * 
 * @author Dejan Antanaskovic, <a href="mailto:dejan.antanaskovic@tuhh.de">dejan.antanaskovic@tuhh.de</a>
 */
public class Gml2RMA10SConv
{
  private final LinkedHashMap<String, String> m_nodesIDProvider = new LinkedHashMap<String, String>( 100000 );

  private final LinkedHashMap<String, String> m_elementsIDProvider = new LinkedHashMap<String, String>( 50000 );

  private final LinkedHashMap<String, String> m_complexElementsIDProvider = new LinkedHashMap<String, String>();

  private final LinkedHashMap<String, String> m_edgesIDProvider = new LinkedHashMap<String, String>( 100000 );

  private double m_offsetX;

  private double m_offsetY;

  private double m_offsetZ;

  private IFEDiscretisationModel1d2d m_discretisationModel1d2d;

  private File m_outputFile;

  private final GMLWorkspace m_terrainModelWS;

  private int getID( IFeatureWrapper2 i1d2dObject )
  {
    if( i1d2dObject == null )
      return 0;
    final String id = i1d2dObject.getGmlID();
    if( i1d2dObject instanceof IFE1D2DNode )
      return getID( m_nodesIDProvider, id );
    else if( i1d2dObject instanceof IFE1D2DEdge )
      return getID( m_edgesIDProvider, id );
    else if( i1d2dObject instanceof IFE1D2DElement )
      return getID( m_elementsIDProvider, id );
    else if( i1d2dObject instanceof IFE1D2DComplexElement )
      return getID( m_complexElementsIDProvider, id );
    else
      return 0;
  }

  private int getID( final LinkedHashMap<String, String> map, final String gmlID )
  {
    // TODO: why the String to Integer and back conversion???
    // at least, comment this!
    if( !map.containsKey( gmlID ) )
      map.put( gmlID, Integer.toString( map.size() + 1 ) );
    return Integer.parseInt( map.get( gmlID ) );
  }

  public Gml2RMA10SConv( IFEDiscretisationModel1d2d sourceModel, final File rma10sOutputFile, IPositionProvider positionProvider, GMLWorkspace terrainModelWS )
  {
    m_discretisationModel1d2d = sourceModel;
    m_outputFile = rma10sOutputFile;
    m_terrainModelWS = terrainModelWS;
    GM_Point point = positionProvider.getGMPoint( 0.0, 0.0, 0.0 );
    m_offsetX = -point.getX();
    m_offsetY = -point.getY();
    m_offsetZ = -point.getZ();
  }

  public void toRMA10sModel( final LinkedHashMap<String, String> roughnessIDProvider ) throws IllegalStateException, IOException
  {
    PrintWriter stream = null;
    try
    {
      stream = new PrintWriter( m_outputFile );
      writeRMA10sModel( roughnessIDProvider, stream );
      stream.close();
    }
    finally
    {
      IOUtils.closeQuietly( stream );
    }
  }

  private void writeRMA10sModel( final LinkedHashMap<String, String> roughnessIDProvider, final PrintWriter stream )
  {
    final IFeatureWrapperCollection<IFE1D2DElement> elements = m_discretisationModel1d2d.getElements();
    final IFeatureWrapperCollection<IFE1D2DNode> nodes = m_discretisationModel1d2d.getNodes();
    final IFeatureWrapperCollection<IFE1D2DEdge> edges = m_discretisationModel1d2d.getEdges();

    final ITerrainModel adapter = (ITerrainModel) m_terrainModelWS.getRootFeature().getAdapter( ITerrainModel.class );
    final IRoughnessPolygonCollection roughnessPolygonCollection = adapter.getRoughnessPolygonCollection();

    final Formatter formatter = new Formatter( stream );

    writeElements( formatter, roughnessIDProvider, elements, roughnessPolygonCollection );
    writeNodes( formatter, nodes );
    writeEdges( formatter, edges );
  }

  private void writeEdges( final Formatter formatter, final IFeatureWrapperCollection<IFE1D2DEdge> edges )
  {
    // TODO: according to Nico, also middle-nodes should be written (generated automatically as the middle of the edge)
    
    int cnt = 1;
    for( final IFE1D2DEdge edge : edges )
    {
      if( edge instanceof IEdgeInv )
        continue;
      int node0ID = getID( edge.getNode( 0 ) );
      int node1ID = getID( edge.getNode( 1 ) );
      int middleNodeID = (edge.getMiddleNode() == null) ? 0 : getID( edge.getMiddleNode() );
      /* Directly format into the string, this is quickest! */
      // System.out.println( edge.getGmlID() + " --> " + getID( edge ) );
      if( TypeInfo.is1DEdge( edge ) )
      {
        int leftRightID = 0;
        if( edge.getContainers().size() > 0 )
        {
          Object object = edge.getContainers().get( 0 );

          if( object instanceof IElement1D )
            leftRightID = getID( ((IElement1D) object) );
        }
        formatter.format( Locale.US, "AR%10d%10d%10d%10d%10d%10d%n", cnt++, node0ID, node1ID, leftRightID, leftRightID, middleNodeID );
        // lines_AR.put( edge.getGmlID(), builder.toString() );
      }
      else if( TypeInfo.is2DEdge( edge ) )
      {
        int leftParent = getID( EdgeOps.getLeftElement( edge ) );
        int rightParent = getID( EdgeOps.getRightElement( edge ) );
        formatter.format( Locale.US, "AR%10d%10d%10d%10d%10d%10d%n", cnt++, node0ID, node1ID, leftParent, rightParent, middleNodeID );
        // lines_AR.put( edge.getGmlID(), builder.toString() );
      }
      else
      {
        // stream.println( "************************************** non 1d/2d edge: " + edge.getGmlID() );
        System.out.println( "non 1d/2d edge: " + edge.getGmlID() );
      }
    }
  }

  private void writeNodes( final Formatter formatter, final IFeatureWrapperCollection<IFE1D2DNode> nodes )
  {
    for( final IFE1D2DNode node : nodes )
    {
      // System.out.println( node.getGmlID() + " --> " + getID( node ) );
      final int nodeID = getID( node );
      final GM_Point point = correctPosition( node.getPoint() );
      formatter.format( Locale.US, "FP%10d%20.7f%20.7f%20.7f%n", nodeID, point.getX(), point.getY(), point.getZ() );

      // lines_FP.put( nodeGmlID, builder.toString() );
      formatter.format( Locale.US, "CS%10d%10.1f%10.3f%10.3f%10.2f%10.2f%10.2f%n", nodeID, 10.0, 2.0, 2.0, 0.0, 0.0, 0.0 );
      // lines_CS.put( nodeGmlID, builder.toString() );

      // TODO: Gernot: put your polynomial stuff here
    }
  }

  private void writeElements( final Formatter formatter, final LinkedHashMap<String, String> roughnessIDProvider, final IFeatureWrapperCollection<IFE1D2DElement> elements, final IRoughnessPolygonCollection roughnessPolygonCollection )
  {
    for( final IFE1D2DElement element : elements )
    {
      int roughnessID = 1;
      // System.out.println( element.getGmlID() + " --> " + getID( element ) );
      try
      {
        /*
         * TODO: according to Nico, if we have an Element1D with Polynomial flow-relations, the special roughness-clas
         * '889' shpould be used.
         */

        final IRoughnessEstimateSpec roughnessEstimateSpec = roughnessPolygonCollection.getRoughnessEstimateSpec( element.recalculateElementGeometry() );
        if( roughnessEstimateSpec == null )
        {
          // TODO: check what to do here...
        }
        else
        {
          IRoughnessCls[] cls = roughnessEstimateSpec.mostSpreadRoughness();
          if( cls.length > 0 )
          {
            roughnessID = getID( roughnessIDProvider, cls[0].getGmlID() );
          }
        }
      }
      catch( final GM_Exception e )
      {
        // TODO Auto-generated catch block
        e.printStackTrace();
      }

      formatter.format( Locale.US, "FE%10d%10d%10d%10d%n", getID( element ), roughnessID, 1, 0 );
    }
  }

  
  // TODO: do not move the points any more, Nico sais it is ok!!
  private GM_Point correctPosition( GM_Point point )
  {
    double z = 0.0;
    try
    {
      z = point.getZ();
    }
    catch( ArrayIndexOutOfBoundsException e )
    {
      // System.out.println( "No Z value!" );
    }
    return GeometryFactory.createGM_Point( point.getX() + m_offsetX, point.getY() + m_offsetY, z + m_offsetZ, point.getCoordinateSystem() );
  }

}
