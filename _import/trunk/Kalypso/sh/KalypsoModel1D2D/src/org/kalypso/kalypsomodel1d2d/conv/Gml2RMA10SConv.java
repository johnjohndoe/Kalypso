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
import java.net.URL;
import java.util.Formatter;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.Locale;

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
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * Converts discretisation model to bce2d model
 * 
 * @author Dejan Antanaskovic, <a href="mailto:dejan.antanaskovic@tuhh.de">dejan.antanaskovic@tuhh.de</a>
 */
public class Gml2RMA10SConv
{
  // private final LinkedHashMap<String, String> lines_AR = new LinkedHashMap<String, String>(100000);
  //
  // private final LinkedHashMap<String, String> lines_FP = new LinkedHashMap<String, String>(50000);
  //
  // private final LinkedHashMap<String, String> lines_FE = new LinkedHashMap<String, String>(30000);
  //
  // private final LinkedHashMap<String, String> lines_RK = new LinkedHashMap<String, String>();
  //
  // private final LinkedHashMap<String, String> lines_CS = new LinkedHashMap<String, String>(50000);

  private final LinkedHashMap<String, String> m_nodesIDProvider = new LinkedHashMap<String, String>( 100000 );

  private final LinkedHashMap<String, String> m_elementsIDProvider = new LinkedHashMap<String, String>( 50000 );

  private final LinkedHashMap<String, String> m_complexElementsIDProvider = new LinkedHashMap<String, String>();

  private final LinkedHashMap<String, String> m_edgesIDProvider = new LinkedHashMap<String, String>( 100000 );

  private LinkedHashMap<String, String> m_roughnessIDProvider;

  private double m_offsetX;

  private double m_offsetY;

  private double m_offsetZ;

  private IFEDiscretisationModel1d2d m_discretisationModel1d2d;

  private URL m_outputURL;

  private final GMLWorkspace m_terrainModelWS;

  private int getID( Object object )
  {
    if( object == null )
      return 0;
    if( object instanceof IFeatureWrapper2 )
      return getID( (IFeatureWrapper2) object );
    else
      return 0;
  }

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
    if( !map.containsKey( gmlID ) )
      map.put( gmlID, Integer.toString( map.size() + 1 ) );
    return Integer.parseInt( map.get( gmlID ) );
  }

  public Gml2RMA10SConv( IFEDiscretisationModel1d2d sourceModel, URL rma10sOutputURL, IPositionProvider positionProvider, GMLWorkspace terrainModelWS )
  {
    this.m_discretisationModel1d2d = sourceModel;
    this.m_outputURL = rma10sOutputURL;
    m_terrainModelWS = terrainModelWS;
    GM_Point point = positionProvider.getGMPoint( 0.0, 0.0, 0.0 );
    m_offsetX = -point.getX();
    m_offsetY = -point.getY();
    m_offsetZ = -point.getZ();

  }

  public void toRMA10sModel( final LinkedHashMap<String, String> roughnessIDProvider ) throws IllegalStateException, IOException
  {
    m_roughnessIDProvider = roughnessIDProvider;
    final PrintWriter stream = new PrintWriter( new File( m_outputURL.getPath() ) );
    // final IFeatureWrapperCollection<IFE1D2DComplexElement> complexElements =
    // m_discretisationModel1d2d.getComplexElements();
    final IFeatureWrapperCollection<IFE1D2DElement> elements = m_discretisationModel1d2d.getElements();
    final IFeatureWrapperCollection<IFE1D2DNode> nodes = m_discretisationModel1d2d.getNodes();
    final IFeatureWrapperCollection<IFE1D2DEdge> edges = m_discretisationModel1d2d.getEdges();

    // final Iterator<IFE1D2DComplexElement> complexElementsIterator = complexElements.iterator();
    // while( complexElementsIterator.hasNext() )
    // {
    // IFE1D2DComplexElement complexElement = complexElementsIterator.next();
    // // TODO process it
    // }

    final Iterator<IFE1D2DElement> elementsIterator = elements.iterator();
    final ITerrainModel adapter = (ITerrainModel) m_terrainModelWS.getRootFeature().getAdapter( ITerrainModel.class );
    final IRoughnessPolygonCollection roughnessPolygonCollection = adapter.getRoughnessPolygonCollection();

    // IRoughnessPolygonCollection collection =

    while( elementsIterator.hasNext() )
    {
      final IFE1D2DElement element = elementsIterator.next();
      int roughnessID = 1;
      // System.out.println( element.getGmlID() + " --> " + getID( element ) );
      try
      {
        IRoughnessEstimateSpec roughnessEstimateSpec = roughnessPolygonCollection.getRoughnessEstimateSpec( element.recalculateElementGeometry() );
        IRoughnessCls[] cls = roughnessEstimateSpec.mostSpreadRoughness();
        if(cls.length > 0){
          roughnessID = getID( m_roughnessIDProvider, cls[0].getGmlID());
        }
        
      }
      catch( GM_Exception e )
      {
        // TODO Auto-generated catch block
        e.printStackTrace();
      }

      final StringBuilder builder = new StringBuilder();
      final Formatter formatter = new Formatter( builder );
      formatter.format( Locale.US, "FE%10d%10d%10d%10d", getID( element ), roughnessID, 1, 0 );

      stream.println( builder.toString() );
      // lines_FE.put( element.getGmlID(), builder.toString() );
    }

    final Iterator<IFE1D2DNode> nodesIterator = nodes.iterator();
    while( nodesIterator.hasNext() )
    {
      final IFE1D2DNode node = nodesIterator.next();
      // System.out.println( node.getGmlID() + " --> " + getID( node ) );
      int nodeID = getID( node );
      final GM_Point point = correctPosition( node.getPoint() );
      final StringBuilder builder = new StringBuilder();
      final Formatter formatter = new Formatter( builder );
      formatter.format( Locale.US, "FP%10d%20.7f%20.7f%20.7f", nodeID, point.getX(), point.getY(), point.getZ() );
      stream.println( builder.toString() );
      // lines_FP.put( nodeGmlID, builder.toString() );
      builder.setLength( 0 );
      formatter.format( Locale.US, "CS%10d%10.1f%10.3f%10.3f%10.2f%10.2f%10.2f", nodeID, 10.0, 2.0, 2.0, 0.0, 0.0, 0.0 );
      stream.println( builder.toString() );
      // lines_CS.put( nodeGmlID, builder.toString() );
      
      // TODO: Gernot: put your polynomial stuff here
    }

    int cnt = 1;
    final Iterator<IFE1D2DEdge> edgesIterator = edges.iterator();
    while( edgesIterator.hasNext() )
    {
      final IFE1D2DEdge edge = edgesIterator.next();
      if( edge instanceof IEdgeInv )
        continue;
      int node0ID = getID( edge.getNode( 0 ) );
      int node1ID = getID( edge.getNode( 1 ) );
      int middleNodeID = (edge.getMiddleNode() == null) ? 0 : getID( edge.getMiddleNode() );
      final StringBuilder builder = new StringBuilder();
      final Formatter formatter = new Formatter( builder );
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
        formatter.format( Locale.US, "AR%10d%10d%10d%10d%10d%10d", cnt++, node0ID, node1ID, leftRightID, leftRightID, middleNodeID );
        stream.println( builder.toString() );
        // lines_AR.put( edge.getGmlID(), builder.toString() );
      }
      else if( TypeInfo.is2DEdge( edge ) )
      {
        int leftParent = getID( EdgeOps.getLeftElement( edge ) );
        int rightParent = getID( EdgeOps.getRightElement( edge ) );
        formatter.format( Locale.US, "AR%10d%10d%10d%10d%10d%10d", cnt++, node0ID, node1ID, leftParent, rightParent, middleNodeID );
        stream.println( builder.toString() );
        // lines_AR.put( edge.getGmlID(), builder.toString() );
      }
      else
      {
        // stream.println( "************************************** non 1d/2d edge: " + edge.getGmlID() );
        System.out.println( "non 1d/2d edge: " + edge.getGmlID() );
      }
    }
    stream.flush();
    stream.close();
  }

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
