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
import java.math.BigDecimal;
import java.util.Collection;
import java.util.Formatter;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;

import org.apache.commons.io.IOUtils;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.kalypso.jts.JTSUtilities;
import org.kalypso.kalypsomodel1d2d.conv.results.RestartEater;
import org.kalypso.kalypsomodel1d2d.ops.CalUnitOps;
import org.kalypso.kalypsomodel1d2d.ops.EdgeOps;
import org.kalypso.kalypsomodel1d2d.ops.TypeInfo;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.DiscretisationModelUtils;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IBoundaryLine;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IEdgeInv;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IElement1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IElement2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DComplexElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IPolyElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.FlowRelationUtilitites;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IKingFlowRelation;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.ITeschkeFlowRelation;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IWeirFlowRelation;
import org.kalypso.kalypsomodel1d2d.schema.binding.results.GMLNodeResult;
import org.kalypso.kalypsomodel1d2d.schema.binding.results.INodeResult;
import org.kalypso.kalypsomodel1d2d.sim.RMA10Calculation;
import org.kalypso.kalypsosimulationmodel.core.IFeatureWrapperCollection;
import org.kalypso.kalypsosimulationmodel.core.Util;
import org.kalypso.kalypsosimulationmodel.core.flowrel.IFlowRelationship;
import org.kalypso.kalypsosimulationmodel.core.flowrel.IFlowRelationshipModel;
import org.kalypso.kalypsosimulationmodel.core.roughness.IRoughnessCls;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRoughnessEstimateSpec;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRoughnessPolygonCollection;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.ITerrainModel;
import org.kalypso.model.wspm.tuhh.schema.schemata.IWspmTuhhQIntervallConstants;
import org.kalypso.simulation.core.SimulationException;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.gml.binding.math.IPolynomial1D;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.Point;

/**
 * Converts discretisation model to bce2d model
 * 
 * @author Dejan Antanaskovic, <a href="mailto:dejan.antanaskovic@tuhh.de">dejan.antanaskovic@tuhh.de</a>
 */
@SuppressWarnings( { "unchecked" })
public class Gml2RMA10SConv implements INativeIDProvider
{
  private final LinkedHashMap<String, Integer> m_roughnessIDProvider = new LinkedHashMap<String, Integer>( 100 );

  private final LinkedHashMap<String, Integer> m_nodesIDProvider = new LinkedHashMap<String, Integer>( 100000 );

  private final LinkedHashMap<String, Integer> m_elementsIDProvider = new LinkedHashMap<String, Integer>( 50000 );

  private final LinkedHashMap<String, Integer> m_complexElementsIDProvider = new LinkedHashMap<String, Integer>();

  private final LinkedHashMap<String, Integer> m_edgesIDProvider = new LinkedHashMap<String, Integer>( 100000 );

  private final WeirIDProvider m_weirIDProvider = new WeirIDProvider();

  private final File m_outputFile;

  private final IFEDiscretisationModel1d2d m_discretisationModel1d2d;

  private final ITerrainModel m_terrainModel;

  private final IFlowRelationshipModel m_flowrelationModel;

  private ICalculationUnit m_calculationUnit;

  private GM_Envelope m_calcUnitBBox;

  private RestartEater m_restartEater;

  // private Formatter formatter;
  //
  // private PrintWriter stream;
  private RMA10Calculation m_calculation;

  private final boolean m_restart;

  public Gml2RMA10SConv( final File rma10sOutputFile, final RMA10Calculation calculation )
  {
    m_outputFile = rma10sOutputFile;
    m_discretisationModel1d2d = calculation.getDiscModel();
    m_terrainModel = calculation.getTerrainModel();
    m_flowrelationModel = calculation.getFlowModel();
    m_calculationUnit = calculation.getCalculationUnit();
    m_calcUnitBBox = CalUnitOps.getBoundingBox( m_calculationUnit );

    // provides the ids for the boundaryline
    m_calculation = calculation;

    // initialize Roughness IDs
    for( final Object o : calculation.getRoughnessClassList() )
    {
      final Feature roughnessCL = (Feature) o;
      getID( m_roughnessIDProvider, roughnessCL.getId() );
    }
    m_restart = m_calculation.getControlModel().getRestart();
    if( m_restart )
    {
      m_restartEater = new RestartEater();
      final IFolder scenarioFolder = Util.getScenarioFolder();
      for( final String path : m_calculation.getControlModel().getRestartPaths() )
      {
        final IFile ifile = scenarioFolder.getFile( path );
        final File file = ifile.getRawLocation().toFile();
        try
        {
          m_restartEater.addResultFile( file );
        }
        catch( final Exception e )
        {
          // TODO Auto-generated catch block
          e.printStackTrace();
        }
      }
    }
  }

  public Gml2RMA10SConv( final File rma10sOutputFile, final IFEDiscretisationModel1d2d discretisationModel1d2d, final ITerrainModel terrainModel, final IFlowRelationshipModel flowrelationModel, final List roughnessClassList )
  {
    m_outputFile = rma10sOutputFile;
    m_discretisationModel1d2d = discretisationModel1d2d;
    m_terrainModel = terrainModel;
    m_flowrelationModel = flowrelationModel;

    m_restart = false;
  }

  public boolean containsID( final IFeatureWrapper2 i1d2dObject )
  {
    if( i1d2dObject == null )
      return false;
    final String id = i1d2dObject.getGmlID();
    if( i1d2dObject instanceof IFE1D2DNode )
    {
      return m_nodesIDProvider.containsKey( id );
    }
    else if( i1d2dObject instanceof IFE1D2DEdge )
    {
      return m_edgesIDProvider.containsKey( id );
    }
    else if( i1d2dObject instanceof IBoundaryLine )
    {
      return m_calculation.containsID( i1d2dObject );
    }
    else if( i1d2dObject instanceof IFE1D2DElement )
    {
      return m_elementsIDProvider.containsKey( id );
    }
    else if( i1d2dObject instanceof IFE1D2DComplexElement )
    {
      return m_complexElementsIDProvider.containsKey( id );
    }
    else
    {
      return false;
    }
  }

  public int getID( final IFeatureWrapper2 i1d2dObject )
  {
    if( i1d2dObject == null )
      return 0;
    final String id = i1d2dObject.getGmlID();
    if( i1d2dObject instanceof IFE1D2DNode )
    {
      return getID( m_nodesIDProvider, id );
    }
    else if( i1d2dObject instanceof IFE1D2DEdge )
    {
      return getID( m_edgesIDProvider, id );
    }
    else if( i1d2dObject instanceof IBoundaryLine )
    {
      return m_calculation.getID( i1d2dObject );
    }
    else if( i1d2dObject instanceof IFE1D2DElement )
    {
      return getID( m_elementsIDProvider, id );
    }
    else if( i1d2dObject instanceof IFE1D2DComplexElement )
    {
      return getID( m_complexElementsIDProvider, id );
    }
    else
    {
      return 0;
    }
  }

  private int getID( final LinkedHashMap<String, Integer> map, final String gmlID )
  {
    if( !map.containsKey( gmlID ) )
    {
      final int id = map.size() + 1;
      map.put( gmlID, id );
      return id;
    }
    else
      return map.get( gmlID );
  }

  public void toRMA10sModel( ) throws IllegalStateException, SimulationException
  {
    PrintWriter stream = null;
    try
    {
      stream = new PrintWriter( m_outputFile );
      writeRMA10sModel( stream );
      stream.close();
    }
    catch( final IOException ioe )
    {
      throw new SimulationException( "Fehler beim Schreiben der Modelldatei: " + ioe.getLocalizedMessage(), ioe );
    }
    catch( final GM_Exception gme )
    {
      throw new SimulationException( "Fehler beim Erzeugen von Geometrien: " + gme.getLocalizedMessage(), gme );
    }
    finally
    {
      IOUtils.closeQuietly( stream );
    }
  }

  // /**
  // * Call after boundary lines writing because it requires the IDs
  // */
  // public void writeJuntionContextTRMA10sModel( ) throws SimulationException, GM_Exception
  // {
  // JunctionContextConverter.write(
  // m_discretisationModel1d2d,
  // m_calcultionUnit,
  // this,
  // formatter );
  // }

  private void writeRMA10sModel( final PrintWriter stream ) throws SimulationException, GM_Exception
  {
    final IFeatureWrapperCollection<IFE1D2DElement> elements = m_discretisationModel1d2d.getElements();
    final IFeatureWrapperCollection<IFE1D2DNode> nodes = m_discretisationModel1d2d.getNodes();
    final IFeatureWrapperCollection<IFE1D2DEdge> edges = m_discretisationModel1d2d.getEdges();

    /* Made a central formatter with US locale, so no locale parameter for each format is needed any more . */
    final Formatter formatter = new Formatter( stream, Locale.US );

    // first method not supporting filtering not working properly
    // if( m_terrainModel != null )
    // {
    // final IRoughnessPolygonCollection roughnessPolygonCollection = m_terrainModel.getRoughnessPolygonCollection();
    // writeElements( formatter, m_roughnessIDProvider, elements, roughnessPolygonCollection );
    // }
    // else
    // {
    // // TODO: is this reallly possible? Better: throw an exception?
    // writeElements( formatter, elements );
    // }
    // writeNodes( formatter, nodes );
    // writeEdges( formatter, edges );
    // JunctionContextConverter.write( m_discretisationModel1d2d, m_calculationUnit, this, formatter );

    // second methods with filtering of edges and nodes
    final IRoughnessPolygonCollection roughnessPolygonCollection = m_terrainModel.getRoughnessPolygonCollection();
    writeElementsNodesAndEdges( formatter, m_roughnessIDProvider, elements, roughnessPolygonCollection );
    JunctionContextConverter.write( m_discretisationModel1d2d, m_calculationUnit, this, formatter );

  }

  private void writeEdgeSet( final Formatter formatter, final Collection<IFE1D2DEdge> edges ) throws GM_Exception
  {

    int cnt = 1;
    for( final IFE1D2DEdge edge : edges )
    {
      if( edge instanceof IEdgeInv )
      {
        continue;
      }

      final int node0ID = getID( edge.getNode( 0 ) );
      final int node1ID = getID( edge.getNode( 1 ) );

      /*
       * If we have no middle node (which is always the case), create it on the fly (just takes middle of edge). This is
       * needed for the restart approach later.
       */
      final int middleNodeID;
      if( edge.getMiddleNode() == null )
      {
        /* create virtual node id */
        final String gmlID = "VirtualMiddleNode" + edge.getGmlID(); // Pseudo id, but unique within this context
        middleNodeID = getID( getNodesIDProvider(), gmlID );

       /* Calculate middle of arc. */
        /**
         * JTSUtilities.pointOnLinePercent( edgeLine, 50 ) doesn't give the proper middle node of the edge!
         * 
         * 
         * final GM_Curve curve = edge.getCurve(); final LineString edgeLine = (LineString) JTSAdapter.export( curve );
         * final Point point = JTSUtilities.pointOnLinePercent( edgeLine, 50 ); final GM_Point middleNodePoint =
         * (GM_Point) JTSAdapter.wrap( point );
         * 
         * final GM_Point edgeMN = edge.getMiddleNodePoint();
         * 
         * System.out.println(middleNodePoint.getX()+";"+edgeMN.getX()+";"+middleNodePoint.getY()+";"+edgeMN.getY()+";"+(middleNodePoint.getX()-edgeMN.getX())+";"+(middleNodePoint.getY()-edgeMN.getY()));
         * 
         */

        /* Write it: Station is not needed, because the element length is taken from real nodes. */
        formatNode( formatter, middleNodeID, edge.getMiddleNodePoint(), null );

      }
      else
      {
        middleNodeID = getID( edge.getMiddleNode() );
      }

      /* Directly format into the string, this is quickest! */
      // System.out.println( edge.getGmlID() + " --> " + getID( edge ) );
      if( TypeInfo.is1DEdge( edge ) )
      {
        int leftRightID = 0;
        if( edge.getContainers().size() > 0 )
        {
          final Object object = edge.getContainers().get( 0 );

          if( object instanceof IElement1D )
            leftRightID = getID( ((IElement1D) object) );
        }
        formatter.format( "AR%10d%10d%10d%10d%10d%10d%n", cnt++, node0ID, node1ID, leftRightID, leftRightID, middleNodeID );
      }
      else if( TypeInfo.is2DEdge( edge ) )
      {

        final IFE1D2DElement leftElement = EdgeOps.getLeftRightElement( m_calculationUnit, edge, EdgeOps.ORIENTATION_LEFT );
        final IFE1D2DElement rightElement = EdgeOps.getLeftRightElement( m_calculationUnit, edge, EdgeOps.ORIENTATION_RIGHT );
        final int leftParent = getID( leftElement );
        final int rightParent = getID( rightElement );
        formatter.format( "AR%10d%10d%10d%10d%10d%10d%n", cnt++, node0ID, node1ID, leftParent, rightParent, middleNodeID );
      }
      else
      {
        // stream.println( "************************************** non 1d/2d edge: " + edge.getGmlID() );
        System.out.println( "non 1d/2d edge: " + edge.getGmlID() );
      }
    }
  }

  private void writeEdges( final Formatter formatter, final IFeatureWrapperCollection<IFE1D2DEdge> edges ) throws GM_Exception
  {
    final List<IFE1D2DEdge> edgeInBBox = edges.query( m_calcUnitBBox );
    int cnt = 1;
    for( final IFE1D2DEdge edge : edgeInBBox/* edges */)
    {
      if( edge instanceof IEdgeInv )
      {
        continue;
      }

      // if( !CalUnitOps.isEdgeOf( m_calculationUnit, edge ) )
      // {
      // continue;
      // }

      final int node0ID = getID( edge.getNode( 0 ) );
      final int node1ID = getID( edge.getNode( 1 ) );

      /*
       * If we have no middle node (which is always the case), create it on the fly (just takes middle of edge). This is
       * needed for the restart approach later.
       */
      final int middleNodeID;
      if( edge.getMiddleNode() == null )
      {
        /* create virtual node id */
        final String gmlID = "VirtualMiddleNode" + edge.getGmlID(); // Pseudo id, but unique within this context
        middleNodeID = getID( getNodesIDProvider(), gmlID );

        /* Calculate middle of arc. */
        final GM_Curve curve = edge.getCurve();
        final LineString edgeLine = (LineString) JTSAdapter.export( curve );
        final Point point = JTSUtilities.pointOnLinePercent( edgeLine, 50 );
        final GM_Point middleNodePoint = (GM_Point) JTSAdapter.wrap( point );

        /* Write it: Station is not needed, because the element length is taken from real nodes. */
        formatNode( formatter, middleNodeID, middleNodePoint, null );
      }
      else
      {
        middleNodeID = getID( edge.getMiddleNode() );
      }

      /* Directly format into the string, this is quickest! */
      // System.out.println( edge.getGmlID() + " --> " + getID( edge ) );
      if( TypeInfo.is1DEdge( edge ) )
      {
        int leftRightID = 0;
        if( edge.getContainers().size() > 0 )
        {
          final Object object = edge.getContainers().get( 0 );

          if( object instanceof IElement1D )
            leftRightID = getID( ((IElement1D) object) );
        }
        formatter.format( "AR%10d%10d%10d%10d%10d%10d%n", cnt++, node0ID, node1ID, leftRightID, leftRightID, middleNodeID );
      }
      else if( TypeInfo.is2DEdge( edge ) )
      {

        final IFE1D2DElement leftElement = EdgeOps.getLeftRightElement( m_calculationUnit, edge, EdgeOps.ORIENTATION_LEFT );
        final IFE1D2DElement rightElement = EdgeOps.getLeftRightElement( m_calculationUnit, edge, EdgeOps.ORIENTATION_RIGHT );
        final int leftParent = getID( leftElement );
        final int rightParent = getID( rightElement );
        formatter.format( "AR%10d%10d%10d%10d%10d%10d%n", cnt++, node0ID, node1ID, leftParent, rightParent, middleNodeID );
      }
      else
      {
        // stream.println( "************************************** non 1d/2d edge: " + edge.getGmlID() );
        System.out.println( "non 1d/2d edge: " + edge.getGmlID() );
      }
    }
  }

  private void writeNodes( final Formatter formatter, final IFeatureWrapperCollection<IFE1D2DNode> nodes ) throws SimulationException
  {
    final List<IFE1D2DNode> nodesInBBox = nodes.query( m_calcUnitBBox );
    for( final IFE1D2DNode<IFE1D2DEdge> node : nodesInBBox/* nodes */)
    {
      // if( !CalUnitOps.isNodeOf( m_calculationUnit, node ) )
      // {
      // continue;
      // }
      if( containsID( node ) )
      {
        continue;
      }
      /* The node itself */
      final int nodeID = getID( node );
      final GM_Point point = node.getPoint();
      BigDecimal station = null;

      if( DiscretisationModelUtils.is1DNode( node ) )
      {
        /* Node parameters */
        final IFlowRelationship relationship = m_flowrelationModel.findFlowrelationship( point.getPosition(), 0.0 );
        if( relationship == null )
          throw new SimulationException( "1D-Knoten ohne Flieﬂparameter gefunden: " + node.getGmlID(), null );

        if( relationship instanceof IKingFlowRelation )
        {
          final IKingFlowRelation kingRelation = (IKingFlowRelation) relationship;
          final BigDecimal width = kingRelation.getWidth();
          final BigDecimal bankSlopeLeft = kingRelation.getBankSlopeLeft();
          final BigDecimal bankSlopeRight = kingRelation.getBankSlopeRight();
          final BigDecimal widthStorage = kingRelation.getWidthStorage();
          final BigDecimal heightStorage = kingRelation.getHeightStorage();
          final BigDecimal slopeStorage = kingRelation.getSlopeStorage();
          formatter.format( "CS%10d%10.1f%10.3f%10.3f%10.2f%10.2f%10.2f%n", nodeID, width, bankSlopeLeft, bankSlopeRight, widthStorage, heightStorage, slopeStorage );
        }
        else if( relationship instanceof ITeschkeFlowRelation )
        {
          final ITeschkeFlowRelation teschkeRelation = (ITeschkeFlowRelation) relationship;
          station = teschkeRelation.getStation();

          final IPolynomial1D[] polynomials = teschkeRelation.getPolynomials();
          final TeschkeRelationConverter teschkeConv = new TeschkeRelationConverter( polynomials );
          final double slope = teschkeRelation.getSlope();
          final Double min = teschkeConv.getMin();
          final Double max = teschkeConv.getMax();

          formatter.format( "MM%10d%20.7f%20.7f%n", nodeID, min, max );

          final IPolynomial1D[] polyArea = teschkeConv.getPolynomialsByType( IWspmTuhhQIntervallConstants.DICT_PHENOMENON_AREA );
          writePolynome( formatter, "AP1", nodeID, polyArea[0], 0, 5, null );
          writePolynome( formatter, "AP2", nodeID, polyArea[0], 5, 10, null );
          writePolynome( formatter, "AP3", nodeID, polyArea[0], 10, 13, null );

          final IPolynomial1D[] polyRunoff = teschkeConv.getPolynomialsByType( IWspmTuhhQIntervallConstants.DICT_PHENOMENON_RUNOFF );
          writePolynome( formatter, "QP1", nodeID, polyRunoff[0], 0, 4, slope );
          writePolynome( formatter, "QP2", nodeID, polyRunoff[0], 4, 9, null );
          writePolynome( formatter, "QP3", nodeID, polyRunoff[0], 9, 13, null );

          final IPolynomial1D[] polyAlpha = teschkeConv.getPolynomialsByType( IWspmTuhhQIntervallConstants.DICT_PHENOMENON_ALPHA );

          if( polyAlpha == null )
            throw new SimulationException( "Kein Alpha-Polynom f¸r Teschke-Netzparameter an Station km: " + station, null );

          if( polyAlpha.length > 1 )
          {
            final double hBV = polyAlpha[1].getRangeMin(); // Bordvollhˆhe ist gleich anfang des ‹bergangsbereich
            formatter.format( "HB%10d%20.7f%n", nodeID, hBV );
            writePolynome( formatter, "AD ", nodeID, polyAlpha[1], 0, 4, polyAlpha[1].getRangeMax() );
          }
          if( polyAlpha.length > 2 )
          {
            writePolynome( formatter, "AK1", nodeID, polyAlpha[2], 0, 5, null );
            writePolynome( formatter, "AK2", nodeID, polyAlpha[2], 5, 10, null );
            writePolynome( formatter, "AK3", nodeID, polyAlpha[2], 10, 13, null );
          }
        }
        else
          throw new SimulationException( "Unbekannte Flieﬂrelation gefunden: " + relationship, null );
      }
      formatNode( formatter, nodeID, point, station );
    }
  }

  private void formatNode( final Formatter formatter, final int nodeID, final GM_Point point, final BigDecimal station )
  {
    /* Now really write the nodes */
    final double x = point.getX();
    final double y = point.getY();
    double z = Double.NaN;

    // TODO: Here we should decide what we do with non-elevation-assigned nodes. For now the elevation of these nodes
    // will be set to '-9999'
    if( point.getCoordinateDimension() == 3 )
      z = point.getZ();
    else
      z = -9999;

    if( station == null )
      formatter.format( "FP%10d%20.7f%20.7f%20.7f%n", nodeID, x, y, z );
    else
      formatter.format( "FP%10d%20.7f%20.7f%20.7f%20.7f%n", nodeID, x, y, z, station );
    if( m_restart )
      writeRestartLines( formatter, nodeID, x, y, z );
  }

  private void writePolynome( final Formatter formatter, final String kind, final int nodeID, final IPolynomial1D poly, final int coeffStart, final int coeffStop, final Double extraValue )
  {
    formatter.format( "%3s%9d", kind, nodeID );

    if( extraValue != null )
      formatter.format( "%20.7f", extraValue );

    final double[] coefficients = poly.getCoefficients();
    for( int j = coeffStart; j < coeffStop; j++ )
    {
      final double coeff;
      if( j < coefficients.length )
        coeff = coefficients[j];
      else
        coeff = 0.0;

      formatter.format( "%20.7f", coeff );
    }

    formatter.format( "%n" );
  }

  private void writeElements( final Formatter formatter, final LinkedHashMap<String, Integer> roughnessIDProvider, final IFeatureWrapperCollection<IFE1D2DElement> elements, final IRoughnessPolygonCollection roughnessPolygonCollection ) throws GM_Exception, SimulationException
  {
    final List<IFE1D2DElement> elementsInBBox = elements.query( m_calcUnitBBox );

    for( final IFE1D2DElement element : elementsInBBox/* elements */)
    {
      if( !CalUnitOps.isFiniteElementOf( m_calculationUnit, element ) )
      {
        continue;
      }

      final int id = getID( element );

      if( element instanceof IElement1D )
      {
        /* 1D-Elements get special handling. */
        final IElement1D element1D = (IElement1D) element;

        final IWeirFlowRelation weir = FlowRelationUtilitites.findWeirElement1D( element1D, m_flowrelationModel );
        if( weir != null )
        {
          /* A Weir? Create dynamic weir number and use it as weir ID. */
          final int weirID = m_weirIDProvider.addWeir( weir );
          final IFE1D2DNode upstreamNode = weir.getUpstreamNode();
          final int upstreamNodeID = getID( upstreamNode );
          formatter.format( "FE%10d%10d%10s%10s%10d%n", id, weirID, "", "", upstreamNodeID );
        }
        else if( FlowRelationUtilitites.isTeschkeElement1D( element1D, m_flowrelationModel ) )
        {
          /* Element without building: The special roughness-class '89' should be used. */
          formatter.format( "FE%10d%10d%n", id, 89 );
        }
        else
        {
          // TODO: give hint what 1D-element is was?
          throw new SimulationException( "1D-Element ohne Bauwerk bzw. ohne Netzparameter: " + element1D.getGmlID(), null );
        }
      }
      else if( element instanceof IPolyElement )
      {
        final int roughnessID = calculateRoughnessID( roughnessIDProvider, roughnessPolygonCollection, element );
        formatter.format( "FE%10d%10d%n", id, roughnessID );
      }
    }
  }

  /**
   * write elements nodes and edges in a way which avoids the filtering of edges and nodes
   * 
   */
  private void writeElementsNodesAndEdges( final Formatter formatter, final LinkedHashMap<String, Integer> roughnessIDProvider, final IFeatureWrapperCollection<IFE1D2DElement> elements, final IRoughnessPolygonCollection roughnessPolygonCollection ) throws GM_Exception, SimulationException
  {
    final List<IFE1D2DElement> elementsInBBox = elements.query( m_calcUnitBBox );
    final HashSet<IFE1D2DEdge> edgeSet = new HashSet<IFE1D2DEdge>( elementsInBBox.size() * 2 );

    for( final IFE1D2DElement element : elementsInBBox/* elements */)
    {
      if( !CalUnitOps.isFiniteElementOf( m_calculationUnit, element ) )
      {
        continue;
      }

      final int id = getID( element );

      if( element instanceof IElement1D )
      {
        contributeToSet( element, edgeSet );
        /* 1D-Elements get special handling. */
        final IElement1D element1D = (IElement1D) element;

        final IWeirFlowRelation weir = FlowRelationUtilitites.findWeirElement1D( element1D, m_flowrelationModel );
        if( weir != null )
        {
          /* A Weir? Create dynamic weir number and use it as weir ID. */
          final int weirID = m_weirIDProvider.addWeir( weir );
          final IFE1D2DNode upstreamNode = weir.getUpstreamNode();
          final int upstreamNodeID = getID( upstreamNode );
          formatter.format( "FE%10d%10d%10s%10s%10d%n", id, weirID, "", "", upstreamNodeID );
        }
        else if( FlowRelationUtilitites.isTeschkeElement1D( element1D, m_flowrelationModel ) )
        {
          /* Element without building: The special roughness-class '89' should be used. */
          formatter.format( "FE%10d%10d%n", id, 89 );
        }
        else
        {
          // TODO: give hint what 1D-element is was?
          throw new SimulationException( "1D-Element ohne Bauwerk bzw. ohne Netzparameter: " + element1D.getGmlID(), null );
        }
      }
      else if( element instanceof IPolyElement )
      {
        contributeToSet( element, edgeSet );
        final int roughnessID = calculateRoughnessID( roughnessIDProvider, roughnessPolygonCollection, element );
        formatter.format( "FE%10d%10d%n", id, roughnessID );
      }
    }

    // write edge set nodes
    for( final IFE1D2DEdge edge : edgeSet )
    {
      writeNodes( formatter, edge.getNodes() );
    }

    // write edges
    writeEdgeSet( formatter, edgeSet );
  }

  /**
   * Collects the element edges and put them into the provides set
   * 
   */
  private static final void contributeToSet( final IFE1D2DElement<IFE1D2DComplexElement, IFE1D2DEdge> ele, final HashSet<IFE1D2DEdge> edgeSet )
  {
    if( ele instanceof IElement1D )
    {
      IFE1D2DEdge edge = ((IElement1D) ele).getEdge();
      if( edge instanceof IEdgeInv )
      {
        edge = ((IEdgeInv) edge).getInverted();
      }
      edgeSet.add( edge );
    }
    else if( ele instanceof IPolyElement )
    {
      for( IFE1D2DEdge edge : ((IPolyElement<IFE1D2DComplexElement, IFE1D2DEdge>) ele).getEdges() )
      {
        if( edge instanceof IEdgeInv )
        {
          edge = ((IEdgeInv) edge).getInverted();
        }
        edgeSet.add( edge );
      }
    }
  }

  private void writeElements( final Formatter formatter, final IFeatureWrapperCollection<IFE1D2DElement> elements )
  {
    for( final IFE1D2DElement element : elements )
    {
      // TODO: this FE line only makes sense for 2D elements; handle cases of 1D Elements as well
      // BUT: only if this code is really used, see todo above
      if( element instanceof IElement2D )
        formatter.format( "FE%10d%10d%10d%10d%n", getID( element ), 0, 1, 0 );
    }
  }

  private void writeRestartLines( final Formatter formatter, final int nodeID, final double x, final double y, final double z )
  {
    final INodeResult node = m_restartEater.getNodeResultAtPosition( x, y );
    double vx = 0.0;
    double vy = 0.0;
    if( node instanceof GMLNodeResult )
    {
      final List<Double> velocity = ((GMLNodeResult) node).getVelocity();
      if( velocity != null )
      {
        vx = velocity.get( 0 );
        vy = velocity.get( 1 );
      }
      else
      {
        vx = node.getAbsoluteVelocity();
        vy = vx;
      }
    }
    else
    {
      vx = node.getAbsoluteVelocity();
      vy = vx;
    }
    formatter.format( "VA%10d%20.7f%20.7f%20.7f%20.7f%n", nodeID, vx, vy, node.getDepth(), node.getWaterlevel() );
  }

  private int calculateRoughnessID( final LinkedHashMap<String, Integer> roughnessIDProvider, final IRoughnessPolygonCollection roughnessPolygonCollection, final IFE1D2DElement element ) throws GM_Exception, SimulationException
  {
    final IRoughnessEstimateSpec roughnessEstimateSpec = roughnessPolygonCollection.getRoughnessEstimateSpec( element.recalculateElementGeometry() );
    if( roughnessEstimateSpec != null )
    {
      final IRoughnessCls[] cls = roughnessEstimateSpec.mostSpreadRoughness();
      if( cls.length > 0 && cls[0] != null )
        return getID( roughnessIDProvider, cls[0].getGmlID() );
    }

    throw new SimulationException( "Keine Rauheitszone gefunden: " + element, null );
  }

  public final LinkedHashMap<String, Integer> getRoughnessIDProvider( )
  {
    return m_roughnessIDProvider;
  }

  public final LinkedHashMap<String, Integer> getNodesIDProvider( )
  {
    return m_nodesIDProvider;
  }

  public WeirIDProvider getWeirProvider( )
  {
    return m_weirIDProvider;
  }

}
