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
import org.kalypso.kalypsomodel1d2d.conv.results.RestartEater;
import org.kalypso.kalypsomodel1d2d.ops.CalcUnitOps;
import org.kalypso.kalypsomodel1d2d.ops.EdgeOps;
import org.kalypso.kalypsomodel1d2d.ops.TypeInfo;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.DiscretisationModelUtils;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IBoundaryLine;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IEdgeInv;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IElement1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DComplexElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IPolyElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.FlowRelationUtilitites;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IBuildingFlowRelation;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IKingFlowRelation;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.ITeschkeFlowRelation;
import org.kalypso.kalypsomodel1d2d.schema.binding.results.GMLNodeResult;
import org.kalypso.kalypsomodel1d2d.schema.binding.results.INodeResult;
import org.kalypso.kalypsomodel1d2d.sim.RMA10Calculation;
import org.kalypso.kalypsosimulationmodel.core.Util;
import org.kalypso.kalypsosimulationmodel.core.flowrel.IFlowRelationship;
import org.kalypso.kalypsosimulationmodel.core.flowrel.IFlowRelationshipModel;
import org.kalypso.model.wspm.tuhh.schema.schemata.IWspmTuhhQIntervallConstants;
import org.kalypso.simulation.core.SimulationException;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.gml.binding.math.IPolynomial1D;

/**
 * Converts discretisation model to RMA10s model
 * 
 * @author Dejan Antanaskovic, <a href="mailto:dejan.antanaskovic@tuhh.de">dejan.antanaskovic@tuhh.de</a>
 */
public class Gml2RMA10SConv implements INativeIDProvider
{
  private final LinkedHashMap<String, Integer> m_roughnessIDProvider = new LinkedHashMap<String, Integer>( 100 );

  private final LinkedHashMap<String, Integer> m_nodesIDProvider = new LinkedHashMap<String, Integer>( 100000 );

  private final LinkedHashMap<String, Integer> m_elementsIDProvider = new LinkedHashMap<String, Integer>( 50000 );

  private final LinkedHashMap<String, Integer> m_complexElementsIDProvider = new LinkedHashMap<String, Integer>();

  private final LinkedHashMap<String, Integer> m_edgesIDProvider = new LinkedHashMap<String, Integer>( 100000 );

  private final BuildingIDProvider m_buildingIDProvider = new BuildingIDProvider();

  private final File m_outputFile;

  private final IFEDiscretisationModel1d2d m_discretisationModel1d2d;

  private final IFlowRelationshipModel m_flowrelationModel;

  private ICalculationUnit m_calculationUnit;

  private GM_Envelope m_calcUnitBBox;

  private RestartEater m_restartEater;

  private RMA10Calculation m_calculation;

  private boolean m_exportRequest;

  private boolean m_exportMiddleNode;

  private boolean m_exportRoughness;

  private final boolean m_restart;

  public Gml2RMA10SConv( final File rma10sOutputFile, final RMA10Calculation calculation )
  {
    m_outputFile = rma10sOutputFile;
    m_discretisationModel1d2d = calculation.getDiscModel();
    m_flowrelationModel = calculation.getFlowModel();
    m_calculationUnit = calculation.getCalculationUnit();
    m_exportRequest = false;
    m_exportMiddleNode = false;
    m_exportRoughness = false;
    m_calcUnitBBox = CalcUnitOps.getBoundingBox( m_calculationUnit );

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
          // TODO: don't eat exceptions in such critical code
          // rethrow as SimulationException
          e.printStackTrace();
        }
      }
    }
  }

  /**
   * This constructor is intended to use primarily for net exporting purpose, not for calculation (calculation unit is
   * NOT defined)
   */
  public Gml2RMA10SConv( final File rma10sOutputFile, final IFEDiscretisationModel1d2d discretisationModel1d2d, final IFlowRelationshipModel flowrelationModel )
  {
    m_outputFile = rma10sOutputFile;
    m_discretisationModel1d2d = discretisationModel1d2d;
    m_flowrelationModel = flowrelationModel;

    m_restart = false;
    m_exportRequest = true;
    m_exportMiddleNode = false;
    m_exportRoughness = false;
  }

  public void setExportParameters( final boolean exportRequested, final boolean exportMiddleNode, final boolean exportRoughness )
  {
    m_exportRequest = exportRequested;
    m_exportMiddleNode = exportMiddleNode;
    m_exportRoughness = exportRoughness;
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

  public int getBoundaryLineID( final IFeatureWrapper2 i1d2dObject )
  {
    if( i1d2dObject == null ) // TODO: this is probably an error in the data, throw an exception instead?
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
      if( m_exportRequest ) // TODO: check what to do here
        return -9999;
      return m_calculation.getBoundaryLineID( i1d2dObject );
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
      throw new SimulationException( Messages.getString("Gml2RMA10SConv.1") + ioe.getLocalizedMessage(), ioe ); //$NON-NLS-1$
    }
    catch( final GM_Exception gme )
    {
      throw new SimulationException( Messages.getString("Gml2RMA10SConv.2") + gme.getLocalizedMessage(), gme ); //$NON-NLS-1$
    }
    finally
    {
      IOUtils.closeQuietly( stream );
    }
  }

  private void writeRMA10sModel( final PrintWriter stream ) throws SimulationException, GM_Exception
  {
    final IFeatureWrapperCollection<IFE1D2DElement> elements = m_discretisationModel1d2d.getElements();

    /* Made a central formatter with US locale, so no locale parameter for each format is needed any more . */
    final Formatter formatter = new Formatter( stream, Locale.US );

    writeElementsNodesAndEdges( formatter, elements );
    JunctionContextConverter.write( m_discretisationModel1d2d, m_calculationUnit, this, formatter );
  }

  private void writeEdgeSet( final Formatter formatter, final Collection<IFE1D2DEdge> edges )
  {
    int cnt = 1;
    for( final IFE1D2DEdge edge : edges )
    {
      if( edge instanceof IEdgeInv )
      {
        continue;
      }

      final int node0ID = getBoundaryLineID( edge.getNode( 0 ) );
      final int node1ID = getBoundaryLineID( edge.getNode( 1 ) );

      /*
       * If we have no middle node (which is always the case), create it on the fly (just takes middle of edge). This is
       * needed for the restart approach later.
       */
      final int middleNodeID;
      if( m_exportRequest && !m_exportMiddleNode )
      {
        middleNodeID = 0;
      }
      else
      {
        if( edge.getMiddleNode() == null )
        {
          /* create virtual node id */
          final String gmlID = "VirtualMiddleNode" + edge.getGmlID(); // Pseudo id, but unique within this context
          middleNodeID = getID( getNodesIDProvider(), gmlID );

          /* Write it: Station is not needed, because the element length is taken from real nodes. */
          formatNode( formatter, middleNodeID, edge.getMiddleNodePoint(), null );
        }
        else
        {
          middleNodeID = getBoundaryLineID( edge.getMiddleNode() );
        }
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
            leftRightID = getBoundaryLineID( ((IElement1D) object) );
        }
        formatter.format( "AR%10d%10d%10d%10d%10d%10d%n", cnt++, node0ID, node1ID, leftRightID, leftRightID, middleNodeID );  //$NON-NLS-1$
      }
      else if( TypeInfo.is2DEdge( edge ) )
      {
        final IFE1D2DElement leftElement;
        final IFE1D2DElement rightElement;
        if( m_exportRequest )
        {
          leftElement = EdgeOps.getLeftRight( edge, EdgeOps.ORIENTATION_LEFT );
          rightElement = EdgeOps.getLeftRight( edge, EdgeOps.ORIENTATION_RIGHT );
        }
        else
        {
          leftElement = EdgeOps.getLeftRightElement( m_calculationUnit, edge, EdgeOps.ORIENTATION_LEFT );
          rightElement = EdgeOps.getLeftRightElement( m_calculationUnit, edge, EdgeOps.ORIENTATION_RIGHT );
        }
        final int leftParent = getBoundaryLineID( leftElement );
        final int rightParent = getBoundaryLineID( rightElement );
        formatter.format( "AR%10d%10d%10d%10d%10d%10d%n", cnt++, node0ID, node1ID, leftParent, rightParent, middleNodeID );  //$NON-NLS-1$
      }
      else
      {
        // stream.println( "************************************** non 1d/2d edge: " + edge.getGmlID() );
        System.out.println( "non 1d/2d edge: " + edge.getGmlID() );  //$NON-NLS-1$
      }
    }
  }

  private void writeNodes( final Formatter formatter, final IFeatureWrapperCollection<IFE1D2DNode> nodes ) throws SimulationException
  {
    final List<IFE1D2DNode> nodesInBBox = m_exportRequest ? nodes : nodes.query( m_calcUnitBBox );
    for( final IFE1D2DNode<IFE1D2DEdge> node : nodesInBBox )
    {
      // TODO: how is now checked if a node is inside the CalcUnit???
      // if( !CalUnitOps.isNodeOf( m_calculationUnit, node ) )
      // {
      // continue;
      // }

      // TODO: no! we want now to write all nodes. Existnce of the id does NOT make sure it was already written.
      // for example, the building-nodes have already been accesed (hence have an id), but have not yet been written.
      // This should be the only place where the nodes get actually written, so no filtering should happen here (except
      // for the calulationunit).
      // if( containsID( node ) )
      // {
      // continue;
      // }

      /* The node itself */
      final int nodeID = getBoundaryLineID( node );
      final GM_Point point = node.getPoint();
      BigDecimal station = null;

      if( DiscretisationModelUtils.is1DNode( node ) )
      {
        /* Node parameters */
        final IFlowRelationship relationship = m_flowrelationModel.findFlowrelationship( point.getPosition(), 0.0 );
        if( relationship == null )
          throw new SimulationException( Messages.getString("Gml2RMA10SConv.11") + node.getGmlID(), null ); //$NON-NLS-1$

        if( relationship instanceof IKingFlowRelation )
        {
          final IKingFlowRelation kingRelation = (IKingFlowRelation) relationship;
          final BigDecimal width = kingRelation.getWidth();
          final BigDecimal bankSlopeLeft = kingRelation.getBankSlopeLeft();
          final BigDecimal bankSlopeRight = kingRelation.getBankSlopeRight();
          final BigDecimal widthStorage = kingRelation.getWidthStorage();
          final BigDecimal heightStorage = kingRelation.getHeightStorage();
          final BigDecimal slopeStorage = kingRelation.getSlopeStorage();
          formatter.format( "CS%10d%10.1f%10.3f%10.3f%10.2f%10.2f%10.2f%n", nodeID, width, bankSlopeLeft, bankSlopeRight, widthStorage, heightStorage, slopeStorage );  //$NON-NLS-1$
        }
        else if( relationship instanceof ITeschkeFlowRelation )
        {
          final ITeschkeFlowRelation teschkeRelation = (ITeschkeFlowRelation) relationship;
          station = teschkeRelation.getStation();
          if( station == null )
            continue; // TODO: only for debug purpose, throw exception instead
          final IPolynomial1D[] polynomials = teschkeRelation.getPolynomials();
          final TeschkeRelationConverter teschkeConv = new TeschkeRelationConverter( polynomials );
          final double slope = teschkeRelation.getSlope();
          final Double min = teschkeConv.getMin();
          final Double max = teschkeConv.getMax();

          formatter.format( "MM%10d%20.7f%20.7f%n", nodeID, min, max );  //$NON-NLS-1$

          final IPolynomial1D[] polyArea = teschkeConv.getPolynomialsByType( IWspmTuhhQIntervallConstants.DICT_PHENOMENON_AREA );
          if( polyArea == null )
            continue; // TODO: only for debug purpose, throw exception instead

          writePolynome( formatter, "AP1", nodeID, polyArea[0], 0, 5, null );  //$NON-NLS-1$
          writePolynome( formatter, "AP2", nodeID, polyArea[0], 5, 10, null );  //$NON-NLS-1$
          writePolynome( formatter, "AP3", nodeID, polyArea[0], 10, 13, null );  //$NON-NLS-1$

          final IPolynomial1D[] polyRunoff = teschkeConv.getPolynomialsByType( IWspmTuhhQIntervallConstants.DICT_PHENOMENON_RUNOFF );
          writePolynome( formatter, "QP1", nodeID, polyRunoff[0], 0, 4, slope );  //$NON-NLS-1$
          writePolynome( formatter, "QP2", nodeID, polyRunoff[0], 4, 9, null );  //$NON-NLS-1$
          writePolynome( formatter, "QP3", nodeID, polyRunoff[0], 9, 13, null );  //$NON-NLS-1$

          final IPolynomial1D[] polyAlpha = teschkeConv.getPolynomialsByType( IWspmTuhhQIntervallConstants.DICT_PHENOMENON_ALPHA );

          if( polyAlpha == null )
            throw new SimulationException( Messages.getString("Gml2RMA10SConv.20") + station, null ); //$NON-NLS-1$

          if( polyAlpha.length > 1 )
          {
            final double hBV = polyAlpha[1].getRangeMin(); // Bordvollhˆhe ist gleich anfang des ‹bergangsbereich
            formatter.format( "HB%10d%20.7f%n", nodeID, hBV );  //$NON-NLS-1$
            writePolynome( formatter, "AD ", nodeID, polyAlpha[1], 0, 4, polyAlpha[1].getRangeMax() );  //$NON-NLS-1$
          }
          if( polyAlpha.length > 2 )
          {
            writePolynome( formatter, "AK1", nodeID, polyAlpha[2], 0, 5, null );  //$NON-NLS-1$
            writePolynome( formatter, "AK2", nodeID, polyAlpha[2], 5, 10, null );  //$NON-NLS-1$
            writePolynome( formatter, "AK3", nodeID, polyAlpha[2], 10, 13, null );  //$NON-NLS-1$
          }
        }
        else
          throw new SimulationException( Messages.getString("Gml2RMA10SConv.26") + relationship, null ); //$NON-NLS-1$
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
      formatter.format( "FP%10d%20.7f%20.7f%20.7f%n", nodeID, x, y, z );  //$NON-NLS-1$
    else
      formatter.format( "FP%10d%20.7f%20.7f%20.7f%20.7f%n", nodeID, x, y, z, station );  //$NON-NLS-1$
    if( m_restart )
      writeRestartLines( formatter, nodeID, x, y );
  }

  private void writePolynome( final Formatter formatter, final String kind, final int nodeID, final IPolynomial1D poly, final int coeffStart, final int coeffStop, final Double extraValue )
  {
    formatter.format( "%3s%9d", kind, nodeID );  //$NON-NLS-1$

    if( extraValue != null )
      formatter.format( "%20.7f", extraValue );  //$NON-NLS-1$

    final double[] coefficients = poly.getCoefficients();
    for( int j = coeffStart; j < coeffStop; j++ )
    {
      final double coeff;
      if( j < coefficients.length )
        coeff = coefficients[j];
      else
        coeff = 0.0;

      formatter.format( "%20.7f", coeff );  //$NON-NLS-1$
    }

    formatter.format( "%n" );  //$NON-NLS-1$
  }

  /**
   * write elements nodes and edges in a way which avoids the filtering of edges and nodes
   * 
   */
  private void writeElementsNodesAndEdges( final Formatter formatter, final IFeatureWrapperCollection<IFE1D2DElement> elements ) throws GM_Exception, SimulationException
  {
    final List<IFE1D2DElement> elementsInBBox = m_exportRequest ? elements : elements.query( m_calcUnitBBox );
    final HashSet<IFE1D2DEdge> edgeSet = new HashSet<IFE1D2DEdge>( elementsInBBox.size() * 2 );

    for( final IFE1D2DElement element : elementsInBBox )
    {
      // TODO: shouldnt the check for calulation unit always happens? -> So export is per calculation unit?
      if( !m_exportRequest && !CalcUnitOps.isFiniteElementOf( m_calculationUnit, element ) )
        continue;

      if( m_exportRequest && element instanceof IElement1D )
        continue;

      final int id = getBoundaryLineID( element );

      if( element instanceof IElement1D )
      {
        contributeToSet( element, edgeSet );
        /* 1D-Elements get special handling. */
        final IElement1D element1D = (IElement1D) element;

        final IBuildingFlowRelation building = FlowRelationUtilitites.findBuildingElement1D( element1D, m_flowrelationModel );
        if( building != null )
        {
          /* A Building? Create dynamic building number and use it as building ID. */
          final int buildingID = m_buildingIDProvider.addBuilding( building );
          final IFE1D2DNode upstreamNode = FlowRelationUtilitites.findUpstreamNode( building, m_discretisationModel1d2d );
          final int upstreamNodeID = getBoundaryLineID( upstreamNode );
          formatter.format( "FE%10d%10d%10s%10s%10d%n", id, buildingID, "", "", upstreamNodeID );    //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
        }
        else if( FlowRelationUtilitites.isTeschkeElement1D( element1D, m_flowrelationModel ) )
        {
          /* Element without building: The special roughness-class '89' should be used. */
          formatter.format( "FE%10d%10d%n", id, 89 );  //$NON-NLS-1$
        }
        else
        {
          // TODO: give hint what 1D-element is was?
          throw new SimulationException( Messages.getString("Gml2RMA10SConv.43") + element1D.getGmlID(), null ); //$NON-NLS-1$
        }
      }
      else if( element instanceof IPolyElement )
      {
        contributeToSet( element, edgeSet );
        final int roughnessID = (m_exportRequest && !m_exportRoughness) ? 0 : getRoughnessID( element );
        formatter.format( "FE%10d%10d%n", id, roughnessID );

        // print roughness correction parameters only if there is any correction
        Double correctionKS = element.getRoughnessCorrectionKS();
        Double correctionAxAy = element.getRoughnessCorrectionAxAy();
        Double correctionDP = element.getRoughnessCorrectionDP();
        if( correctionKS == null || correctionKS.isNaN() )
          correctionKS = 1.0;
        if( correctionAxAy == null || correctionAxAy.isNaN() )
          correctionAxAy = 1.0;
        if( correctionDP == null || correctionDP.isNaN() )
          correctionDP = 1.0;
        if( correctionKS != 1.0 || correctionAxAy != 1.0 || correctionDP != 1.0 )
          formatter.format( "RC%10d%10.6f%10.6f%10.6f%n", id, correctionKS.doubleValue(), correctionAxAy.doubleValue(), correctionDP.doubleValue() );
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

  private void writeRestartLines( final Formatter formatter, final int nodeID, final double x, final double y )
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
    formatter.format( "VA%10d%20.7f%20.7f%20.7f%20.7f%n", nodeID, vx, vy, node.getDepth(), node.getWaterlevel() ); //$NON-NLS-1$
  }

  private int getRoughnessID( final IFE1D2DElement element ) throws SimulationException
  {
    final String roughnessClsID = element.getRoughnessClsID();
    if( roughnessClsID != null && roughnessClsID.length() > 0 )
      return getID( m_roughnessIDProvider, roughnessClsID );
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

  public BuildingIDProvider getBuildingProvider( )
  {
    return m_buildingIDProvider;
  }

}
