/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
import java.math.BigDecimal;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Formatter;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Set;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.kalypso.contribs.java.util.FormatterUtils;
import org.kalypso.kalypsomodel1d2d.conv.results.RestartNodes;
import org.kalypso.kalypsomodel1d2d.ops.CalcUnitOps;
import org.kalypso.kalypsomodel1d2d.ops.TypeInfo;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.DiscretisationModelUtils;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IContinuityLine1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IElement1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DComplexElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFELine;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IJunctionElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IPolyElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ITransitionElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.PolyElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.FlowRelationUtilitites;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IBuildingFlowRelation;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IKingFlowRelation;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.ITeschkeFlowRelation;
import org.kalypso.kalypsomodel1d2d.schema.binding.results.INodeResult;
import org.kalypso.kalypsomodel1d2d.sim.ISimulation1D2DConstants;
import org.kalypso.kalypsomodel1d2d.ui.geolog.IGeoLog;
import org.kalypso.kalypsosimulationmodel.core.flowrel.IFlowRelationship;
import org.kalypso.kalypsosimulationmodel.core.flowrel.IFlowRelationshipModel;
import org.kalypso.kalypsosimulationmodel.core.roughness.IRoughnessCls;
import org.kalypso.kalypsosimulationmodel.core.roughness.IRoughnessClsCollection;
import org.kalypso.model.wspm.tuhh.schema.schemata.IWspmTuhhQIntervallConstants;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.gml.binding.commons.IGeoStatus;
import org.kalypsodeegree_impl.gml.binding.math.IPolynomial1D;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * Converts discretisation model to RMA10s model
 * 
 * @author Dejan Antanaskovic, <a href="mailto:dejan.antanaskovic@tuhh.de">dejan.antanaskovic@tuhh.de</a>
 */
public class Gml2RMA10SConv implements INativeIDProvider
{
  private static enum LINE_CASES
  {
    VA,
    GA,
    VO,
    GO
  }

  private final IdMap m_roughnessIDProvider;

  private final IdMap m_nodesIDProvider = new IdMap();

  private final IdMap m_elementsIDProvider = new IdMap();

  private final IdMap m_complexElementsIDProvider = new IdMap();

  private final IdMap m_edgesIDProvider = new IdMap();

  private final IdMap m_linesIDProvider = new IdMap();

  // TODO probably identical to m_nodesProvider (its key set))
  private final Set<String> m_writtenNodesIDs = new HashSet<String>();

  private final BuildingIDProvider m_buildingIDProvider = new BuildingIDProvider();

  private final IFEDiscretisationModel1d2d m_discretisationModel1d2d;

  private final IFlowRelationshipModel m_flowrelationModel;

  private final ICalculationUnit m_calculationUnit;

  private final GM_Envelope m_calcUnitBBox;

  private final RestartNodes m_restartNodes;

  private final boolean m_exportRequest;

  private final boolean m_exportMiddleNode;

  private final IGeoLog m_log;

  // TODO: check: calculation?
  public Gml2RMA10SConv( final IFEDiscretisationModel1d2d discretisationModel1d2d, final IFlowRelationshipModel flowrelationModel, final ICalculationUnit calcUnit, final IRoughnessClsCollection roughnessModel, final RestartNodes restartNodes, final boolean exportRequested, final boolean exportMiddleNode, final IGeoLog log )
  {
    m_discretisationModel1d2d = discretisationModel1d2d;
    m_flowrelationModel = flowrelationModel;

    m_exportRequest = exportRequested;
    m_exportMiddleNode = exportMiddleNode;

    m_calculationUnit = calcUnit;
    m_log = log;
    m_calcUnitBBox = calcUnit == null ? null : CalcUnitOps.getBoundingBox( m_calculationUnit );

    m_restartNodes = restartNodes;

    // initialize Roughness IDs
    // TODO: Fishy!
    if( roughnessModel == null )
      m_roughnessIDProvider = null;
    else
    {
      m_roughnessIDProvider = new IdMap( roughnessModel.size() );
      for( final IRoughnessCls o : roughnessModel )
        m_roughnessIDProvider.getOrAdd( o.getGmlID() );
    }
  }

  /**
   * @return <code>0</code>, if feature is <code>null</code> or of unknown type.
   * @see org.kalypso.kalypsomodel1d2d.conv.INativeIDProvider#getConversionID(java.lang.String)
   */
  public int getConversionID( final IFeatureWrapper2 feature )
  {
    if( feature == null ) // TODO: this is probably an error in the data, throw an exception instead?
      return 0;

    final String id = feature.getGmlID();
    if( feature instanceof IFE1D2DNode )
      return m_nodesIDProvider.getOrAdd( id );

    if( feature instanceof IFE1D2DEdge )
      return m_edgesIDProvider.getOrAdd( id );

    if( feature instanceof IFE1D2DElement || feature instanceof IJunctionElement )
      return m_elementsIDProvider.getOrAdd( id );

    if( feature instanceof IFELine )
      return m_linesIDProvider.getOrAdd( id );

    if( feature instanceof IFE1D2DComplexElement )
      return m_complexElementsIDProvider.getOrAdd( id );

    if( feature instanceof IRoughnessCls )
      return m_roughnessIDProvider.getOrAdd( id );

    return 0;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.INativeIDProvider#getConversionID(java.lang.String)
   */
  public int getConversionID( final String featureGmlID )
  {
    if( m_nodesIDProvider.contains( featureGmlID ) )
      return m_nodesIDProvider.getOrAdd( featureGmlID );

    if( m_edgesIDProvider.contains( featureGmlID ) )
      return m_edgesIDProvider.getOrAdd( featureGmlID );

    if( m_elementsIDProvider.contains( featureGmlID ) )
      return m_elementsIDProvider.getOrAdd( featureGmlID );

    if( m_linesIDProvider.contains( featureGmlID ) )
      return m_linesIDProvider.getOrAdd( featureGmlID );

    if( m_complexElementsIDProvider.contains( featureGmlID ) )
      return m_complexElementsIDProvider.getOrAdd( featureGmlID );

    if( m_roughnessIDProvider.contains( featureGmlID ) )
      return m_roughnessIDProvider.getOrAdd( featureGmlID );

    return 0;
  }

  public void writeRMA10sModel( final File outputFile ) throws CoreException, IOException
  {
    Formatter formatter = null;
    try
    {
      // REMARK: Made a central formatter with US locale (causing decimal point to be '.'),
      // so no locale parameter for each format is needed any more .
      formatter = new Formatter( outputFile, Charset.defaultCharset().name(), Locale.US );
      writeRMA10sModel( formatter );
      FormatterUtils.checkIoException( formatter );
    }
    finally
    {
      if( formatter != null )
      {
        // REMARK: do not check io-exception here, else other exception would be hidden by this on
        formatter.close();
      }
    }
  }

  @SuppressWarnings("unchecked")
  private void writeRMA10sModel( final Formatter formatter ) throws CoreException, IOException
  {
    final IFeatureWrapperCollection<IFE1D2DElement> elements = m_discretisationModel1d2d.getElements();

    writeElementsNodesAndEdges( formatter, elements );

    final IFeatureWrapperCollection<IFE1D2DComplexElement> complexElements = m_discretisationModel1d2d.getComplexElements();

    // write transition elements
    if( m_calculationUnit instanceof ICalculationUnit1D2D )
    {
      for( final IFE1D2DComplexElement complexElement : complexElements )
      {
        if( complexElement instanceof ITransitionElement )
        {
          final ITransitionElement transitionElement = (ITransitionElement) complexElement;
          if( transitionElement.isMemberOfCalculationUnit( m_calculationUnit ) )
            writeTransitionLine( formatter, transitionElement );
        }
      }
    }

    // write junction elements
    for( final IFE1D2DComplexElement complexElement : complexElements )
    {
      if( complexElement instanceof IJunctionElement )
      {
        final IJunctionElement junctionElement = (IJunctionElement) complexElement;
        if( junctionElement.isMemberOfCalculationUnit( m_calculationUnit ) )
          writeJunctionElement( formatter, junctionElement );
      }
    }
  }

  private void writeJunctionElement( final Formatter formatter, final IJunctionElement junctionElement ) throws IOException
  {
    final int junctionElementID = getConversionID( junctionElement );
    final List<IFELine> continuityLines = junctionElement.getContinuityLines();
    formatter.format( "JE%10s", junctionElementID );
    for( final IFELine line : continuityLines )
    {
      formatter.format( "%10d", getConversionID( line.getNodes().get( 0 ) ) );
      FormatterUtils.checkIoException( formatter );
    }

    formatter.format( "%nFE%10d%10d%n", junctionElementID, 902 );// 901: water level, 902 energy head
    FormatterUtils.checkIoException( formatter );
  }

  @SuppressWarnings("unchecked")
  private void writeTransitionLine( final Formatter formatter, final ITransitionElement transitionElement ) throws CoreException, IOException
  {
    final int transitionElementID = getConversionID( transitionElement );
    final IContinuityLine1D line1D;
    final int node1D_ID;
    int element1D_ID = -1;
    final int line2D_ID;
    final List<IFELine> transitionElementContinuityLines = transitionElement.getContinuityLines();
    if( transitionElementContinuityLines.get( 0 ) instanceof IContinuityLine1D )
    {
      line1D = (IContinuityLine1D) transitionElementContinuityLines.get( 0 );
      line2D_ID = getConversionID( transitionElementContinuityLines.get( 1 ) );
    }
    else
    {
      line2D_ID = getConversionID( transitionElementContinuityLines.get( 0 ) );
      line1D = (IContinuityLine1D) transitionElementContinuityLines.get( 1 );
    }
    final IFE1D2DNode node1D = line1D.getNodes().get( 0 );
    node1D_ID = getConversionID( node1D );
    final IFeatureWrapperCollection<IFeatureWrapper2> containers = node1D.getContainers();
    for( final IFeatureWrapper2 container : containers )
    {
      if( element1D_ID != -1 )
        break;
      if( container instanceof IFE1D2DEdge )
      {
        final IFeatureWrapperCollection edgeContainers = ((IFE1D2DEdge) container).getContainers();
        for( final Object edgeContainer : edgeContainers )
        {
          if( edgeContainer instanceof IElement1D )
            if( m_calculationUnit.contains( (IElement1D) edgeContainer ) )
            {
              element1D_ID = getConversionID( (IElement1D) edgeContainer );
              break;
            }
        }
      }
    }
    if( element1D_ID == -1 || node1D_ID == -1 )
    {
      final GM_Object location = transitionElement.getLocation();
      final String message = "Fehler beim Schreiben einer 1D-2D Kopplung: kein 1D-Element gefunden.";
      final IGeoStatus status = m_log.log( IStatus.ERROR, ISimulation1D2DConstants.CODE_PRE, message, location, null );
      throw new CoreException( status );
    }
    if( line2D_ID == -1 )
    {
      final GM_Object location = transitionElement.getLocation();
      final String message = "Fehler beim Schreiben einer 1D-2D Kopplung: keine 2D-Linie gefunden.";
      final IGeoStatus status = m_log.log( IStatus.ERROR, ISimulation1D2DConstants.CODE_PRE, message, location, null );
      throw new CoreException( status );
    }

    final int transitionElementType;
    switch( transitionElement.getTransitionType() )
    {
      case TYPE2D1D:
        transitionElementType = 1;
        break;
      case TYPE1D2D:
      default:
        transitionElementType = 2;
        break;
    }

    formatter.format( "TL%10d%10d%10d%10d%10d%n", transitionElementID, element1D_ID, line2D_ID, node1D_ID, transitionElementType ); //$NON-NLS-1$
    FormatterUtils.checkIoException( formatter );
  }

  @SuppressWarnings("unchecked")
  private void writeEdgeSet( final Formatter formatter, final Collection<IFE1D2DEdge> edges ) throws IOException
  {
    int cnt = 1;
    for( final IFE1D2DEdge edge : edges )
    {
      final IFE1D2DNode node0 = edge.getNode( 0 );
      final int node0ID = getConversionID( node0 );
      final IFE1D2DNode node1 = edge.getNode( 1 );
      final int node1ID = getConversionID( node1 );

      /*
       * If we have no middle node (which is always the case), create it on the fly (just takes middle of edge). This is
       * needed for the restart approach later.
       */
      final int middleNodeID;
      if( !m_exportMiddleNode )
        middleNodeID = 0;
      else
      {
        if( edge.getMiddleNode() == null )
        {
          /* create virtual node id */
          final String gmlID = "VirtualMiddleNode" + edge.getGmlID(); // Pseudo id, but unique within this context
          middleNodeID = m_nodesIDProvider.getOrAdd( gmlID );

          /* Write it: Station is not needed, because the element length is taken from real nodes. */
          formatNode( formatter, middleNodeID, edge.getMiddleNodePoint(), null, true );
        }
        else
          middleNodeID = getConversionID( edge.getMiddleNode() );
      }

      /* Directly format into the string, this is quickest! */
      if( TypeInfo.is1DEdge( edge ) )
      {
        int leftRightID = 0;
        if( edge.getContainers().size() > 0 )
        {
          final Object object = edge.getContainers().get( 0 );

          if( object instanceof IElement1D )
            leftRightID = getConversionID( ((IElement1D) object) );
        }
        formatter.format( "AR%10d%10d%10d%10d%10d%10d%n", cnt++, node0ID, node1ID, leftRightID, leftRightID, middleNodeID ); //$NON-NLS-1$
      }
      else if( TypeInfo.is2DEdge( edge ) )
      {
        final IFeatureWrapperCollection<PolyElement> elements = edge.getAdjacentElements();

        final GM_Point point0 = node0.getPoint();
        final GM_Point point1 = node1.getPoint();

        final double x0 = point0.getX();
        final double y0 = point0.getY();
        final double vx0 = point1.getX() - x0;
        final double vy0 = point1.getY() - y0;

        IFE1D2DElement leftElement = null;
        IFE1D2DElement rightElement = null;
        // find left and right elements
        nextElement: for( final PolyElement element : elements )
        {
          final IFeatureWrapperCollection<IFE1D2DEdge> elementEdges = element.getEdges();
          // find node adjacent to node0 other than node1
          for( final IFE1D2DEdge elementEdge : elementEdges )
          {
            final IFeatureWrapperCollection<IFE1D2DNode> nodes = elementEdge.getNodes();
            final IFE1D2DNode node2;
            if( !elementEdge.equals( edge ) && nodes.contains( node0 ) )
            {
              if( nodes.get( 0 ).equals( node0 ) )
              {
                node2 = nodes.get( 1 );
              }
              else
              {
                node2 = nodes.get( 0 );
              }
              final GM_Point point2 = node2.getPoint();
              final double vx1 = point2.getX() - x0;
              final double vy1 = point2.getY() - y0;
              final double magnitude = vx0 * vy1 - vy0 * vx1;
              if( magnitude > 0 )
              {
                // positive cross product
                if( leftElement == null )
                  leftElement = element;
                else
                  System.out.println();
              }
              else
              {
                rightElement = element;
              }
              continue nextElement;
            }
          }
        }
        final int leftParent;
        final int rightParent;
        if( m_exportRequest )
        {
          leftParent = getConversionID( leftElement );
          rightParent = getConversionID( rightElement );
        }
        else
        {
          leftParent = m_calculationUnit.contains( leftElement ) ? getConversionID( leftElement ) : 0;
          rightParent = m_calculationUnit.contains( rightElement ) ? getConversionID( rightElement ) : 0;
        }
        formatter.format( "AR%10d%10d%10d%10d%10d%10d%n", cnt++, node0ID, node1ID, leftParent, rightParent, middleNodeID ); //$NON-NLS-1$
      }
      else
      {
        // stream.println( "************************************** non 1d/2d edge: " + edge.getGmlID() );
        System.out.println( "non 1d/2d edge: " + edge.getGmlID() ); //$NON-NLS-1$
      }
    }

    FormatterUtils.checkIoException( formatter );
  }

  @SuppressWarnings("unchecked")
  private void writeNodes( final Formatter formatter, final IFeatureWrapperCollection<IFE1D2DNode> nodes ) throws CoreException, IOException
  {
    final List<IFE1D2DNode> nodesInBBox = m_exportRequest ? nodes : nodes.query( m_calcUnitBBox );
    writeNodes( formatter, nodesInBBox );
  }

  @SuppressWarnings("unchecked")
  private void writeNodes( final Formatter formatter, final List<IFE1D2DNode> nodes ) throws CoreException, IOException
  {
    for( final IFE1D2DNode node : nodes )
    {
      // TODO: only write nodes, which are within the requested calculation unit!

      if( m_writtenNodesIDs.contains( node.getGmlID() ) )
        continue;

      // check if node elevation is assigned
      double z = Double.NaN;
      try
      {
        z = node.getPoint().getZ();
      }
      catch( final ArrayIndexOutOfBoundsException e )
      {
        // could happen that the node only has x and y coordinates
        // ignore now, case will be handled soon, z = Double.NaN;
      }
      if( Double.isNaN( z ) )
      {
        final GM_Point position = node.getPoint();
        final String msg = "Modellknoten ohne Höhe.";
        final IGeoStatus status = m_log.log( IStatus.ERROR, ISimulation1D2DConstants.CODE_PRE, msg, position, null );
        throw new CoreException( status );
      }

      m_writtenNodesIDs.add( node.getGmlID() );

      /* The node itself */
      final int nodeID = getConversionID( node );
      final GM_Point point = node.getPoint();
      BigDecimal station = null;

      if( DiscretisationModelUtils.is1DNode( node ) )
      {
        /* Node parameters */
        final Class<IFlowRelationshipModel>[] flowRelationTypes = new Class[] { IKingFlowRelation.class, ITeschkeFlowRelation.class };

        final double searchDistance = 0.1;
        final IFlowRelationship relationship = m_flowrelationModel.findFlowrelationship( point.getPosition(), searchDistance, flowRelationTypes );
        if( relationship == null )
        {
          final String msg = "Keine Knotenparameter für 1D-Knoten vorhanden";
          final GM_Object location = node.getPoint();
          final IGeoStatus status = m_log.log( IStatus.ERROR, ISimulation1D2DConstants.CODE_PRE, msg, location, null );
          throw new CoreException( status );
        }

        if( relationship instanceof IKingFlowRelation )
        {
          final IKingFlowRelation kingRelation = (IKingFlowRelation) relationship;
          final BigDecimal width = kingRelation.getWidth();
          final BigDecimal bankSlopeLeft = kingRelation.getBankSlopeLeft();
          final BigDecimal bankSlopeRight = kingRelation.getBankSlopeRight();
          final BigDecimal widthStorage = kingRelation.getWidthStorage();
          final BigDecimal heightStorage = kingRelation.getHeightStorage();
          final BigDecimal slopeStorage = kingRelation.getSlopeStorage();
          formatter.format( "CS%10d%10.1f%10.3f%10.3f%10.2f%10.2f%10.2f%n", nodeID, width, bankSlopeLeft, bankSlopeRight, widthStorage, heightStorage, slopeStorage ); //$NON-NLS-1$
        }
        else if( relationship instanceof ITeschkeFlowRelation )
        {
          final ITeschkeFlowRelation teschkeRelation = (ITeschkeFlowRelation) relationship;
          station = teschkeRelation.getStation();
          if( station == null )
            continue; // TODO: only for debug purpose, throw exception instead
          final List<IPolynomial1D> polynomials = teschkeRelation.getPolynomials();
          final TeschkeRelationConverter teschkeConv = new TeschkeRelationConverter( polynomials.toArray( new IPolynomial1D[] {} ) );
          final double slope = teschkeRelation.getSlope();
          final Double min = teschkeConv.getMin();
          final Double max = teschkeConv.getMax();

          formatter.format( "MM%10d%20.7f%20.7f%n", nodeID, min, max ); //$NON-NLS-1$

          final IPolynomial1D[] polyArea = teschkeConv.getPolynomialsByType( IWspmTuhhQIntervallConstants.DICT_PHENOMENON_AREA );
          if( polyArea == null )
          {
            final String msg = "Knotenparameter enthält kein Flächenpolynom.";
            final GM_Object location = node.getPoint();
            final IGeoStatus status = m_log.log( IStatus.ERROR, ISimulation1D2DConstants.CODE_PRE, msg, location, null );
            throw new CoreException( status );
          }
          writePolynomialRanges( formatter, "PRA", nodeID, min, polyArea );
          for( int j = 0; j < polyArea.length; j++ )
          {
            writeSplittedPolynomials( formatter, "AP ", nodeID, j, polyArea[j], null );
          }

          final IPolynomial1D[] polyRunoff = teschkeConv.getPolynomialsByType( IWspmTuhhQIntervallConstants.DICT_PHENOMENON_RUNOFF );
          if( polyRunoff == null )
          {
            final String msg = "Knotenparameter enthält kein Abflusspolynom.";
            final GM_Object location = node.getPoint();
            final IGeoStatus status = m_log.log( IStatus.ERROR, ISimulation1D2DConstants.CODE_PRE, msg, location, null );
            throw new CoreException( status );
          }

          writePolynomialRanges( formatter, "PRQ", nodeID, min, polyRunoff );
          for( int j = 0; j < polyRunoff.length; j++ )
          {
            writeSplittedPolynomials( formatter, "QP ", nodeID, j, polyRunoff[j], slope );
          }

          final IPolynomial1D[] polyAlpha = teschkeConv.getPolynomialsByType( IWspmTuhhQIntervallConstants.DICT_PHENOMENON_ALPHA );
          if( polyAlpha == null )
          {
            final String msg = "Knotenparameter enthält kein Alphapolynom.";
            final GM_Object location = node.getPoint();
            final IGeoStatus status = m_log.log( IStatus.ERROR, ISimulation1D2DConstants.CODE_PRE, msg, location, null );
            throw new CoreException( status );
          }

          writePolynomialRanges( formatter, "PRB", nodeID, min, polyAlpha );
          for( int j = 0; j < polyAlpha.length; j++ )
          {
            writeSplittedPolynomials( formatter, "ALP", nodeID, j, polyAlpha[j], null );
          }

        }
        else
        {
          final String msg = Messages.getString( "Gml2RMA10SConv.26" ) + relationship;
          final GM_Object location = node.getLocation();
          final IGeoStatus status = m_log.log( IStatus.ERROR, ISimulation1D2DConstants.CODE_PRE, msg, location, null );
          throw new CoreException( status );
        }
      }
      formatNode( formatter, nodeID, point, station, false );
      FormatterUtils.checkIoException( formatter );
    }
  }

  /**
   * @param formatter
   * @param nodeID
   *          the id of the node
   * @param point
   *          the geo point of the node
   * @param station
   *          the station of the node (only for 1d nodes, else null)
   * @param isMidside
   *          flag, if the node is a midside node (for writing the restart file)
   */
  private void formatNode( final Formatter formatter, final int nodeID, final GM_Point point, final BigDecimal station, final boolean isMidside ) throws IOException
  {
    /* Now really write the nodes */
    final double x = point.getX();
    final double y = point.getY();
    double z = Double.NaN;
    // TODO: Here we should decide what we do with non-elevation-assigned nodes. For now the elevation of these nodes
    // will be set to '-9999'
    // TODO: this cannot happen anymore (?) because an exception would have been thrown earlier
    if( point.getCoordinateDimension() == 3 )
      z = point.getZ();
    else
      z = -9999;

    if( station == null )
      formatter.format( "FP%10d%20.7f%20.7f%20.7f%n", nodeID, x, y, z ); //$NON-NLS-1$
    else
      formatter.format( "FP%10d%20.7f%20.7f%20.7f%20.7f%n", nodeID, x, y, z, station ); //$NON-NLS-1$

    writeRestartLines( formatter, nodeID, x, y, isMidside );
  }

  private void writeSplittedPolynomials( final Formatter formatter, final String kind, final int nodeID, final int polynomialNo, final IPolynomial1D poly, final Double extraValue ) throws IOException
  {
    formatter.format( "%3s%9d%3d", kind, nodeID, polynomialNo + 1 ); //$NON-NLS-1$

    if( extraValue != null )
      formatter.format( "%20.7f", extraValue ); //$NON-NLS-1$

    final double[] coefficients = poly.getCoefficients();
    for( final double coeff : coefficients )
    {
      formatter.format( "%20.7f", coeff ); //$NON-NLS-1$
    }

    formatter.format( "%n" ); //$NON-NLS-1$

    FormatterUtils.checkIoException( formatter );
  }

  private void writePolynomialRanges( final Formatter formatter, final String kind, final int nodeID, final double min, final IPolynomial1D[] poly ) throws IOException
  {
    formatter.format( "%3s%9d%3d%20.7f", kind, nodeID, poly.length, min );
    for( final IPolynomial1D element : poly )
    {
      formatter.format( "%20.7f", element.getRangeMax() );
      FormatterUtils.checkIoException( formatter );
    }
    formatter.format( "%n" ); //$NON-NLS-1$
    FormatterUtils.checkIoException( formatter );
  }

  /**
   * write elements nodes and edges in a way which avoids the filtering of edges and nodes
   */
  @SuppressWarnings("unchecked")
  private void writeElementsNodesAndEdges( final Formatter formatter, final IFeatureWrapperCollection<IFE1D2DElement> elements ) throws CoreException, IOException
  {
    final List<IFE1D2DElement> elementsInBBox = m_exportRequest ? elements : elements.query( m_calcUnitBBox );
    final Set<IFE1D2DEdge> edgeSet = new LinkedHashSet<IFE1D2DEdge>( elementsInBBox.size() * 2 );

    if( elementsInBBox.size() == 0 )
    {
      final String msg = "Das Modell enthält keine Elemente. Berechnung nicht möglich.";
      final IGeoStatus status = m_log.log( IStatus.ERROR, ISimulation1D2DConstants.CODE_PRE, msg, null, null );
      throw new CoreException( status );
    }

    for( final IFE1D2DElement element : elementsInBBox )
    {
      // TODO: shouldn't the check for calculation unit always happens? -> So export is per calculation unit?
      if( !m_exportRequest && !m_calculationUnit.contains( element ) )
        continue;

      // TODO: has nothing to do with export request; make special flag which kinds of elements should get exported
      if( m_exportRequest && element instanceof IElement1D )
        continue;

      final int id = getConversionID( element );

      if( element instanceof IElement1D )
      {
        final IFE1D2DEdge edge = ((IElement1D) element).getEdge();
        edgeSet.add( edge );

        /* 1D-Elements get special handling. */
        final IElement1D element1D = (IElement1D) element;

        final IBuildingFlowRelation building = FlowRelationUtilitites.findBuildingElement1D( element1D, m_flowrelationModel );
        if( building != null )
        {
          /* A Building? Create dynamic building number and use it as building ID. */
          final int buildingID = m_buildingIDProvider.addBuilding( building );
          final IFE1D2DNode upstreamNode = FlowRelationUtilitites.findUpstreamNode( building, m_discretisationModel1d2d );
          final int upstreamNodeID = getConversionID( upstreamNode );
          formatter.format( "FE%10d%10d%10s%10s%10d%n", id, buildingID, "", "", upstreamNodeID ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
        }
        else if( FlowRelationUtilitites.isTeschkeElement1D( element1D, m_flowrelationModel ) )
        {
          /* Element without building: The special roughness-class '89' should be used. */
          formatter.format( "FE%10d%10d%n", id, 89 ); //$NON-NLS-1$
        }
        else
        {
          // TODO: give hint what 1D-element is was?
          final String msg = Messages.getString( "Gml2RMA10SConv.43" ) + element1D.getGmlID();
          final IGeoStatus status = m_log.log( IStatus.ERROR, ISimulation1D2DConstants.CODE_PRE, msg, null, null );
          throw new CoreException( status );
        }

        // TODO: find 1D-calc unit in which this element resides
        // TODO write new lp line
        final ICalculationUnit1D calcUnit1D = find1dCalcUnit( m_calculationUnit, element1D );
        if( calcUnit1D != null && building == null )
        {
          final int interpolationCount = calcUnit1D.getInterpolationCount();
          formatter.format( "IP%10d%10d%n", id, interpolationCount );
        }
      }
      else if( element instanceof IPolyElement )
      {
        for( final IFE1D2DEdge edge : ((IPolyElement<IFE1D2DComplexElement, IFE1D2DEdge>) element).getEdges() )
        {
          edgeSet.add( edge );
        }

        final int roughnessID = m_roughnessIDProvider == null ? 0 : getRoughnessID( element );
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
    FormatterUtils.checkIoException( formatter );

    // write edge set nodes
    for( final IFE1D2DEdge edge : edgeSet )
    {
      writeNodes( formatter, edge.getNodes() );
      if( m_exportMiddleNode )
      {
        final IFE1D2DNode middleNode = edge.getMiddleNode();
        if( middleNode != null )
        {
          final List<IFE1D2DNode> list = new ArrayList<IFE1D2DNode>();
          list.add( middleNode );
          writeNodes( formatter, list );
        }
      }
    }

    // write edges
    writeEdgeSet( formatter, edgeSet );
  }

  /**
   * Finds the first 1D-Calculation unit which contains the given element.<br>
   * Recursively searches within sub-units of 1d2d units.
   */
  @SuppressWarnings("unchecked")
  private static ICalculationUnit1D find1dCalcUnit( final ICalculationUnit calcUnit, final IElement1D element1D )
  {
    if( calcUnit == null )
      return null;

    if( calcUnit instanceof ICalculationUnit1D )
    {
      // TODO: check, if this is not too slow
      if( calcUnit.contains( element1D ) )
        return (ICalculationUnit1D) calcUnit;

      return null;
    }

    /* This should never happen... */
    if( calcUnit instanceof ICalculationUnit2D )
      return null;

    if( calcUnit instanceof ICalculationUnit1D2D )
    {
      final ICalculationUnit1D2D calcUnit1d2d = (ICalculationUnit1D2D) calcUnit;
      final List<ICalculationUnit> subUnits = calcUnit1d2d.getChangedSubUnits();
      for( final ICalculationUnit calculationUnit : subUnits )
      {
        final ICalculationUnit1D foundUnit = find1dCalcUnit( calculationUnit, element1D );
        if( foundUnit != null )
          return foundUnit;
      }

      return null;
    }

    throw new UnsupportedOperationException();
  }

  private void writeRestartLines( final Formatter formatter, final int nodeID, final double x, final double y, final boolean isMidside ) throws IOException
  {
    if( m_restartNodes == null )
      return;

    if( m_restartNodes.getSize() == 0 )
      return;

    final INodeResult node = m_restartNodes.getNodeResultAtPosition( x, y );
    if( node == null )
    {
      // we check only corner nodes, because in 1d it could be the case that there are midside nodes without restart
      // data
      if( !isMidside )
      {
        final GM_Point position = GeometryFactory.createGM_Point( x, y, KalypsoDeegreePlugin.getDefault().getCoordinateSystem() );
        m_log.log( IStatus.WARNING, ISimulation1D2DConstants.CODE_PRE, "Keine Restartwerte für Modellknoten gefunden.", position, null );
      }
      return;
    }

    // ---------------------------
    // now write the restart lines
    // ---------------------------
    double velXComp = 0;
    double velYComp = 0;
    List<Double> velTotal = new ArrayList<Double>();
    double virtDepth = 0;
    final List<LINE_CASES> restartCases = new ArrayList<LINE_CASES>();

    restartCases.add( LINE_CASES.VA );
    restartCases.add( LINE_CASES.GA );
    restartCases.add( LINE_CASES.VO );
    restartCases.add( LINE_CASES.GO );

    // --------------------------------------------------------------------------------------------------------
    // Write the velocities and the depth values in the VA-line; if RESTART should be done, this is the minimum
    // information, i.e.
    // enough information for restart with steady state, but not enough for unsteady calculations.
    // -------------------------------------------------------------------------------------------
    for( final LINE_CASES restartCase : restartCases )
    {

      switch( restartCase )
      {
        case VA:
          velTotal = node.getVirtualVelocity();
          virtDepth = node.getVirtualDepth();
          break;
        case GA:
          velTotal = node.getVelOverTime();
          virtDepth = node.getVirtDepOverTime();
          break;
        case VO:
          velTotal = node.getVelPrevStep();
          virtDepth = node.getVirtDepPrevStep();
          break;
        case GO:
          velTotal = node.getVelOverTimePrevStep();
          virtDepth = node.getVirtDepOverTimePrevStep();
          break;
      }

      // no values in feature present at all (old models)
      if( velTotal == null )
      {
        velXComp = 0.0;
        velYComp = 0.0;
      }
      else
      {
        if( velTotal.get( 0 ) == null )
        {
          velXComp = 0.0;
          velYComp = 0.0;
        }
        // This is the situation we want to have in the unsteady restarting.
        else
        {
          velXComp = velTotal.get( 0 );
          velYComp = velTotal.get( 1 );
        }
      }

      switch( restartCase )
      {
        case VA:
          // --------------------------------------------------------------
          // Write the velocities of the current time step into the VA line
          // --------------------------------------------------------------
          formatter.format( "VA%10d%20.7f%20.7f%20.7f%20.7f%n", nodeID, velXComp, velYComp, virtDepth, node.getWaterlevel() ); //$NON-NLS-1$
          FormatterUtils.checkIoException( formatter );
          break;
        case GA:
          // -------------------------------------------------------------------------------
          // Write the gradients of the velocities of the current time step into the GA line
          // -------------------------------------------------------------------------------
          formatter.format( "GA%10d%20.7f%20.7f%20.7f%n", nodeID, velXComp, velYComp, virtDepth ); //$NON-NLS-1$
          FormatterUtils.checkIoException( formatter );
          break;
        case VO:
          // ---------------------------------------------------------------
          // Write the velocities of the previous time step into the VO line
          // ---------------------------------------------------------------
          formatter.format( "VO%10d%20.7f%20.7f%20.7f%n", nodeID, velXComp, velYComp, virtDepth ); //$NON-NLS-1$
          FormatterUtils.checkIoException( formatter );
          break;
        case GO:
          // --------------------------------------------------------------------------------
          // Write the gradients of the velocities of the previous time step into the GO line
          // --------------------------------------------------------------------------------
          formatter.format( "GO%10d%20.7f%20.7f%20.7f%n", nodeID, velXComp, velYComp, virtDepth ); //$NON-NLS-1$
          FormatterUtils.checkIoException( formatter );
          break;
      }

    }
  }

  @SuppressWarnings("unchecked")
  private int getRoughnessID( final IFE1D2DElement element )
  {
    final String roughnessClsID = element.getRoughnessClsID();
    if( roughnessClsID != null && roughnessClsID.length() > 0 )
      return m_roughnessIDProvider.getOrAdd( roughnessClsID );

    // TODO: use default zone instead.
    // Right now it is set to '0' which means the element is deactivated for the simulation
    final String msg = String.format( "Element '%s' hat keine Rauheitszone zugewiesen und wird für die Rechnung deaktiviert.", element.getGmlID() );

    final IFE1D2DNode node = (IFE1D2DNode) element.getNodes().get( 0 );
    final GM_Point point = node.getPoint();
    m_log.log( IStatus.WARNING, ISimulation1D2DConstants.CODE_PRE, msg, point, null );

    return 0;
  }

  public BuildingIDProvider getBuildingProvider( )
  {
    return m_buildingIDProvider;
  }

}
