/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.math.BigDecimal;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Formatter;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.kalypso.contribs.java.util.FormatterUtils;
import org.kalypso.kalypsomodel1d2d.conv.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.conv.results.RestartNodes;
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
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IFlowRelation2D;
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
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.gml.binding.commons.IGeoStatus;
import org.kalypsodeegree_impl.gml.binding.math.IPolynomial1D;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * Converts discretisation model to RMA�Kalypso model
 * 
 * @author Dejan Antanaskovic, <a href="mailto:dejan.antanaskovic@tuhh.de">dejan.antanaskovic@tuhh.de</a>
 * @author ig
 */
@SuppressWarnings("unchecked")
public class Gml2RMA10SConv implements INativeIDProvider, I2DMeshConverter
{
  private static enum LINE_CASES
  {
    VA,
    GA,
    VO,
    GO
  }

  public static final boolean SUPPORT_MIDSIDE_NODES = true;

  public static final boolean SUPPORT_FLOW_RESISTANCE_CLASSES = true;

  private static String WEIR2D_CONST_ID = "_2D_WEIR2RMA10_ID_"; //$NON-NLS-1$

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

  private final RestartNodes m_restartNodes;

  private final boolean m_exportRequest;

  private final boolean m_exportMiddleNode;

  private final IGeoLog m_log;

  private final List<PseudoEdge> m_listNonExistingPseudoEdges = new ArrayList<PseudoEdge>();

  private final Map<Integer, List<PseudoEdge>> m_mapPolyWeir2DSubElement = new HashMap<Integer, List<PseudoEdge>>();

  private final Map<Integer, String> m_mapTmpElementToPolyWeir = new HashMap<Integer, String>();

  private final Map<IPolyElement, IFlowRelation2D> m_mapPolyElementsWithWeir = new HashMap<IPolyElement, IFlowRelation2D>();

  // TODO: check: calculation?
  public Gml2RMA10SConv( final IFEDiscretisationModel1d2d discretisationModel1d2d, final IFlowRelationshipModel flowrelationModel, final ICalculationUnit calcUnit, final IRoughnessClsCollection roughnessModel, final RestartNodes restartNodes, final boolean exportRequested, final boolean exportMiddleNode, final IGeoLog log )
  {
    m_discretisationModel1d2d = (discretisationModel1d2d);
    m_flowrelationModel = flowrelationModel;

    m_exportRequest = exportRequested;
    m_exportMiddleNode = exportMiddleNode;

    m_calculationUnit = calcUnit;
    m_log = log;
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

    // collect information about 2d buildings to perform this mapping fast on demand
    for( final IFlowRelationship relationship : flowrelationModel )
    {
      if( relationship instanceof IFlowRelation2D )
      {
        final IFlowRelation2D lBuilding2d = (IFlowRelation2D) relationship;
        final IPolyElement lPolyElementWithWeir = m_discretisationModel1d2d.find2DElement( lBuilding2d.getPosition(), 0.01 );
        if( m_calculationUnit.contains( lPolyElementWithWeir ) )
        {
          m_mapPolyElementsWithWeir.put( lPolyElementWithWeir, lBuilding2d );
        }
      }
    }
    // m_intBuildingsIdCounter = 1;
  }

  @Override
  public int getConversionID( final IFeatureWrapper2 feature )
  {
    return getConversionID( feature, null );
  }

  /**
   * @return <code>0</code>, if feature is <code>null</code> or of unknown type.
   * @see org.kalypso.kalypsomodel1d2d.conv.INativeIDProvider#getConversionID(java.lang.String)
   */
  public int getConversionID( final IFeatureWrapper2 feature, final String pGMLId )
  {
    if( feature == null ) // TODO: this is probably an error in the data, throw an exception instead?
      return 0;

    String id = feature.getGmlID();
    if( pGMLId != null )
      id = pGMLId;
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
  @Override
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

  @Override
  public void writeMesh( final File file ) throws IOException
  {
    OutputStream outputStream = null;
    try
    {
      outputStream = new FileOutputStream( file );
      writeRMA10sModel( outputStream );
    }
    finally
    {
      if( outputStream != null )
      {
        // REMARK: do not check io-exception here, else other exception would be hidden by this on
        outputStream.close();
      }
    }
  }

  public void writeRMA10sModel( final OutputStream outputStream) throws IOException
  {
    Formatter formatter = null;
    try
    { 
      // REMARK: Made a central formatter with US locale (causing decimal point to be '.'),
      // so no locale parameter for each format is needed any more .
      formatter = new Formatter( outputStream, Charset.defaultCharset().name(), Locale.US );
      writeRMA10sModel( formatter );
      FormatterUtils.checkIoException( formatter );
    }
    catch (final Exception e) {
      e.printStackTrace();
      throw new IOException();
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


  private void writeRMA10sModel( final Formatter formatter ) throws CoreException, IOException
  {
    // we dont need all elements to check each one for membership in calculation unit.
    // we get it direct from the calculation unit
    // final IFeatureWrapperCollection<IFE1D2DElement> elements = m_discretisationModel1d2d.getElements();

    writeElementsNodesAndEdges( formatter );

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
    formatter.format( "JE%10s", junctionElementID ); //$NON-NLS-1$
    for( final IFELine line : continuityLines )
    {
      formatter.format( "%10d", getConversionID( line.getNodes().get( 0 ) ) ); //$NON-NLS-1$
      FormatterUtils.checkIoException( formatter );
    }

    formatter.format( "%nFE%10d%10d%n", junctionElementID, 902 );// 901: water level, 902 energy head //$NON-NLS-1$
    FormatterUtils.checkIoException( formatter );
  }

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
      final String message = org.kalypso.kalypsomodel1d2d.conv.i18n.Messages.getString( "org.kalypso.kalypsomodel1d2d.conv.Gml2RMA10SConv.6" ); //$NON-NLS-1$
      final IGeoStatus status = m_log.log( IStatus.ERROR, ISimulation1D2DConstants.CODE_PRE, message, location, null );
      throw new CoreException( status );
    }
    if( line2D_ID == -1 )
    {
      final GM_Object location = transitionElement.getLocation();
      final String message = org.kalypso.kalypsomodel1d2d.conv.i18n.Messages.getString( "org.kalypso.kalypsomodel1d2d.conv.Gml2RMA10SConv.7" ); //$NON-NLS-1$
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
          middleNodeID = writeMiddleNode( edge.getGmlID(), edge.getMiddleNodePoint(), formatter );
        }
        else
        {
          middleNodeID = getConversionID( edge.getMiddleNode() );
        }
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
          IFE1D2DNode node2 = null;
          if( m_mapTmpElementToPolyWeir.containsValue( element.getGmlID() ) )
          {
            node2 = getAdjacentPseudoNode( element, edge );
          }

          for( final IFE1D2DEdge elementEdge : elementEdges )
          {
            final IFeatureWrapperCollection<IFE1D2DNode> nodes = elementEdge.getNodes();
            if( !elementEdge.equals( edge ) && nodes.contains( node0 ) || node2 != null )
            {
              if( node2 == null )
              {
                if( nodes.get( 0 ).equals( node0 ) )
                {
                  node2 = nodes.get( 1 );
                }
                else
                {
                  node2 = nodes.get( 0 );
                }
              }
              final GM_Point point2 = node2.getPoint();
              final double vx1 = point2.getX() - x0;
              final double vy1 = point2.getY() - y0;
              final double magnitude = vx0 * vy1 - vy0 * vx1;
              if( magnitude > 0 )
              {
                // positive cross product
                if( leftElement == null )
                {
                  leftElement = element;
                }
                else
                {
                  //                  System.out.println();
                }
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
          leftParent = getConversionIDIntern( leftElement, edge );
          rightParent = getConversionIDIntern( rightElement, edge );
        }
        else
        {
          leftParent = m_calculationUnit.contains( leftElement ) ? getConversionIDIntern( leftElement, edge ) : 0;
          rightParent = m_calculationUnit.contains( rightElement ) ? getConversionIDIntern( rightElement, edge ) : 0;
        }
        formatter.format( "AR%10d%10d%10d%10d%10d%10d%n", cnt++, node0ID, node1ID, leftParent, rightParent, middleNodeID ); //$NON-NLS-1$
      }
      else
      {
        // stream.println( "************************************** non 1d/2d edge: " + edge.getGmlID() );
        System.out.println( "non 1d/2d edge: " + edge.getGmlID() ); //$NON-NLS-1$
      }
    }
    writeNonExistingPseudoEdges( cnt, formatter );

    FormatterUtils.checkIoException( formatter );
  }

  /**
   * formats the middle node and returns the id of written node
   */
  private int writeMiddleNode( final String pStrEdgeGmlId, final GM_Point pMiddlePoint, final Formatter pFormater ) throws IOException
  {
    int lMiddleNodeID;

    /* create virtual node id */
    final String gmlID = "VirtualMiddleNode" + pStrEdgeGmlId; // Pseudo id, but unique within this context //$NON-NLS-1$
    lMiddleNodeID = m_nodesIDProvider.getOrAdd( gmlID );

    /* Write it: Station is not needed, because the element length is taken from real nodes. */
    formatNode( pFormater, lMiddleNodeID, pMiddlePoint, null, true );

    return lMiddleNodeID;
  }

  private void writeNonExistingPseudoEdges( int pIntCount, final Formatter pFormater ) throws IOException
  {
    for( final PseudoEdge lIterEdge : m_listNonExistingPseudoEdges )
    {
      final int node0ID = getConversionID( lIterEdge.getFirstNode() );
      final int node1ID = getConversionID( lIterEdge.getSecondNode() );
      final int lIntMiddleNodeId = writeMiddleNode( lIterEdge.getFirstNode().getGmlID() + lIterEdge.getSecondNode().getGmlID(), lIterEdge.getMiddleNodePoint(), pFormater );
      pFormater.format( "AR%10d%10d%10d%10d%10d%10d%n", pIntCount++, node1ID, node0ID, lIterEdge.getIntLeftParent(), lIterEdge.getIntRightParent(), lIntMiddleNodeId ); //$NON-NLS-1$
    }
  }

  /**
   * for pseudo edges determine the according id of pseudo element or in others cases returns the conversion id of given
   * element
   */
  private int getConversionIDIntern( final IFE1D2DElement pElement, final IFE1D2DEdge pEdge )
  {
    if( pElement == null )
      return 0;

    if( m_mapTmpElementToPolyWeir.containsValue( pElement.getGmlID() ) )
    {
      final IFeatureWrapperCollection<IFE1D2DNode> lNodesListFromGivenEdge = pEdge.getNodes();
      for( final Map.Entry<Integer, List<PseudoEdge>> lIterPseudoEntry : m_mapPolyWeir2DSubElement.entrySet() )
      {
        final List<PseudoEdge> lListEdges = lIterPseudoEntry.getValue();
        for( final PseudoEdge lPseudoEdge : lListEdges )
        {
          if( lNodesListFromGivenEdge.contains( lPseudoEdge.getFirstNode() ) && lNodesListFromGivenEdge.contains( lPseudoEdge.getSecondNode() )
              && pElement.getGmlID().equals( lPseudoEdge.getStrGMLParentId() ) )
          {
            return lPseudoEdge.getIntParentId();
          }
        }
      }
    }
    else
    {
      return getConversionID( pElement );
    }

    return 0;
  }

  private IFE1D2DNode getAdjacentPseudoNode( final IFE1D2DElement pElement, final IFE1D2DEdge pEdge )
  {
    if( m_mapTmpElementToPolyWeir.containsValue( pElement.getGmlID() ) )
    {
      for( final PseudoEdge lIterEdge : m_listNonExistingPseudoEdges )
      {
        if( lIterEdge.getStrGMLParentId().equals( pElement.getGmlID() ) )
        {
          if( lIterEdge.getSecondNode().equals( pEdge.getNode( 0 ) ) )
          {
            return lIterEdge.getFirstNode();
          }
          if( lIterEdge.getFirstNode().equals( pEdge.getNode( 0 ) ) )
          {
            return lIterEdge.getSecondNode();
          }
        }
      }
    }
    return null;
  }

  private int getDirectionOfPseudoEdges( final List<IFE1D2DNode> pListNodes )
  {
    int lIntResDirection = -1;
    double lDoubleResAngle = 0;

    GM_Point lPointStart = null;
    GM_Point lPointPrev = null;
    GM_Point lPointAct = null;
    for( int lIntCounter = 0; lIntCounter < pListNodes.size(); ++lIntCounter )
    {
      final IFE1D2DNode lActNode = pListNodes.get( lIntCounter );// ( IFE1D2DNode )lIterNode;
      if( lIntCounter == 0 )
      {
        lPointStart = lActNode.getPoint();
      }
      else if( lIntCounter > 1 && !lPointStart.equals( lPointAct ) )
      {
        lPointAct = lActNode.getPoint();
        final double lDoubleAngleInBetween = Math.atan2( lActNode.getPoint().getY() - lPointStart.getY(), lActNode.getPoint().getX() - lPointStart.getX() )
        - Math.atan2( lPointPrev.getY() - lPointStart.getY(), lPointPrev.getX() - lPointStart.getX() );

        lDoubleResAngle += lDoubleAngleInBetween;
      }
      lPointPrev = lActNode.getPoint();
    }
    if( lDoubleResAngle > 0 )
    {
      lIntResDirection = 1;
    }
    else
    {
      lIntResDirection = 0;
    }
    return lIntResDirection;
  }

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
        final String msg = org.kalypso.kalypsomodel1d2d.conv.i18n.Messages.getString( "org.kalypso.kalypsomodel1d2d.conv.Gml2RMA10SConv.9" ); //$NON-NLS-1$
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
          final String msg = org.kalypso.kalypsomodel1d2d.conv.i18n.Messages.getString( "org.kalypso.kalypsomodel1d2d.conv.Gml2RMA10SConv.12" ); //$NON-NLS-1$
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
            final String msg = org.kalypso.kalypsomodel1d2d.conv.i18n.Messages.getString( "org.kalypso.kalypsomodel1d2d.conv.Gml2RMA10SConv.13" ); //$NON-NLS-1$
            final GM_Object location = node.getPoint();
            final IGeoStatus status = m_log.log( IStatus.ERROR, ISimulation1D2DConstants.CODE_PRE, msg, location, null );
            throw new CoreException( status );
          }
          writePolynomialRanges( formatter, "PRA", nodeID, min, polyArea ); //$NON-NLS-1$
          for( int j = 0; j < polyArea.length; j++ )
          {
            writeSplittedPolynomials( formatter, "AP ", nodeID, j, polyArea[j], null ); //$NON-NLS-1$
          }

          final IPolynomial1D[] polyRunoff = teschkeConv.getPolynomialsByType( IWspmTuhhQIntervallConstants.DICT_PHENOMENON_RUNOFF );
          if( polyRunoff == null )
          {
            final String msg = org.kalypso.kalypsomodel1d2d.conv.i18n.Messages.getString( "org.kalypso.kalypsomodel1d2d.conv.Gml2RMA10SConv.16" ); //$NON-NLS-1$
            final GM_Object location = node.getPoint();
            final IGeoStatus status = m_log.log( IStatus.ERROR, ISimulation1D2DConstants.CODE_PRE, msg, location, null );
            throw new CoreException( status );
          }

          writePolynomialRanges( formatter, "PRQ", nodeID, min, polyRunoff ); //$NON-NLS-1$
          for( int j = 0; j < polyRunoff.length; j++ )
          {
            writeSplittedPolynomials( formatter, "QP ", nodeID, j, polyRunoff[j], slope ); //$NON-NLS-1$
          }

          final IPolynomial1D[] polyAlpha = teschkeConv.getPolynomialsByType( IWspmTuhhQIntervallConstants.DICT_PHENOMENON_ALPHA );
          if( polyAlpha == null )
          {
            final String msg = org.kalypso.kalypsomodel1d2d.conv.i18n.Messages.getString( "org.kalypso.kalypsomodel1d2d.conv.Gml2RMA10SConv.19" ); //$NON-NLS-1$
            final GM_Object location = node.getPoint();
            final IGeoStatus status = m_log.log( IStatus.ERROR, ISimulation1D2DConstants.CODE_PRE, msg, location, null );
            throw new CoreException( status );
          }

          writePolynomialRanges( formatter, "PRB", nodeID, min, polyAlpha ); //$NON-NLS-1$
          for( int j = 0; j < polyAlpha.length; j++ )
          {
            writeSplittedPolynomials( formatter, "ALP", nodeID, j, polyAlpha[j], null ); //$NON-NLS-1$
          }

        }
        else
        {
          final String msg = Messages.getString( "org.kalypso.kalypsomodel1d2d.conv.Gml2RMA10SConv.26", relationship );//$NON-NLS-1$
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

    writeRestartLines( formatter, nodeID, point, isMidside );
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
    formatter.format( "%3s%9d%3d%20.7f", kind, nodeID, poly.length, min ); //$NON-NLS-1$
    for( final IPolynomial1D element : poly )
    {
      formatter.format( "%20.7f", element.getRangeMax() ); //$NON-NLS-1$
      FormatterUtils.checkIoException( formatter );
    }
    formatter.format( "%n" ); //$NON-NLS-1$
    FormatterUtils.checkIoException( formatter );
  }

  /**
   * write elements nodes and edges in a way which avoids the filtering of edges and nodes
   */
  private void writeElementsNodesAndEdges( final Formatter formatter ) throws CoreException, IOException
  {
    // it is not needed to query for elements in box of calculation unit - calc. unit contains already only this needed
    // elements.
    // for the export case we also do not need the "box", we will just export all of the elements from the model.
    // additional check for membership of calculation unit for each element in the loop, was also removed.

    final List<IFE1D2DElement> elementsInBBox = m_discretisationModel1d2d.getElements();

    final List<IFE1D2DElement> lListAllElements = new ArrayList<IFE1D2DElement>();
    lListAllElements.addAll( elementsInBBox );

    // better way, but creates some errors because of the order of elements in output
    // TODO: check the reordering problem
    // if( m_exportRequest ){
    // lListAllElements = m_discretisationModel1d2d.getElements();
    // }
    // else{
    // final List<IElement1D> elements1dInBBox = m_calculationUnit.getElements1D();
    // final List<IPolyElement> elements2dInBBox = m_calculationUnit.getElements2D();
    // lListAllElements.addAll( elements2dInBBox );
    // lListAllElements.addAll( elements1dInBBox );
    // }
    final Set<IFE1D2DEdge> edgeSet = new LinkedHashSet<IFE1D2DEdge>( lListAllElements.size() * 2 );

    if( lListAllElements.size() == 0 )
    {
      final String msg = org.kalypso.kalypsomodel1d2d.conv.i18n.Messages.getString( "org.kalypso.kalypsomodel1d2d.conv.Gml2RMA10SConv.25" ); //$NON-NLS-1$
      final IGeoStatus status = m_log.log( IStatus.ERROR, ISimulation1D2DConstants.CODE_PRE, msg, null, null );
      throw new CoreException( status );
    }

    for( final IFE1D2DElement element : lListAllElements )
    {
      // TODO: shouldn't the check for calculation unit always happens? -> So export is per calculation unit?
      if( !m_exportRequest && !m_calculationUnit.contains( element ) )
        continue;

      // TODO: has nothing to do with export request; make special flag which kinds of elements should get exported
      if( m_exportRequest && element instanceof IElement1D )
        continue;

      int id = getConversionID( element );

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
          final String msg = Messages.getString( "org.kalypso.kalypsomodel1d2d.conv.Gml2RMA10SConv.43", element1D.getGmlID() );//$NON-NLS-1$
          final IGeoStatus status = m_log.log( IStatus.ERROR, ISimulation1D2DConstants.CODE_PRE, msg, null, null );
          throw new CoreException( status );
        }

        // TODO: find 1D-calc unit in which this element resides
        // TODO write new lp line
        final ICalculationUnit1D calcUnit1D = find1dCalcUnit( m_calculationUnit, element1D );
        if( calcUnit1D != null && building == null )
        {
          final int interpolationCount = calcUnit1D.getInterpolationCount();
          formatter.format( "IP%10d%10d%n", id, interpolationCount ); //$NON-NLS-1$
        }
      }
      else if( element instanceof IPolyElement )
      {
        final IFlowRelationship building = m_mapPolyElementsWithWeir.get( element );

        if( building != null )
        {
          final int buildingID = m_buildingIDProvider.addBuilding( building );

          if( element.getNodes().size() > 4 )
          {
            final List<IFE1D2DNode> lListNodes = getOrderedListOfNodes( element );

            final int lIntDirectionOfEdges = getDirectionOfPseudoEdges( lListNodes );
            final int lListElementNodesSize = lListNodes.size();
            final int upstreamNodePositionInEachElement = FlowRelationUtilitites.findUpstreamNodePolyWeirPositionInNodesRing( building, lListNodes, lIntDirectionOfEdges );

            int lIntLastId = 0;
            for( int lIntIter = 0; lIntIter < lListElementNodesSize / 2 - 1; ++lIntIter )
            {
              final List<PseudoEdge> lListEdges = new ArrayList<PseudoEdge>();
              boolean lBoolLastEdgeExists = false;
              if( lIntIter > 0 )
              {
                id = getConversionID( element, element.getGmlID() + WEIR2D_CONST_ID + buildingID + "_" + lIntIter ); //$NON-NLS-1$
              }
              else
              {
                lBoolLastEdgeExists = true;
              }
              final PseudoEdge lPseudoEdge0 = new PseudoEdge( (lListNodes.get( lIntIter )), (lListNodes.get( lIntIter + 1 )), id, element.getGmlID(), true );
              final PseudoEdge lPseudoEdge1 = new PseudoEdge( (lListNodes.get( lIntIter + 1 )), (lListNodes.get( lListElementNodesSize - (lIntIter + 3) )), id, element.getGmlID(), true );

              final PseudoEdge lPseudoEdge2 = new PseudoEdge( (lListNodes.get( lListElementNodesSize - (lIntIter + 3) )), (lListNodes.get( lListElementNodesSize - (lIntIter + 2) )), id, element.getGmlID(), true );
              final PseudoEdge lPseudoEdge3 = new PseudoEdge( (lListNodes.get( lListElementNodesSize - (lIntIter + 2) )), (lListNodes.get( lIntIter )), id, element.getGmlID(), lBoolLastEdgeExists );

              if( lIntIter > 0 )
              {
                lPseudoEdge3.setIntLeftParent( id );
                lPseudoEdge3.setIntRightParent( lIntLastId );
                m_listNonExistingPseudoEdges.add( lPseudoEdge3 );
              }

              lListEdges.add( lPseudoEdge0 );
              lListEdges.add( lPseudoEdge1 );
              lListEdges.add( lPseudoEdge2 );
              lListEdges.add( lPseudoEdge3 );
              lIntLastId = id;
              m_mapTmpElementToPolyWeir.put( id, element.getGmlID() );
              m_mapPolyWeir2DSubElement.put( id, lListEdges );
              final int upstreamNodeID = getConversionID( lListEdges.get( upstreamNodePositionInEachElement ).getFirstNode() );
              formatter.format( "FE%10d%10d%10s%10s%10d%n", id, buildingID, "", "", upstreamNodeID ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

            }
            for( final IFE1D2DEdge edge : ((IPolyElement<IFE1D2DComplexElement, IFE1D2DEdge>) element).getEdges() )
            {
              edgeSet.add( edge );
            }
          }
          /* A Building? Create dynamic building number and use it as building ID. */
          // final IFE1D2DNode upstreamNode = FlowRelationUtilitites.findUpstreamNode( building,
          // m_discretisationModel1d2d );
          // final int upstreamNodeID = getConversionID( upstreamNode );
          // final int roughnessID = m_roughnessIDProvider == null ? 0 : getRoughnessID( element );
          //          formatter.format( "FE%10d%10d%n", id, roughnessID ); //$NON-NLS-1$
        }

        else
        {
          for( final IFE1D2DEdge edge : ((IPolyElement<IFE1D2DComplexElement, IFE1D2DEdge>) element).getEdges() )
          {
            edgeSet.add( edge );
          }

          final int roughnessID = m_roughnessIDProvider == null ? 0 : getRoughnessID( element );
          formatter.format( "FE%10d%10d%n", id, roughnessID ); //$NON-NLS-1$

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
            formatter.format( "RC%10d%10.6f%10.6f%10.6f%n", id, correctionKS.doubleValue(), correctionAxAy.doubleValue(), correctionDP.doubleValue() ); //$NON-NLS-1$
        }
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

  private List<IFE1D2DNode> getOrderedListOfNodes( final IFE1D2DElement element )
  {
    final List<IFE1D2DNode> lOrderedListRes = new ArrayList<IFE1D2DNode>();

    final int lIntEdgesSize = ((IPolyElement<IFE1D2DComplexElement, IFE1D2DEdge>) element).getEdges().size();
    for( int lIntCounter = 0; lIntCounter < lIntEdgesSize - 1; ++lIntCounter )
    { // final IFE1D2DEdge edge : ((IPolyElement<IFE1D2DComplexElement, IFE1D2DEdge>) element).getEdges() ){
      final IFE1D2DEdge lEdgeAct = ((IPolyElement<IFE1D2DComplexElement, IFE1D2DEdge>) element).getEdges().get( lIntCounter );
      final IFE1D2DEdge lEdgeNext = ((IPolyElement<IFE1D2DComplexElement, IFE1D2DEdge>) element).getEdges().get( lIntCounter + 1 );
      if( lEdgeNext.getNodes().contains( lEdgeAct.getNode( 1 ) ) )
      {
        if( !lOrderedListRes.contains( lEdgeAct.getNode( 0 ) ) )
        {
          lOrderedListRes.add( lEdgeAct.getNode( 0 ) );
        }
        lOrderedListRes.add( lEdgeAct.getNode( 1 ) );
      }
      else
      {
        if( !lOrderedListRes.contains( lEdgeAct.getNode( 1 ) ) )
        {
          lOrderedListRes.add( lEdgeAct.getNode( 1 ) );
        }
        lOrderedListRes.add( lEdgeAct.getNode( 0 ) );
      }
    }
    lOrderedListRes.add( lOrderedListRes.get( 0 ) );

    return lOrderedListRes;
  }

  /**
   * Finds the first 1D-Calculation unit which contains the given element.<br>
   * Recursively searches within sub-units of 1d2d units.
   */
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

  private void writeRestartLines( final Formatter formatter, final int nodeID, final GM_Point point, final boolean isMidside ) throws IOException
  {
    if( m_restartNodes == null )
      return;

    if( m_restartNodes.getSize() == 0 )
      return;

    final INodeResult node = m_restartNodes.getNodeResultAtPosition( point );
    if( node == null )
    {
      // we check only corner nodes, because in 1d it could be the case that there are midside nodes without restart
      // data
      if( !isMidside )
      {
        m_log.log( IStatus.WARNING, ISimulation1D2DConstants.CODE_PRE, org.kalypso.kalypsomodel1d2d.conv.i18n.Messages.getString( "org.kalypso.kalypsomodel1d2d.conv.Gml2RMA10SConv.30" ), point, null ); //$NON-NLS-1$
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
      if( velTotal == null || velTotal.size() == 0 )
      {
        velXComp = 0.0;
        velYComp = 0.0;
      }

      else if( velTotal != null )
      {
        //        System.out.print( "velTotal: " + velTotal );
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

  private int getRoughnessID( final IFE1D2DElement element )
  {
    final String roughnessClsID = element.getRoughnessClsID();
    if( roughnessClsID != null && roughnessClsID.length() > 0 )
      return m_roughnessIDProvider.getOrAdd( roughnessClsID );

    // TODO: use default zone instead.
    // Right now it is set to '0' which means the element is deactivated for the simulation
    final String msg = org.kalypso.kalypsomodel1d2d.conv.i18n.Messages.getString( "org.kalypso.kalypsomodel1d2d.conv.Gml2RMA10SConv.31", element.getGmlID() ); //$NON-NLS-1$

    final IFE1D2DNode node = (IFE1D2DNode) element.getNodes().get( 0 );
    final GM_Point point = node.getPoint();
    m_log.log( IStatus.WARNING, ISimulation1D2DConstants.CODE_PRE, msg, point, null );

    return 0;
  }

  public BuildingIDProvider getBuildingProvider( )
  {
    return m_buildingIDProvider;
  }

  class PseudoEdge
  {
    private IFE1D2DNode m_node1;

    private IFE1D2DNode m_node2;

    private int m_intParentId;

    private String m_strGMLParentId;

    private boolean m_boolRealExistingEdge;

    private int m_intLeftParent;

    private int m_intRightParent;

    public PseudoEdge( final IFE1D2DNode pNode1, final IFE1D2DNode pNode2, final int pIntParentID, final String pStrGMLParentId, final boolean pBoolRealExisting )
    {
      m_node1 = pNode1;
      m_node2 = pNode2;
      m_intParentId = pIntParentID;
      m_strGMLParentId = pStrGMLParentId;
      m_boolRealExistingEdge = pBoolRealExisting;
    }

    public final IFE1D2DNode getFirstNode( )
    {
      return m_node1;
    }

    public final void setFirstNode( final IFE1D2DNode pFirstNode )
    {
      m_node1 = pFirstNode;
    }

    public final IFE1D2DNode getSecondNode( )
    {
      return m_node2;
    }

    public final void setSecondNode( final IFE1D2DNode pSecondNode )
    {
      m_node2 = pSecondNode;
    }

    public final int getIntParentId( )
    {
      return m_intParentId;
    }

    public final void setIntParentId( final int intParentId )
    {
      m_intParentId = intParentId;
    }

    public final String getStrGMLParentId( )
    {
      return m_strGMLParentId;
    }

    public final void setStrGMLParentId( final String strGMLParentId )
    {
      m_strGMLParentId = strGMLParentId;
    }

    public final boolean isBoolRealExistingEdge( )
    {
      return m_boolRealExistingEdge;
    }

    public final void setBoolRealExistingEdge( final boolean boolRealExistingEdge )
    {
      m_boolRealExistingEdge = boolRealExistingEdge;
    }

    public final int getIntLeftParent( )
    {
      return m_intLeftParent;
    }

    public final void setIntLeftParent( final int intLeftParent )
    {
      m_intLeftParent = intLeftParent;
    }

    public final int getIntRightParent( )
    {
      return m_intRightParent;
    }

    public final void setIntRightParent( final int intRightParent )
    {
      m_intRightParent = intRightParent;
    }

    /**
     * @see org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DEdge#getMiddleNodePoint()
     */
    public GM_Point getMiddleNodePoint( )
    {
      final GM_Point point1 = getFirstNode().getPoint();
      final GM_Point point2 = getSecondNode().getPoint();
      final double x = (point1.getX() + point2.getX()) / 2;
      final double y = (point1.getY() + point2.getY()) / 2;
      if( point1.getCoordinateDimension() > 2 && point2.getCoordinateDimension() > 2 )
      {
        final double z = (point1.getZ() + point2.getZ()) / 2;
        return GeometryFactory.createGM_Point( x, y, z, point1.getCoordinateSystem() );
      }
      else
        return GeometryFactory.createGM_Point( x, y, point1.getCoordinateSystem() );
    }

    @Override
    public boolean equals( final Object pPseudoEdge )
    {
      if( pPseudoEdge instanceof PseudoEdge )
      {
        final PseudoEdge lPseudoEdge = (PseudoEdge) pPseudoEdge;
        if( (this.m_node1.getGmlID().equals( lPseudoEdge.getFirstNode().getGmlID() ) && this.m_node2.getGmlID().equals( lPseudoEdge.getSecondNode().getGmlID() ))
            || (this.m_node2.getGmlID().equals( lPseudoEdge.getFirstNode().getGmlID() ) && this.m_node1.getGmlID().equals( lPseudoEdge.getSecondNode().getGmlID() )) )
        {
          return true;
        }
      }

      return false;
    }

    @Override
    public String toString( )
    {
      return "First node: " + getFirstNode() + ", second node: " + getSecondNode() + ", gml parent id: " + getStrGMLParentId() + ", created parent id: " + getIntParentId() + ", is real edge: " //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$
      + isBoolRealExistingEdge() + "\n"; //$NON-NLS-1$
    }
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.I2DMeshConverter#supportFlowResistanceClasses()
   */
  @Override
  public boolean supportFlowResistanceClasses( )
  { 
    return SUPPORT_FLOW_RESISTANCE_CLASSES;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.I2DMeshConverter#supportMidsideNodes()
   */
  @Override
  public boolean supportMidsideNodes( )
  {
    return SUPPORT_MIDSIDE_NODES;
  }
}
