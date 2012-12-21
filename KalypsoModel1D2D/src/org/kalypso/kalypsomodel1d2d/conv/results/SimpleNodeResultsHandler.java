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
package org.kalypso.kalypsomodel1d2d.conv.results;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.kalypso.contribs.java.lang.NumberUtils;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DDebug;
import org.kalypso.kalypsomodel1d2d.conv.EReadError;
import org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler;
import org.kalypso.kalypsomodel1d2d.conv.RMA10S2GmlConv.RESULTLINES;
import org.kalypso.kalypsomodel1d2d.conv.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.results.INodeResult;
import org.kalypso.kalypsomodel1d2d.schema.binding.results.SimpleNodeResult;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.kalypsodeegree_impl.model.sort.SplitSort;
import org.kalypsodeegree_impl.tools.GeometryUtilities;

/**
 * @author ig
 */
public class SimpleNodeResultsHandler implements IRMA10SModelElementHandler
{
  private final Map<Integer, INodeResult> m_nodeIndex = new HashMap<>();

  private final String m_crs;

  private final Map<Date, List<INodeResult>> m_mapSortedResults = new HashMap<>();

  private final Map<GM_Position, Integer> m_mapInitialPoints;

  private Date m_timeOfResult = new Date( 0 );

  private ArrayList<INodeResult> m_listActResults;

  private final FeatureList m_nodes = new SplitSort( null, null );

  private final double DEFAULT_SEARCH_DISTANCE = 0.5;

  public SimpleNodeResultsHandler( final Map<GM_Position, Integer> pMapInitialPoints )
  {
    m_mapInitialPoints = pMapInitialPoints;

    m_crs = KalypsoDeegreePlugin.getDefault().getCoordinateSystem();
  }

  @Override
  public void end( )
  {
    final List<INodeResult> lListActResults = new ArrayList<>( m_mapInitialPoints.size() );
    fillListWithDefaults( lListActResults, m_mapInitialPoints.size() );
    final Set<Integer> lSetIdsDone = new HashSet<>();
    final Set<Integer> lSetKeys = m_nodeIndex.keySet();
    final Iterator<Integer> lIterSetKeys = lSetKeys.iterator();

    while( lIterSetKeys.hasNext() )
    {
      final Integer lIntIndex = lIterSetKeys.next();
      final INodeResult lActNode = m_nodeIndex.get( lIntIndex );
      int lIntPos = -1;
      final GM_Position lGM_Position = lActNode.getPoint().getPosition();
      GM_Position lGM_PositionRounded = GeometryFactory.createGM_Position( NumberUtils.getRoundedToSignificant( lGM_Position.getX(), NodeResultHelper.INT_ROUND_SIGNIFICANT ), NumberUtils.getRoundedToSignificant( lGM_Position.getY(), NodeResultHelper.INT_ROUND_SIGNIFICANT ), NumberUtils.getRoundedToSignificant( lGM_Position.getZ(), NodeResultHelper.INT_ROUND_SIGNIFICANT ) );

      try
      {
        lIntPos = m_mapInitialPoints.get( lGM_PositionRounded );
      }
      catch( final Exception e )
      {
        final IFE1D2DNode lNode = getResultNodeFromPoint( lActNode.getPoint() );
        try
        {
          lGM_PositionRounded = GeometryFactory.createGM_Position( NumberUtils.getRoundedToSignificant( lNode.getPoint().getX(), NodeResultHelper.INT_ROUND_SIGNIFICANT ), NumberUtils.getRoundedToSignificant( lNode.getPoint().getY(), NodeResultHelper.INT_ROUND_SIGNIFICANT ), NumberUtils.getRoundedToSignificant( lNode.getPoint().getZ(), NodeResultHelper.INT_ROUND_SIGNIFICANT ) );
          lIntPos = m_mapInitialPoints.get( lGM_PositionRounded );
        }
        catch( final Exception e1 )
        {
          // e1.printStackTrace();
        }
      }
      if( lIntPos != -1 )
      {
        lListActResults.set( lIntPos - 1, lActNode );
        lSetIdsDone.add( lIntPos );
      }
    }

    m_listActResults = new ArrayList<>( m_mapInitialPoints.size() );
    m_listActResults.addAll( lListActResults );
  }

  private IFE1D2DNode getResultNodeFromPoint( final GM_Point point )
  {
    final Feature feature = GeometryUtilities.findNearestFeature( point, DEFAULT_SEARCH_DISTANCE, m_nodes, IFE1D2DNode.PROPERTY_POINT );
    if( feature == null )
      return null;

    return (IFE1D2DNode) feature.getAdapter( IFE1D2DNode.class );
  }

  private void fillListWithDefaults( final List<INodeResult> listActResults, final int pIntSize )
  {
    final List<GM_Position> lListPositions = new ArrayList<>( m_mapInitialPoints.keySet() );
    for( int i = 0; i < pIntSize; ++i )
    {
      final SimpleNodeResult lDefaultNodeResult = new SimpleNodeResult();
      lDefaultNodeResult.setWaterlevel( Double.parseDouble( "0.0" ) ); //$NON-NLS-1$
      lDefaultNodeResult.setLocation( lListPositions.get( i ).getX(), lListPositions.get( i ).getY(), lListPositions.get( i ).getZ(), m_crs );
      listActResults.add( lDefaultNodeResult );
    }
  }

  public List<Feature> getCreatedFeatures( )
  {
    return null;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#handleArc(java.lang.String, int, int, int, int,
   *      int, int)
   */
  @Override
  public void handleArc( final String lineString, final int id, final int node1ID, final int node2ID, final int elementLeftID, final int elementRightID, final int middleNodeID )
  {
  }

  @Override
  public void handleFlowResistance( final String lineString, final int id, final double combinedLambda, final double soilLambda, final double vegetationLambda )
  {
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#handleElement(java.lang.String, int, int, int,
   *      int)
   */
  @Override
  public void handleElement( final String lineString, final int id, final int currentRougthnessClassID, final int previousRoughnessClassID, final int eleminationNumber )
  {
    // // For each element calculate the geometry (elemID, cornernode1, midsidenode1, cornernode2, midsidenode2,
    // // not needed in this case
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#handleNode(java.lang.String, int, double, double,
   *      double)
   */
  @Override
  public void handleNode( final String lineString, final int id, final double easting, final double northing, final double elevation )
  {
    try
    {
      /* Create new Simple Node Result */
      /* and remember node result for additional result data */
      final SimpleNodeResult result = new SimpleNodeResult();
      m_nodeIndex.put( id, result );

      /* Fill node result with data */
      result.setLocation( easting, northing, elevation, m_crs );

    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }

  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#handlerError(java.lang.String,
   *      org.kalypso.kalypsomodel1d2d.conv.EReadError)
   */
  @Override
  public void handleError( final String lineString, final EReadError errorHints )
  {
    System.out.println( "Error: " + lineString + ", " + errorHints ); //$NON-NLS-1$ //$NON-NLS-2$
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#start()
   */
  @Override
  public void start( )
  {
    m_timeOfResult = null;
    if( m_timeOfResult != null && m_listActResults != null )
    {
      m_mapSortedResults.put( m_timeOfResult, m_listActResults );
    }
    m_listActResults = null;// new ArrayList< INodeResult >( m_mapInitialPoints.size() );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#handleResult(java.lang.String, int, double,
   *      double, double, double)
   */
  @Override
  public void handleResult( final String lineString, final int id, final double vx, final double vy, final double virtualDepth, final double waterlevel )
  {
    final INodeResult result = m_nodeIndex.get( id );
    if( result == null )
    {
      KalypsoModel1D2DDebug.SIMULATIONRESULT.printf( "%s %d ", Messages.getString( "org.kalypso.kalypsomodel1d2d.conv.results.NodeResultsHandler.71" ), id ); //$NON-NLS-1$ //$NON-NLS-2$
      return;
    }

    // set actual values to result node
    result.setResultValues( vx, vy, virtualDepth, waterlevel );
    final List<Double> velocity = new LinkedList<>();
    velocity.add( vx );
    velocity.add( vy );
    result.setVelocity( velocity );

  }

  @Override
  public void handleTimeDependentAdditionalResult( final String lineString, final int id, final double velXComponent, final double velYComponent, final double depthComponent, final RESULTLINES resultlines )
  {
    final INodeResult result = m_nodeIndex.get( id );

    if( result == null )
    {
      KalypsoModel1D2DDebug.SIMULATIONRESULT.printf( "%s %d ", Messages.getString( "org.kalypso.kalypsomodel1d2d.conv.results.NodeResultsHandler.73" ), id ); //$NON-NLS-1$ //$NON-NLS-2$
      return;
    }
    switch( resultlines )
    {
      case LINE_VO:
        result.setResultPrevStepValues( velXComponent, velYComponent, depthComponent );

        break;
      case LINE_GA:
        result.setTimeDerivativeValues( velXComponent, velYComponent, depthComponent );

        break;
      case LINE_GO:
        result.setTimeDerivativeValuesPrevStep( velXComponent, velYComponent, depthComponent );

        break;
      case LINE_VA:
        break;

        // TODO: catch LINE_VA case and print message; normally the handleResult function can be called without water
        // stage information!
        // Normally this shouldn't happen, because otherwise the 2D-file is 'broken'

    }
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#handleTime(java.lang.String, java.util.Date)
   */
  @Override
  public void handleTime( final String line, final Date time )
  {
    if( m_timeOfResult == null )
    {
      m_timeOfResult = time;
    }
  }

  /**
   * Returns the time which was read from the .2d file.
   */
  public Date getTime( )
  {
    return m_timeOfResult;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#handleJunction(java.lang.String, int, int, int,
   *      int)
   */
  @Override
  public void handleJunction( final String parseLine, final int junctionID, final int element1dID, final int boundaryLine2dID, final int node1dID )
  {
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#handleNodeInformation(java.lang.String, int, int,
   *      double, double, double, double)
   */
  @Override
  public void handleNodeInformation( final String line, final int id, final int dry, final double value1, final double value2, final double value3, final double value4 )
  {
    final INodeResult result = m_nodeIndex.get( id );
    if( result == null )
    {
      KalypsoModel1D2DDebug.SIMULATIONRESULT.printf( "%s %d ", Messages.getString( "org.kalypso.kalypsomodel1d2d.conv.results.NodeResultsHandler.75" ), id ); //$NON-NLS-1$ //$NON-NLS-2$
      return;
    }
    result.setDry( dry );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#handle1dJunctionInformation(java.lang.String,
   *      int, java.util.List)
   */
  @Override
  public void handle1dJunctionInformation( final String line, final int junctionId, final List<Integer> junctionNodeIDList )
  {
  }

  public final Map<GM_Position, Integer> getMapInitialPoints( )
  {
    return m_mapInitialPoints;
  }

  public List<INodeResult> getResultsForDate( final Date pDate )
  {
    return m_mapSortedResults.get( pDate );
  }

  public List<INodeResult> updateLastRecordForDate( final Date pDate )
  {
    m_mapSortedResults.put( pDate, m_listActResults );
    return m_listActResults;
  }

  @Override
  public void handle1dPolynomialRangesInformation( final String line, final String lStrPolyKind, final int lIntNodeId, final int lIntAmountRanges, final List<Double> lListPolyAreaMaxRanges )
  {
  }

  @Override
  public void handle1dPolynomeMinMax( final String line, final int id, final double min, final double max )
  {
  }

  @Override
  public void handle1dSplittedPolynomialsInformation( final String line, final String lStrPolyKind, final int lIntNodeId, final int lIntAmountRanges, final List<Double> lListPolyAreaMaxRanges, final Double lIntSlope )
  {
  }

  @Override
  public void handleNode( final String line, final int id, final double easting, final double northing, final double elevation, final double stationName )
  {
  }

  @Override
  public void handleRoughness( final String id, final String label )
  {
  }
}
