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

import java.io.IOException;
import java.io.OutputStream;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Formatter;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.java.lang.NumberUtils;
import org.kalypso.contribs.java.util.FormatterUtils;
import org.kalypso.gml.processes.constDelaunay.ConstraintDelaunayHelper;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.conv.results.SWANResultsReader;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IContinuityLine2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFELine;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IPolyElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IBoundaryCondition;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IBoundaryCondition.BOUNDARY_TYPE;
import org.kalypso.kalypsomodel1d2d.ui.geolog.IGeoLog;
import org.kalypso.kalypsosimulationmodel.core.flowrel.IFlowRelationship;
import org.kalypso.kalypsosimulationmodel.core.flowrel.IFlowRelationshipModel;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree.model.geometry.GM_SurfacePatch;
import org.kalypsodeegree.model.geometry.GM_Triangle;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * Converts discretisation model to SWAN model
 * 
 * @author ig
 */
@SuppressWarnings("unchecked")
public class Gml2SWANConv implements INativeIDProvider
{

  private Formatter m_formatterNodes = null;

  private Formatter m_formatterElements = null;

  private Formatter m_formatterBat = null;

  private Formatter m_formatterPos = null;

  private final IFEDiscretisationModel1d2d m_discretisationModel1d2d;

  private final ICalculationUnit m_calculationUnit;

  private final IGeoLog m_log;

  private Map<GM_Position, Integer> m_mapNodesActPositions;

  private Map<GM_Position, Integer> m_mapPositionsToConditions = new HashMap<GM_Position, Integer>();

  private Map<IFELine, Integer> m_mapContiLineToConditions = new HashMap<IFELine, Integer>();

  private final List<IPolyElement> m_listAllElements;

  private List<GM_Position> m_listAdditionalOuputCoord = null;

  private int m_intCounterElements = 1;

  private int m_intCounterNodes = 1;

  private List<IBoundaryCondition> m_unitBoundaryConditions;

  private int m_intGlobalContiId = 1;

  private double m_doubleGlobalMinX = Double.MAX_VALUE;

  private double m_doubleGlobalMinY = Double.MAX_VALUE;

  private boolean m_boolDoShift = true;

  private String m_strCRS;

  private Map<IFELine, Integer> m_mapContiLineWithSWANBoundaryToCondition = new HashMap<IFELine, Integer>();

  /**
   *
   */
  public Gml2SWANConv( final IFEDiscretisationModel1d2d discretisationModel1d2d, final IFlowRelationshipModel flowrelationModel, final ICalculationUnit calcUnit, final IGeoLog log )
  {
    this( discretisationModel1d2d, flowrelationModel, calcUnit, log, true );
  }
  
  /**
   *
   */
  public Gml2SWANConv( final IFEDiscretisationModel1d2d discretisationModel1d2d, final IFlowRelationshipModel flowrelationModel, final ICalculationUnit calcUnit, final IGeoLog log, final boolean doShift )
  {
    m_discretisationModel1d2d = (discretisationModel1d2d);
    m_calculationUnit = calcUnit;
    m_log = log;
    m_boolDoShift = doShift;

    m_listAllElements = m_calculationUnit.getElements2D();

    final String lStrCalculationUnitId = m_calculationUnit.getGmlID();
    m_unitBoundaryConditions = new ArrayList<IBoundaryCondition>();
    for( final IFlowRelationship relationship : flowrelationModel )
    {
      if( relationship instanceof IBoundaryCondition )
      {
        final IBoundaryCondition boundaryCondition = (IBoundaryCondition) relationship;
        if( boundaryCondition.isMemberOf( lStrCalculationUnitId ) && BOUNDARY_TYPE.WavesBoundary.equals( boundaryCondition.getBoundaryType() ) )
          m_unitBoundaryConditions.add( boundaryCondition );
      }
    }
    if( !m_boolDoShift )
    {
      m_doubleGlobalMinX = 0;
      m_doubleGlobalMinY = 0;
    }
  }

  public int getConversionID( final IFeatureWrapper2 feature )
  {
    return getConversionID( feature, null );
  }

  /**
   * @return <code>0</code>, if feature is <code>null</code> or of unknown type.
   * @see org.kalypso.kalypsomodel1d2d.conv.INativeIDProvider#getConversionID(java.lang.String)
   */
  @SuppressWarnings("unused")
  public int getConversionID( final IFeatureWrapper2 feature, final String pGMLId )
  {
    return 0;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.INativeIDProvider#getConversionID(java.lang.String)
   */
  public int getConversionID( final String featureGmlID )
  {
    return 0;
  }

  /**
   *
   */
  public Map<GM_Position, Integer> writeSWANModel( final OutputStream outputStreamNodes, final OutputStream outputStreamElements, final OutputStream outputStreamBat, final OutputStream outputStreamPos )
  {

    try
    {
      // REMARK: Made a central formatter with US locale (causing decimal point to be '.'),
      // so no locale parameter for each format is needed any more .
      m_formatterNodes = new Formatter( outputStreamNodes, Charset.defaultCharset().name(), Locale.US );
      m_formatterElements = new Formatter( outputStreamElements, Charset.defaultCharset().name(), Locale.US );
      m_formatterBat = new Formatter( outputStreamBat, Charset.defaultCharset().name(), Locale.US );
      m_formatterPos = new Formatter( outputStreamPos, Charset.defaultCharset().name(), Locale.US );
      determineBoundNodes();
      writeSWANNodesModel();
      FormatterUtils.checkIoException( m_formatterNodes );
      FormatterUtils.checkIoException( m_formatterElements );
      FormatterUtils.checkIoException( m_formatterBat );
      FormatterUtils.checkIoException( m_formatterPos );
      return m_mapNodesActPositions;
    }
    catch( Throwable t )
    {
      m_log.log( StatusUtilities.statusFromThrowable( t ) );
      return null;
    }
    finally
    {
      if( m_formatterNodes != null )
      {
        m_formatterNodes.close();
      }
      if( m_formatterElements != null )
      {
        m_formatterElements.close();
      }
      if( m_formatterBat != null )
      {
        m_formatterBat.close();
      }
      if( m_formatterPos != null )
      {
        m_formatterPos.close();
      }
    }
  }

  /**
   * this function checks all nodes for placement on the boundary of selected calculation unit, it is marked as outside
   * node if this boundaries are also in model defined conti-lines and on this lines are defined wave boundary
   * conditions.
   */
  private void determineBoundNodes( )
  {
    for( final Object object : m_listAllElements )
    {
      if( object instanceof IPolyElement )
      {
        final IPolyElement lPolyElement = (IPolyElement) object;
        for( Object lNodeAct : lPolyElement.getNodes() )
        {

          if( m_boolDoShift )
          {
            if( ((IFE1D2DNode) lNodeAct).getPoint().getPosition().getX() < m_doubleGlobalMinX )
            {
              m_doubleGlobalMinX = ((IFE1D2DNode) lNodeAct).getPoint().getPosition().getX();
            }
            if( ((IFE1D2DNode) lNodeAct).getPoint().getPosition().getY() < m_doubleGlobalMinY )
            {
              m_doubleGlobalMinY = ((IFE1D2DNode) lNodeAct).getPoint().getPosition().getY();
            }
          }
          int lIntConditionTmp = -1;
          try
          {
            lIntConditionTmp = m_mapPositionsToConditions.get( ((IFE1D2DNode) lNodeAct).getPoint().getPosition() );
          }
          catch( Exception e )
          {
//            m_log.log( StatusUtilities.statusFromThrowable( e ) );
          }
          if( lIntConditionTmp != -1 )
          {
            continue;
          }

          if( !isNodeOnContiLine( (IFE1D2DNode) lNodeAct ) )
          {
            if( !isNodeOnBound( (IFE1D2DNode) lNodeAct, m_intGlobalContiId ) )
            {
              m_mapPositionsToConditions.put( ((IFE1D2DNode) lNodeAct).getPoint().getPosition(), 0 );
            }
            else
            {
              m_intGlobalContiId++; 
            }
          }
        }
      }
    }
    if( m_boolDoShift )
    {
      m_doubleGlobalMinX = ( (long)m_doubleGlobalMinX-- );
      m_doubleGlobalMinY = ( (long)m_doubleGlobalMinY-- );
    }
  }

  /**
   *
   */
  private boolean isNodeOnContiLine( IFE1D2DNode pNodeAct )
  {
    return isNodeOnContiLine( pNodeAct, m_intGlobalContiId );
  }

  /**
   *
   */
  private boolean isNodeOnContiLine( IFE1D2DNode pNodeAct, final int pIntContiLineId )
  {
    IFELine lContiLineAct = null;
    try
    {
      lContiLineAct = m_discretisationModel1d2d.findContinuityLine( pNodeAct.getPoint(), 0.01 );
    }
    catch( Exception e )
    {
      e.printStackTrace();
      return false;
    }
    if( lContiLineAct != null && lContiLineAct instanceof IContinuityLine2D && m_calculationUnit.contains( lContiLineAct ) && isSWANBoundaryCondOnContiLine( lContiLineAct ) )
    {
      int lIntCondition;
      if( m_mapContiLineToConditions.get( lContiLineAct ) == null )
      {
        lIntCondition = pIntContiLineId;
        m_intGlobalContiId = pIntContiLineId + 1;
        m_mapContiLineToConditions.put( lContiLineAct, lIntCondition );
//        if( isSWANBoundaryCondOnContiLine( lContiLineAct ) )
        {
          m_mapContiLineWithSWANBoundaryToCondition.put( lContiLineAct, lIntCondition );
        }
      }
      else
      {
        lIntCondition = m_mapContiLineToConditions.get( lContiLineAct );
      }
      m_mapPositionsToConditions.put( pNodeAct.getPoint().getPosition(), lIntCondition );
      return true;
    }

    return false;
  }

  /**
   *
   */
  private boolean isSWANBoundaryCondOnContiLine( IFELine contiLineAct )
  {
    for( final IBoundaryCondition boundaryCondition : m_unitBoundaryConditions )
    {
      if( boundaryCondition.getTypeByLocation().equals( IBoundaryCondition.PARENT_TYPE_ELEMENT1D2D ) || boundaryCondition.getTypeByLocation().equals( IBoundaryCondition.PARENT_TYPE_LINE1D2D ) )
      {
        if( boundaryCondition.getParentElementID() != null && boundaryCondition.getParentElementID().equals( contiLineAct.getGmlID() ) )
        {
          return true;
        }
      }
    }
    return false;
  }

  /**
   *
   */
  private void writeSWANNodesModel( )
  {
    m_strCRS = KalypsoDeegreePlugin.getDefault().getCoordinateSystem();
    List<GM_Triangle> lListResults = new ArrayList<GM_Triangle>();
    for( final Object object : m_listAllElements )
    {
      if( object instanceof IPolyElement )
      {
        final IPolyElement lPolyElement = (IPolyElement) object;
        final GM_Surface<GM_SurfacePatch> lGM_Surface = lPolyElement.getGeometry();

        try
        {
          final GM_Triangle[] triangles = ConstraintDelaunayHelper.convertToTriangles( lGM_Surface, m_strCRS, false );
          lListResults.addAll( Arrays.asList( triangles ) );
        }
        catch( Throwable e )
        {
          m_log.log( StatusUtilities.statusFromThrowable( e ) );
        }
      }
    }
    writeAllElements( lListResults, m_mapPositionsToConditions );
  }

  /**
   *
   */
  private int writeAllElements( final List<GM_Triangle> pTriangles, final Map<GM_Position, Integer> pMapPositionsToConditions )
  {
    m_formatterElements.format( "%d 3 0%n", pTriangles.size() ); //$NON-NLS-1$
    m_formatterNodes.format( "%d 2 0 1%n", countNodes( pTriangles ) ); //$NON-NLS-1$

    m_mapNodesActPositions = new HashMap<GM_Position, Integer>();
    for( final GM_Triangle lTriangle : pTriangles )
    {
      try
      {
        List<Integer> lListNodesAct = new ArrayList<Integer>();
        lListNodesAct.clear();
        GM_Position[] lAllPositionsOfTri = lTriangle.getExteriorRing();
        for( final GM_Position lGM_Position : lAllPositionsOfTri )
        {
          GM_Position lGM_PositionRounded = GeometryFactory.createGM_Position( NumberUtils.getRoundedToSignificant( lGM_Position.getX(), SWANResultsReader.INT_ROUND_SIGNIFICANT ), NumberUtils.getRoundedToSignificant( lGM_Position.getY(), SWANResultsReader.INT_ROUND_SIGNIFICANT ), NumberUtils.getRoundedToSignificant( lGM_Position.getZ(), SWANResultsReader.INT_ROUND_SIGNIFICANT ) );
          
//          if( m_mapNodesActPositions.get( lGM_Position ) == null )
          if( m_mapNodesActPositions.get( lGM_PositionRounded ) == null )
          {
            int lIntNodeNr = 0;
            int lIntCondition = -1;
            try
            {
              lIntCondition = pMapPositionsToConditions.get( lGM_Position );
            }
            catch( Exception e )
            {

            }
            if( lIntCondition == -1 )
            {
              // for the case that the angles in triangles should be reduced
              // while checking this some new positions could be created, here the condition for each one of them was
              // set to according number
              if( checkPositionsOnBound( pMapPositionsToConditions, lAllPositionsOfTri ) )
              {
                lIntCondition = getConditionForPosition( lGM_Position );
              }
              else
              {
                lIntCondition = 0;
              }
            }
            lIntNodeNr = writeNode( lGM_PositionRounded, lIntCondition );
            writeBot( lGM_PositionRounded );
            if( m_listAdditionalOuputCoord == null ){
              writePos( getPositionRoundedForSWANAdditional( lGM_PositionRounded ) );
            }
            m_mapNodesActPositions.put( lGM_PositionRounded, lIntNodeNr );
            lListNodesAct.add( lIntNodeNr );
          }
          else
          {
            if( !lListNodesAct.contains( m_mapNodesActPositions.get( lGM_PositionRounded ) ) )
            {
              lListNodesAct.add( m_mapNodesActPositions.get( lGM_PositionRounded ) );
            }
          }
        }
        writeElement( lListNodesAct );
      }
      catch( IOException e )
      {
        m_log.log( StatusUtilities.statusFromThrowable( e ) );
      }
    }

    m_formatterNodes.format( "# generated by Kalypso!%n" ); //$NON-NLS-1$
    
    if( m_listAdditionalOuputCoord != null ){
      Set<GM_Position> lSetPositions = new HashSet<GM_Position>();
      for( Iterator iterator = m_listAdditionalOuputCoord.iterator(); iterator.hasNext(); )
      {
        GM_Position lPosition = getPositionRoundedForSWANAdditional( (GM_Position) iterator.next() );
        if( !lSetPositions.contains( lPosition ) ){
          lSetPositions.add( lPosition );
          try
          {
            writePos( lPosition );
          }
          catch( IOException e )
          {
            KalypsoModel1D2DPlugin.getDefault().getLog().log( StatusUtilities.statusFromThrowable( e ) );
          }
        }
      }
    }
    return 0;
  }

  /**
   *
   */
  private GM_Position getPositionRoundedForSWANAdditional( final GM_Position exactPosition )
  {
    return GeometryFactory.createGM_Position( NumberUtils.getRoundedToSignificant( exactPosition.getX(), 2 ), NumberUtils.getRoundedToSignificant( exactPosition.getY(), 2 ) );
  }

  /**
   *
   */
  private int countNodes( List<GM_Triangle> triangles )
  {
    Set<GM_Position> lSetPositions = new HashSet<GM_Position>();
    for( final GM_Triangle lTri : triangles )
    {
      lSetPositions.addAll( Arrays.asList( lTri.getExteriorRing() ) );
    }
    return lSetPositions.size();
  }

  private int getConditionForPosition( GM_Position position )
  {
    IFELine lContiLineAct = null;
    try
    {
      lContiLineAct = m_discretisationModel1d2d.findContinuityLine( GeometryFactory.createGM_Point( position, m_strCRS ), 0.01 );
    }
    catch( Exception e )
    {
      m_log.log( StatusUtilities.statusFromThrowable( e ) );
      return 0;
    }
    if( lContiLineAct != null && lContiLineAct instanceof IContinuityLine2D && m_calculationUnit.contains( lContiLineAct ) )
    {
      return m_mapContiLineToConditions.get( lContiLineAct );
    }
    return 0;
  }

  private boolean checkPositionsOnBound( Map<GM_Position, Integer> mapPositionsToConditions, GM_Position[] allPositionsOfTri )
  {
    for( final GM_Position lGM_Position : allPositionsOfTri )
    {
      if( mapPositionsToConditions.get( lGM_Position ) != null )
      {
        return true;
      }
    }
    return false;
  }

  private boolean isNodeOnBound( final IFE1D2DNode pNode, final int pIntContiId )
  {
    boolean lBoolResult = false;
    if( m_mapPositionsToConditions.get( pNode.getPoint().getPosition() ) != null || isNodeOnContiLine( pNode, m_intGlobalContiId ) || !isNodeInCalcUnit( pNode )// +
    // 1
    )
    {
      return lBoolResult;
    }
    IFeatureWrapperCollection lContainers = pNode.getContainers();
    for( final Object lContainerObject : lContainers )
    {
      if( lContainerObject instanceof IFE1D2DEdge )
      {
        IFE1D2DEdge lEdge = (IFE1D2DEdge) lContainerObject;

        IFeatureWrapperCollection<IFE1D2DElement> adjacentElements = lEdge.getAdjacentElements();
        if( adjacentElements.size() < 2 || (!m_calculationUnit.contains( adjacentElements.get( 0 ) ) && m_calculationUnit.contains( adjacentElements.get( 1 ) ))
            || (!m_calculationUnit.contains( adjacentElements.get( 1 ) ) && m_calculationUnit.contains( adjacentElements.get( 0 ) )) )
        {
          if( m_mapPositionsToConditions.get( pNode.getPoint().getPosition() ) == null )
            m_mapPositionsToConditions.put( pNode.getPoint().getPosition(), pIntContiId );

          lBoolResult = true;
          for( final Object lNodeObj : lEdge.getNodes() )
          {
            if( lNodeObj instanceof IFE1D2DNode )
            {
              final IFE1D2DNode lNodeNew = (IFE1D2DNode) lNodeObj;
              if( !pNode.equals( lNodeNew ) && isNodeOnBound( lNodeNew, pIntContiId ) )
              {
                // if( m_mapPositionsToConditions.get( lNodeNew.getPoint().getPosition() ) == null )
                // m_mapPositionsToConditions.put( lNodeNew.getPoint().getPosition(), pIntContiId );
              }
            }
          }
        }
      }
    }
    return lBoolResult;
  }

  private boolean isNodeInCalcUnit( IFE1D2DNode pNode )
  {
    for( final IFE1D2DElement lElement : pNode.getElements() )
    {
      if( m_calculationUnit.contains( lElement ) )
      {
        return true;
      }
    }
    return false;
  }

  @SuppressWarnings("unused")
  private int writeBot( final IFE1D2DNode pNodeAct ) throws IOException
  {
    m_formatterBat.format( "%f%n", pNodeAct.getPoint().getZ() ); //$NON-NLS-1$

    FormatterUtils.checkIoException( m_formatterBat );
    return 0;
  }

  private int writeBot( final GM_Position pNodeAct ) throws IOException
  {
    m_formatterBat.format( "%f%n", pNodeAct.getZ() ); //$NON-NLS-1$

    FormatterUtils.checkIoException( m_formatterBat );
    return 0;
  }

  private int writePos( final GM_Position pNodeAct ) throws IOException
  {
    m_formatterPos.format( "%.3f %.3f%n", pNodeAct.getX() - m_doubleGlobalMinX, pNodeAct.getY() - m_doubleGlobalMinY ); //$NON-NLS-1$

    FormatterUtils.checkIoException( m_formatterBat );
    return 0;
  }

  private void writeElement( final List<Integer> pListPositions ) throws IOException
  {
    String lStrElementLine = "" + m_intCounterElements + " "; //$NON-NLS-1$ //$NON-NLS-2$
    for( final Integer lPositionActNr : pListPositions )
    {
      lStrElementLine += lPositionActNr + " "; //$NON-NLS-1$
    }
    m_formatterElements.format( lStrElementLine + "%n" ); //$NON-NLS-1$
    m_intCounterElements++;
    FormatterUtils.checkIoException( m_formatterElements );
  }

  private int writeNode( final GM_Position pNodeAct, final int pIntBoundary ) throws IOException
  {
    String lStrPositionLine = "" + m_intCounterNodes + ""; //$NON-NLS-1$ //$NON-NLS-2$
    m_formatterNodes.format( lStrPositionLine + " %.3f %.3f %d%n", pNodeAct.getX() - m_doubleGlobalMinX, pNodeAct.getY() - m_doubleGlobalMinY, pIntBoundary ); //$NON-NLS-1$

    FormatterUtils.checkIoException( m_formatterNodes );
    return m_intCounterNodes++;

  }

  public final Map<IFELine, Integer> getMapContiLineWithSWANBoundaryToCondition( )
  {
    return m_mapContiLineWithSWANBoundaryToCondition;
  }

  public final double getDoubleGlobalMinX( )
  {
    return m_doubleGlobalMinX;
  }

  public final double getDoubleGlobalMinY( )
  {
    return m_doubleGlobalMinY;
  }

  public final List<GM_Position> getListAdditionalOuputCoord( )
  {
    return m_listAdditionalOuputCoord;
  }

  public final void setListAdditionalOuputCoord( final  List<GM_Position> listAdditionalOuputCoord )
  {
    m_listAdditionalOuputCoord = listAdditionalOuputCoord;
  }
  
  
}
