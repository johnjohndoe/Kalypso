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
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Formatter;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

import javax.xml.datatype.XMLGregorianCalendar;

import org.apache.commons.vfs2.FileObject;
import org.eclipse.core.runtime.CoreException;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.java.util.FormatterUtils;
import org.kalypso.gml.processes.constDelaunay.ConstraintDelaunayHelper;
import org.kalypso.kalypsomodel1d2d.conv.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.conv.results.RestartNodes;
import org.kalypso.kalypsomodel1d2d.conv.telemac.SerafinWriter;
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
import org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModel1D2D;
import org.kalypso.kalypsomodel1d2d.schema.dict.Kalypso1D2DDictConstants;
import org.kalypso.kalypsomodel1d2d.ui.geolog.IGeoLog;
import org.kalypso.kalypsosimulationmodel.core.flowrel.IFlowRelationship;
import org.kalypso.kalypsosimulationmodel.core.flowrel.IFlowRelationshipModel;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.ComponentUtilities;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.observation.result.TupleResultUtilities;
import org.kalypso.observation.util.TupleResultIndex;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Polygon;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Triangle;

/**
 * Converts discretisation model to Telemac model
 * 
 * @author ig
 */
public class Gml2TelemacConv implements INativeIDProvider
{
  public static final String SERAFIN_PARAM_TYPE_VISCOSITY = "VISCOSITY"; //$NON-NLS-1$

  public static final String SERAFIN_PARAM_TYPE_FREE_SURFACE = "FREE SURFACE"; //$NON-NLS-1$

  public static final String SERAFIN_PARAM_TYPE_WATER_DEPTH = "WATER DEPTH"; //$NON-NLS-1$

  public static final String SERAFIN_PARAM_TYPE_VELOCITY_V = "VELOCITY V"; //$NON-NLS-1$

  public static final String SERAFIN_PARAM_TYPE_VELOCITY_U = "VELOCITY U"; //$NON-NLS-1$

  public static final String SERAFIN_UNIT_METER_QUADRAT_PER_SEC = "M2/S"; //$NON-NLS-1$

  public static final String SERAFIN_UNIT_METER_PER_SEC = "M/S"; //$NON-NLS-1$

  public static final String SERAFIN_UNIT_METER = "M"; //$NON-NLS-1$

  public static final String SERAFIN_PARAM_TYPE_BOT = "BOTTOM"; //$NON-NLS-1$

  public static final String BOUNDARY_CONDITIONS_FILE_EXTENTION = ".qls"; //$NON-NLS-1$

  public static final String BOUNDARY_NODES_FILE_EXTENTION = ".cli"; //$NON-NLS-1$

  public static final String SERAFIN_FILE_EXTENTION = ".slf"; //$NON-NLS-1$

  public static final String TELEMAC_UNIT_METER_QUBIC_PER_SEC = "m3/s"; //$NON-NLS-1$

  public static final String TELEMAC_Q_TYPE = "Q"; //$NON-NLS-1$

  public static final String TELEMAC_BOUNDARY_TYPE_Q = "4 5 5"; //$NON-NLS-1$

  public static final String TELEMAC_UNIT_METER = "m"; //$NON-NLS-1$

  public static final String TELEMAC_H_TYPE = "SL"; //$NON-NLS-1$

  public static final String TELEMAC_BOUNDARY_TYPE_H_WQ = "5 4 4"; //$NON-NLS-1$

  public static final String WQ_BOUND_FILE_EXTENTION = ".slq"; //$NON-NLS-1$

  public static final String TELEMAC_BOUNDARY_TYPE_CLOSED = "2 2 2"; //$NON-NLS-1$

  public static final String CASE_FILE_EXTENTION = ".cas";

  public static final String GEO_FILE_PREFIX = "geo_";

  public static final String RESULT_FILE_PREFIX = "res_";

  private static final Integer INNER_POSITION_ID = 0;

  private static final Integer OUTER_CLOSED_POSITION_ID = 1;

  class BoundaryConditionTelemac
  {
    private String m_name;

    private String m_unit;

    private String m_typeStr;

    private int m_type;

    private boolean m_isWQ = false;

    private int m_factor = 1;

    private IObservation<TupleResult> m_obs;

    private IComponent m_abscissaComponent;

    private IComponent m_ordinateComponent;

    private String m_parentContiLineId;

    public BoundaryConditionTelemac( )
    {
    }

    public BoundaryConditionTelemac( int type )
    {
      m_type = type;
    }

    public String getName( )
    {
      return m_name;
    }

    public void setName( String name )
    {
      this.m_name = name;
    }

    public String getUnit( )
    {
      return m_unit;
    }

    public void setUnit( String unit )
    {
      this.m_unit = unit;
    }

    public String getTypeStr( )
    {
      return m_typeStr;
    }

    public void setTypeStr( String typeStr )
    {
      this.m_typeStr = typeStr;
    }

    public int getType( )
    {
      return m_type;
    }

    public void setType( int type )
    {
      this.m_type = type;
    }

    public boolean isWQ( )
    {
      return m_isWQ;
    }

    public void setWQ( boolean isWQ )
    {
      m_isWQ = isWQ;
    }

    public IObservation<TupleResult> getObs( )
    {
      return m_obs;
    }

    public void setObs( IObservation<TupleResult> observation )
    {
      m_obs = observation;
    }

    public IComponent getAbscissaComponent( )
    {
      return m_abscissaComponent;
    }

    public void setAbscissaComponent( IComponent abscissaComponent )
    {
      this.m_abscissaComponent = abscissaComponent;
    }

    public IComponent getOrdinateComponent( )
    {
      return m_ordinateComponent;
    }

    public void setOrdinateComponent( IComponent ordinateComponent )
    {
      this.m_ordinateComponent = ordinateComponent;
    }

    public int getFactor( )
    {
      return m_factor;
    }

    public void setFactor( int factor )
    {
      m_factor = factor;
    }

    public void setParentContiLineId( final String contiLineId )
    {
      m_parentContiLineId = contiLineId;
    }

    public String getParentContiLineId()
    {
      return m_parentContiLineId;
    }
    
    @Override
    public int hashCode( )
    {
      final int prime = 31;
      int result = 1;
      result = prime * result + getOuterType().hashCode();
      result = prime * result + ((m_parentContiLineId == null) ? 0 : m_parentContiLineId.hashCode());
      result = prime * result + ((m_typeStr == null) ? 0 : m_typeStr.hashCode());
      return result;
    }

    @Override
    public boolean equals( Object obj )
    {
      if( this == obj )
        return true;
      if( obj == null )
        return false;
      if( getClass() != obj.getClass() )
        return false;
      BoundaryConditionTelemac other = (BoundaryConditionTelemac)obj;
      if( !getOuterType().equals( other.getOuterType() ) )
        return false;
      if( m_parentContiLineId == null )
      {
        if( other.m_parentContiLineId != null )
          return false;
      }
      else if( !m_parentContiLineId.equals( other.m_parentContiLineId ) )
        return false;
      if( m_typeStr == null )
      {
        if( other.m_typeStr != null )
          return false;
      }
      else if( !m_typeStr.equals( other.m_typeStr ) )
        return false;
      return true;
    }

    private Gml2TelemacConv getOuterType( )
    {
      return Gml2TelemacConv.this;
    }
    
  }

  private Formatter m_formatterBoundNodes = null;

  private Formatter m_formatterBoundaries = null;

  private Formatter m_formatterWQBoundaries = null;

  private final IFEDiscretisationModel1d2d m_discretisationModel1d2d;

  private final ICalculationUnit m_calculationUnit;

  private final IGeoLog m_log;

  private Map<GM_Position, Integer> m_mapNodesActPositions;

  private Map<GM_Position, Integer> m_mapPositionsToConditions = new HashMap<>();

  private Map<Integer, String> m_mapBoundaryToContilineId = new HashMap<>();

  private final List<IPolyElement> m_listAll2DElements;

  private List<GM_Position> m_listBoundNodes = new ArrayList<>();

  private List<GM_Position> m_listAdditionalOuputCoord = null;

  private int m_intCounterNodes = 1;

  private Map<String, IBoundaryCondition> m_mapUnitBoundaryConditions;

  private int m_intGlobalContiId = 1;

  private int m_globalMinX = Integer.MAX_VALUE;

  private int m_globalMinY = Integer.MAX_VALUE;

  private boolean m_boolDoShift = true;

  private String m_strCRS;

  private int[] m_iparams = new int[SerafinWriter.IPARAM_NB];

  private String[] m_paramNames;

  private String[] m_paramUnits;

  private RestartNodes m_restartNodes;

  private List<BoundaryConditionTelemac> m_listBoundaries = new ArrayList<>();

  private IControlModel1D2D m_controlModel;

  private ArrayList<GM_Position> m_listAllNodes;

  private final Map<Integer, TupleResultIndex> m_BCTupleResultIndexCache = new HashMap<>();

  private FileObject m_fileObjWorkingDir;

  private String m_projectFileName = null;

  private boolean m_boundFound;

  private Set<String> m_setElementsIds = null;

  private Map<IFE1D2DNode, IFELine> m_mapAllNodesOnContiLines = new HashMap<>(); 

  /**
   *
   */
  public Gml2TelemacConv( final IFEDiscretisationModel1d2d discretisationModel1d2d, final IFlowRelationshipModel flowrelationModel, final IControlModel1D2D controlModel, final RestartNodes restartNodes, final IGeoLog log )
  {
    this( discretisationModel1d2d, flowrelationModel, controlModel, restartNodes, log, true );
  }

  /**
   *
   */
  public Gml2TelemacConv( final IFEDiscretisationModel1d2d discretisationModel1d2d, final IFlowRelationshipModel flowrelationModel, final IControlModel1D2D controlModel, final RestartNodes restartNodes, final IGeoLog log, final boolean doShift )
  {
    m_discretisationModel1d2d = (discretisationModel1d2d);
    m_controlModel = controlModel;
    m_calculationUnit = controlModel.getCalculationUnit();
    m_log = log;
    m_boolDoShift = doShift;
    m_restartNodes = restartNodes;

    m_listAll2DElements = m_calculationUnit.getElements2D();
    for( IFELine contiLine:  m_calculationUnit.getContinuityLines() ){
      for( IFE1D2DNode node: contiLine.getNodes() ){
        m_mapAllNodesOnContiLines.put( node, contiLine );
      }
    }

    if( m_setElementsIds == null )
    {
      m_setElementsIds = new HashSet<>();
      for( IPolyElement element : m_listAll2DElements )
        m_setElementsIds.add( element.getId() );
    }
    final String lStrCalculationUnitId = m_calculationUnit.getId();
    m_mapUnitBoundaryConditions = new HashMap<>();
    for( final IFlowRelationship relationship : flowrelationModel.getFlowRelationsShips() )
    {
      if( relationship instanceof IBoundaryCondition )
      {
        final IBoundaryCondition boundaryCondition = (IBoundaryCondition)relationship;
        String parentElement = null;
        if( boundaryCondition.isMemberOf( lStrCalculationUnitId ) && !BOUNDARY_TYPE.WavesBoundary.equals( boundaryCondition.getBoundaryType() ) )
          parentElement = boundaryCondition.getParentElementID();
        if( parentElement != null )
        {
          m_mapUnitBoundaryConditions.put( parentElement, boundaryCondition );
        }
      }
    }
    if( !m_boolDoShift )
    {
      m_globalMinX = 0;
      m_globalMinY = 0;
    }
  }

  @Override
  public int getConversionID( final Feature feature )
  {
    return getConversionID( feature, null );
  }

  /**
   * @return <code>0</code>, if feature is <code>null</code> or of unknown type.
   * @see org.kalypso.kalypsomodel1d2d.conv.INativeIDProvider#getConversionID(java.lang.String)
   */
  @SuppressWarnings( "unused" )
  public int getConversionID( final Feature feature, final String pGMLId )
  {
    return 0;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.INativeIDProvider#getConversionID(java.lang.String)
   */
  @Override
  public int getConversionID( final String featureGmlID )
  {
    return 0;
  }

  /**
   *
   */
  public Map<GM_Position, Integer> writeTelemacModel( final FileObject pFileObjWorkingDir )
  {
    try
    {
      m_fileObjWorkingDir = pFileObjWorkingDir;
      // REMARK: Made a central formatter with US locale (causing decimal point to be '.'),
      // so no locale parameter for each format is needed any more .
      final FileObject lModelBoundNodesFile = pFileObjWorkingDir.resolveFile( getProjectFileName() + BOUNDARY_NODES_FILE_EXTENTION );
      final FileObject lModelBoundariesFile = pFileObjWorkingDir.resolveFile( getProjectFileName() + BOUNDARY_CONDITIONS_FILE_EXTENTION );
      m_formatterBoundNodes = new Formatter( lModelBoundNodesFile.getContent().getOutputStream(), Charset.defaultCharset().name(), Locale.US );
      m_formatterBoundaries = new Formatter( lModelBoundariesFile.getContent().getOutputStream(), Charset.defaultCharset().name(), Locale.US );
      determineBoundNodes();
      initIParams();
      writeTelemacNodesModel( pFileObjWorkingDir );
      FormatterUtils.checkIoException( m_formatterBoundNodes );
      FormatterUtils.checkIoException( m_formatterBoundaries );
      return m_mapNodesActPositions;
    }
    catch( Throwable t )
    {
      t.printStackTrace();
      m_log.log( StatusUtilities.statusFromThrowable( t ) );
      return null;
    }
    finally
    {
      if( m_formatterBoundNodes != null )
      {
        m_formatterBoundNodes.close();
      }
      if( m_formatterBoundaries != null )
      {
        m_formatterBoundaries.close();
      }
    }
  }

  private void initIParams( )
  {
    /*
     * BOTTOM M VELOCITY U M/S VELOCITY V M/S WATER DEPTH M FREE SURFACE M VISCOSITY M2/S
     */

    if( m_restartNodes == null )
    {
      m_paramNames = new String[] { SERAFIN_PARAM_TYPE_BOT };
      m_paramUnits = new String[] { SERAFIN_UNIT_METER };
    }
    else
    {
      m_paramNames = new String[] { SERAFIN_PARAM_TYPE_BOT, SERAFIN_PARAM_TYPE_VELOCITY_U, SERAFIN_PARAM_TYPE_VELOCITY_V, SERAFIN_PARAM_TYPE_WATER_DEPTH, SERAFIN_PARAM_TYPE_FREE_SURFACE };
      m_paramUnits = new String[] { SERAFIN_UNIT_METER, SERAFIN_UNIT_METER_PER_SEC, SERAFIN_UNIT_METER_PER_SEC, SERAFIN_UNIT_METER, SERAFIN_UNIT_METER };
    }
  }

  /**
   * this function checks all nodes for placement on the boundary of selected calculation unit, it is marked as outside
   * node if this boundaries are also in model defined conti-lines and on this lines are defined wave boundary
   * conditions.
   */
  private void determineBoundNodes( )
  {
    double doubleGlobalMinX = Double.MAX_VALUE;
    double doubleGlobalMinY = Double.MAX_VALUE;
    for( final Object object : m_listAll2DElements )
    {
      if( object instanceof IPolyElement )
      {
        final IPolyElement lPolyElement = (IPolyElement)object;
        for( Object lNodeAct : lPolyElement.getNodes() )
        {
          if( m_boolDoShift )
          {
            if( ((IFE1D2DNode)lNodeAct).getPoint().getPosition().getX() < doubleGlobalMinX )
            {
              doubleGlobalMinX = ((IFE1D2DNode)lNodeAct).getPoint().getPosition().getX();
            }
            if( ((IFE1D2DNode)lNodeAct).getPoint().getPosition().getY() < doubleGlobalMinY )
            {
              doubleGlobalMinY = ((IFE1D2DNode)lNodeAct).getPoint().getPosition().getY();
            }
          }
          int lIntConditionTmp = -1;
          try
          {
            lIntConditionTmp = m_mapPositionsToConditions.get( ((IFE1D2DNode)lNodeAct).getPoint().getPosition() );
          }
          catch( Exception e )
          {
            // m_log.log( StatusUtilities.statusFromThrowable( e ) );
          }
          if( lIntConditionTmp != -1 )
          {
            continue;
          }

          {
            m_boundFound = false;
            if( !isNodeOnBound( (IFE1D2DNode)lNodeAct ) )
            {
              m_mapPositionsToConditions.put( ((IFE1D2DNode)lNodeAct).getPoint().getPosition(), INNER_POSITION_ID );
            }
            else
            {
              // m_intGlobalContiId++;
            }
          }
        }
      }
    }
    if( m_boolDoShift )
    {
      m_globalMinX = ((int)doubleGlobalMinX--);
      m_globalMinY = ((int)doubleGlobalMinY--);
    }
  }

  /**
   *
   */
  private boolean isNodeOnContiLine( IFE1D2DNode pNodeAct )
  {
    IFELine lContiLineAct = null;
    if( m_mapAllNodesOnContiLines.isEmpty() || !m_mapAllNodesOnContiLines.containsKey( pNodeAct ) ){
      return false;
    }

    lContiLineAct = m_mapAllNodesOnContiLines.get( pNodeAct );
    if( lContiLineAct != null && lContiLineAct instanceof IContinuityLine2D && isBoundaryCondOnContiLine( lContiLineAct ) )
    {
      GM_Position position = pNodeAct.getPoint().getPosition();
      if( m_mapBoundaryToContilineId.get( m_intGlobalContiId ) == null || !m_mapBoundaryToContilineId.containsValue( lContiLineAct.getId() ) )
      {
        m_intGlobalContiId++;
        m_mapBoundaryToContilineId.put( m_intGlobalContiId, lContiLineAct.getId() );
      }
      m_mapPositionsToConditions.put( position, getExistingContiLineId( lContiLineAct.getId() ) );
      if( !m_listBoundNodes.contains( position ) )
        m_listBoundNodes.add( position );
      return true;
    }

    return false;
  }

  private Integer getExistingContiLineId( String id )
  {
    if( !m_mapBoundaryToContilineId.containsValue( id ) ){
      return null;
    }
    for( Iterator<Integer> iterator = m_mapBoundaryToContilineId.keySet().iterator(); iterator.hasNext(); )
    {
      Integer key = iterator.next();
      if( m_mapBoundaryToContilineId.get( key ).equals( id ) ){
        return key;
      }
    }
    return null;
  }

  /**
   *
   */
  private boolean isBoundaryCondOnContiLine( IFELine contiLineAct )
  {
    if( m_mapUnitBoundaryConditions.get( contiLineAct.getId() ) != null )
    {
      return true;
    }
    return false;
  }

  /**
   *
   *
   */
  private void writeTelemacNodesModel( FileObject pFileObjWorkingDir )
  {
    m_strCRS = KalypsoDeegreePlugin.getDefault().getCoordinateSystem();
    List<GM_Triangle> lListResults = new ArrayList<>();
    for( final Object object : m_listAll2DElements )
    {
      if( object instanceof IPolyElement )
      {
        final IPolyElement lPolyElement = (IPolyElement)object;
        final GM_Polygon lGM_Surface = lPolyElement.getGeometry();

        try
        {
          final GM_Triangle[] triangles = ConstraintDelaunayHelper.triangulateSimple( lGM_Surface ); //$NON-NLS-1$
          lListResults.addAll( Arrays.asList( triangles ) );
        }
        catch( Throwable e )
        {
          m_log.log( StatusUtilities.statusFromThrowable( e ) );
        }
      }
    }
    writeAllElements( lListResults, pFileObjWorkingDir );
  }

  /**
   *
   */
  private int writeAllElements( final List<GM_Triangle> pTriangles, final FileObject pFileObjWorkingDir )
  {
    m_listAllNodes = new ArrayList<>( countNodes( pTriangles ) );
    if( m_boolDoShift )
    {
      m_iparams[2] = m_globalMinX;
      m_iparams[3] = m_globalMinY;
    }
    SerafinWriter serafinWriter = new SerafinWriter( m_calculationUnit.getName(), pTriangles/* , pMapPositionsToConditions */, m_listAllNodes, m_listBoundNodes, m_iparams, m_strCRS );
//    int timeSteps = 1;
    try
    {
      FileObject serafinFile = pFileObjWorkingDir.resolveFile( GEO_FILE_PREFIX + getProjectFileName() + SERAFIN_FILE_EXTENTION );
      serafinWriter.setFile( new File( serafinFile.getURL().toURI() ) );// pFileObjWorkingDir.getURL().toURI().resolve(
                                                                        // "test_telemac.slf" ) ) ); timeSteps
      serafinWriter.writeAll( m_paramNames, m_paramUnits, m_restartNodes );

      writeBoundNodes();
      
      writeBoundaries();
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }

    return 0;
  }

  public String getProjectFileName( )
  {
    if( m_projectFileName == null || "".equals( m_projectFileName.trim() ) ) { //$NON-NLS-1$
      m_projectFileName = m_calculationUnit.getName().trim().replace( ' ', '_' );
    }

    return m_projectFileName;
  }

  @SuppressWarnings( "deprecation" )
  private void writeBoundaries( ) throws CoreException
  {
    Map<Integer, BoundaryConditionTelemac> wqBounds = new HashMap<>();
    String headerLine0 = "  T  "; //$NON-NLS-1$
    String headerLine1 = "  t  "; //$NON-NLS-1$

    int iCount = 1;
    for( BoundaryConditionTelemac actBoundary : m_listBoundaries )
    {
      if( actBoundary.isWQ() )
      {
        wqBounds.put( iCount, actBoundary );
        iCount++;
        continue;
      }
      else if( TELEMAC_BOUNDARY_TYPE_CLOSED.equals( actBoundary.getTypeStr() ) )
      {
        continue;
      }

      headerLine0 += actBoundary.getName() + "(" + iCount + ")  "; //$NON-NLS-1$  //$NON-NLS-2$
      headerLine1 += actBoundary.getUnit() + "   "; //$NON-NLS-1$
      iCount++;
    }

    headerLine0 += "%n"; //$NON-NLS-1$
    headerLine1 += "%n"; //$NON-NLS-1$
    if( wqBounds.size() < iCount - 1 )
    {
      m_formatterBoundaries.format( headerLine0 );
      m_formatterBoundaries.format( headerLine1 );

      String valuesLine = " ";

//      final IObservation<TupleResult> observation = m_controlModel.getTimeSteps();
//      final TupleResult result = observation.getResult();
//      final IComponent[] components = result.getComponents();
//      final IComponent componentTime = ComponentUtilities.findComponentByID( components, Kalypso1D2DDictConstants.DICT_COMPONENT_TIME );

      {
        /* Sort records by time */
        final TupleResultIndex index = m_BCTupleResultIndexCache.get( m_listBoundaries.get( 0 ).getType() );
        Iterator<IRecord> iterator = index.getIterator();
        
        final IComponent[] components = m_listBoundaries.get( 0 ).getObs().getResult().getComponents();
        final IComponent componentTime = ComponentUtilities.findComponentByID( components, Kalypso1D2DDictConstants.DICT_COMPONENT_TIME );
        
//        if( !iterator.hasNext() )
//        {
//          final String errMsg = Messages.getString( "org.kalypso.kalypsomodel1d2d.conv.Control1D2DConverter.35" );//$NON-NLS-1$
//          throw new CoreException( StatusUtilities.createErrorStatus( errMsg ) );
//        }

        final IRecord firstRecord = iterator.next();

        {
          final TupleResult owner = firstRecord.getOwner();
          final int indexTime = owner.indexOfComponent( componentTime );

          final XMLGregorianCalendar cal = (XMLGregorianCalendar)firstRecord.getValue( indexTime );
          long firstStepCal = cal.toGregorianCalendar().getTime().getTime();
          int stepCount = 1;
          iterator = index.getIterator();
          for( ; iterator.hasNext(); stepCount++ )
          {
            final IRecord record = iterator.next();

            final XMLGregorianCalendar stepXMLGrCal = (XMLGregorianCalendar)record.getValue( indexTime );
            final Calendar stepCal = stepXMLGrCal.toGregorianCalendar();
            valuesLine += "" + ((stepCal.getTime().getTime() - firstStepCal) / 1000); //$NON-NLS-1$
            valuesLine += "  "; //$NON-NLS-1$
            for( BoundaryConditionTelemac actBoundary : m_listBoundaries )
            {
              if( TELEMAC_BOUNDARY_TYPE_CLOSED.equals( actBoundary.getTypeStr() ) || actBoundary.isWQ() )
              {
                continue;
              }
              valuesLine += actBoundary.getFactor() * getValueBoundCond( actBoundary, stepCal ) + " "; //$NON-NLS-1$
            }
            valuesLine += "%n"; //$NON-NLS-1$
            m_formatterBoundaries.format( valuesLine );
            valuesLine = ""; //$NON-NLS-1$
          }
          if( m_controlModel.getIaccyc() > stepCount )
          {
            final String errMsg = Messages.getString( "org.kalypso.kalypsomodel1d2d.conv.Control1D2DConverter.12", stepCount, m_controlModel.getIaccyc() ); //$NON-NLS-1$
            throw new CoreException( StatusUtilities.createErrorStatus( errMsg ) );
          }
        }
      }
    }
    if( wqBounds.size() > 0 )
    {
      writeWQBoundaries( wqBounds );
    }
  }

  private void writeWQBoundaries( Map<Integer, BoundaryConditionTelemac> wqBounds )
  {
    try
    {
      final FileObject lModelWQBoundariesFile = m_fileObjWorkingDir.resolveFile( getProjectFileName() + WQ_BOUND_FILE_EXTENTION );
      m_formatterWQBoundaries = new Formatter( lModelWQBoundariesFile.getContent().getOutputStream(), Charset.defaultCharset().name(), Locale.US );
      for( Iterator<Integer> iterator = wqBounds.keySet().iterator(); iterator.hasNext(); )
      {
        Integer boundId = iterator.next();
        BoundaryConditionTelemac actBoundary = wqBounds.get( boundId );
        String headerLine = "  Q(" + boundId + ")  Z(" + boundId + ")  %n";//$NON-NLS-1$  //$NON-NLS-2$  //$NON-NLS-3$
        m_formatterWQBoundaries.format( headerLine );
        headerLine = "  m3/s    m%n"; //$NON-NLS-1$
        m_formatterWQBoundaries.format( headerLine );
        final TupleResult result = actBoundary.getObs().getResult();
        final IComponent[] components = result.getComponents();
        final int icompQ = result.indexOfComponent( ComponentUtilities.findComponentByID( components, Kalypso1D2DDictConstants.DICT_COMPONENT_DISCHARGE ) );
        final int icompZ = result.indexOfComponent( ComponentUtilities.findComponentByID( components, Kalypso1D2DDictConstants.DICT_COMPONENT_WATERLEVEL ) );
        for( IRecord record : result )
        {
          m_formatterWQBoundaries.format( " %f %f%n", record.getValue( icompQ ), record.getValue( icompZ ) ); //$NON-NLS-1$
        }

      }
      FormatterUtils.checkIoException( m_formatterWQBoundaries );
    }
    catch( Throwable t )
    {
      t.printStackTrace();
      m_log.log( StatusUtilities.statusFromThrowable( t ) );
    }
    finally
    {
      if( m_formatterWQBoundaries != null )
      {
        m_formatterWQBoundaries.close();
      }
    }
  }

  /**
   * get value of boundary condition (QC or HC or ... ) for given time.
   */
  private Double getValueBoundCond( final BoundaryConditionTelemac boundary, final Calendar stepCal )
  {
    final TupleResultIndex tupleResultIndex = m_BCTupleResultIndexCache.get( boundary.getType() );
    final Number result = (Number)tupleResultIndex.getValue( boundary.getOrdinateComponent(), stepCal.getTime() );
    return (result == null || Double.isNaN( result.doubleValue() )) ? 0.0 : result.doubleValue();
  }

  private BoundaryConditionTelemac resolveBoundaryType( int actBoundary )
  {
    BoundaryConditionTelemac boundary = new BoundaryConditionTelemac( actBoundary );
    String contiLineId = m_mapBoundaryToContilineId.get( actBoundary );
    boundary.setParentContiLineId( contiLineId );
    if( contiLineId == null )
    {
      boundary.setTypeStr( TELEMAC_BOUNDARY_TYPE_CLOSED );
      boundary.setName( "" ); //$NON-NLS-1$
      boundary.setUnit( "" ); //$NON-NLS-1$
      return boundary;
    }
    IBoundaryCondition boundaryCondition = m_mapUnitBoundaryConditions.get( contiLineId );
    {
      if( boundaryCondition.getTypeByLocation().equals( IBoundaryCondition.PARENT_TYPE_LINE1D2D ) )
      {
        final IObservation<TupleResult> obs = boundaryCondition.getObservation();
        final TupleResult obsResult = obs.getResult();
        boundary.setObs( obs );
        IComponent abscissaComponent = TupleResultUtilities.findComponentById( obsResult, Kalypso1D2DDictConstants.DICT_COMPONENT_TIME );
        IComponent ordinateComponent = TupleResultUtilities.findComponentById( obsResult, Kalypso1D2DDictConstants.DICT_COMPONENT_WATERLEVEL );
        if( abscissaComponent != null && ordinateComponent != null )
        {
          m_BCTupleResultIndexCache.put( actBoundary, new TupleResultIndex( obsResult, abscissaComponent ) );
          boundary.setTypeStr( TELEMAC_BOUNDARY_TYPE_H_WQ );
          boundary.setName( TELEMAC_H_TYPE );
          boundary.setUnit( TELEMAC_UNIT_METER );
          boundary.setAbscissaComponent( abscissaComponent );
          boundary.setOrdinateComponent( ordinateComponent );
        }
        else
        {
          abscissaComponent = TupleResultUtilities.findComponentById( obsResult, Kalypso1D2DDictConstants.DICT_COMPONENT_TIME );
          ordinateComponent = TupleResultUtilities.findComponentById( obsResult, Kalypso1D2DDictConstants.DICT_COMPONENT_DISCHARGE );
          if( abscissaComponent != null && ordinateComponent != null )
          {
            m_BCTupleResultIndexCache.put( actBoundary, new TupleResultIndex( obsResult, abscissaComponent ) );
            boundary.setTypeStr( TELEMAC_BOUNDARY_TYPE_Q );
            boundary.setName( TELEMAC_Q_TYPE );
            boundary.setUnit( TELEMAC_UNIT_METER_QUBIC_PER_SEC );
            boundary.setAbscissaComponent( abscissaComponent );
            boundary.setOrdinateComponent( ordinateComponent );
//            boundary.setFactor( -1 );
          }
          else
          {
            abscissaComponent = TupleResultUtilities.findComponentById( obsResult, Kalypso1D2DDictConstants.DICT_COMPONENT_WATERLEVEL );
            ordinateComponent = TupleResultUtilities.findComponentById( obsResult, Kalypso1D2DDictConstants.DICT_COMPONENT_DISCHARGE );
            if( abscissaComponent != null && ordinateComponent != null )
            {
              boundary.setTypeStr( TELEMAC_BOUNDARY_TYPE_H_WQ );
              boundary.setName( TELEMAC_Q_TYPE );
              boundary.setUnit( TELEMAC_UNIT_METER_QUBIC_PER_SEC );
              boundary.setWQ( true );
              boundary.setAbscissaComponent( abscissaComponent );
              boundary.setOrdinateComponent( ordinateComponent );
            }
          }

        }
      }
    }

    return boundary;
  }

  private void writeBoundNodes( ) throws IOException
  {
    int lastBoundary = 0;
    int boundNodeCount = 1;
    for( GM_Position node : m_listBoundNodes )
    {
      Integer actBoundary = m_mapPositionsToConditions.get( node );
      BoundaryConditionTelemac boundType = resolveBoundaryType( actBoundary );
      writeNode( node, boundType, boundNodeCount );
      if( lastBoundary != actBoundary && !m_listBoundaries.contains( boundType ) )
      {
        m_listBoundaries.add( boundType );
      }
      lastBoundary = actBoundary;
      boundNodeCount++;
    }
  }

  /**
   *
   */
  private Set<GM_Position> countNodes( List<GM_Triangle> triangles )
  {
    Set<GM_Position> lSetPositions = new HashSet<>();
    for( final GM_Triangle lTri : triangles )
    {
      lSetPositions.addAll( Arrays.asList( lTri.getExteriorRing() ) );
    }
    return lSetPositions;
  }

  private boolean isNodeOnBound( final IFE1D2DNode pNode )
  {
    boolean lBoolResult = false;
    GM_Position position = pNode.getPoint().getPosition();
    if( m_mapPositionsToConditions.get( position ) != null || !isNodeInCalcUnit( pNode ) )
    {
      return lBoolResult;
    }
    int positionId;
    if( !isNodeOnContiLine( pNode ) )
    {
      positionId = OUTER_CLOSED_POSITION_ID;
    }
    else{
      positionId = m_intGlobalContiId;
    }
    final IFE1D2DEdge[] lContainers = pNode.getLinkedEdges();
    for( final Object lContainerObject : lContainers )
    {
      if( lContainerObject instanceof IFE1D2DEdge )
      { 
        IFE1D2DEdge lEdge = (IFE1D2DEdge)lContainerObject;

        final IFE1D2DElement[] adjacentElements = lEdge.getLinkedElements();
        if( adjacentElements.length < 2 || (!isElementInCalcUnit( adjacentElements[ 0 ] ) && isElementInCalcUnit( adjacentElements[1] ))
            || (!isElementInCalcUnit( adjacentElements[ 1 ] ) && isElementInCalcUnit( adjacentElements[ 0 ] )) )
        {
          if( m_mapPositionsToConditions.get( position ) == null )
          {
            m_mapPositionsToConditions.put( position, positionId );
            m_listBoundNodes.add( position );
          }
          lBoolResult = true;
          if( !m_boundFound && counterClockWise( pNode, lEdge ) )
          {
            continue;
          }
          m_boundFound = true;
          for( final Object lNodeObj : lEdge.getNodes() )
          {
            if( lNodeObj instanceof IFE1D2DNode )
            {
              final IFE1D2DNode lNodeNew = (IFE1D2DNode)lNodeObj;
              if( !pNode.equals( lNodeNew ) && isNodeOnBound( lNodeNew ) )
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

  private boolean counterClockWise( final IFE1D2DNode pNode, final IFE1D2DEdge lEdge )
  {
    final IFE1D2DElement[] adjacentElements = lEdge.getLinkedElements();
    IFE1D2DElement element = null;
    for( int i = 0; i < adjacentElements.length; i++ )
    {
      IFE1D2DElement ife1d2dElement = adjacentElements[ i ];
      if( adjacentElements.length < 2 || m_calculationUnit.contains( ife1d2dElement ) )
      {
        element = ife1d2dElement;
        break;
      }
    }
    IFE1D2DNode lNodeNew0 = lEdge.getNodes()[ 0 ];
    if( lNodeNew0.equals( pNode ) )
    {
      lNodeNew0 = lEdge.getNodes()[ 1 ];
    }
    
    IFE1D2DNode lNodeNew1 = element.getNodes()[ 2 ];
    for( int i = 0; i < element.getNodes().length - 1; i++ )
    {
      if( !lNodeNew0.equals( element.getNodes()[ i ] ) && !pNode.equals( element.getNodes()[ i ] ) ) 
      {
        lNodeNew1 = element.getNodes()[ i ];
        break;
      }
      
    }
    if( orientation( pNode.getPoint().getPosition(), lNodeNew0.getPoint().getPosition(), lNodeNew1.getPoint().getPosition() ) < 0 )
    {
      return true;
    }
    return false;
  }

  private int orientation( final GM_Position pos1, final GM_Position pos2, final GM_Position pos3 )
  {
    final double s_a = signedArea( pos1, pos2, pos3 );
    return s_a > 0 ? 1 : (s_a < 0 ? -1 : 0);
  }

  private static double signedArea( final GM_Position pos1, final GM_Position pos2, final GM_Position pos3 )
  {
    return (pos1.getX() * (pos2.getY() - pos3.getY()) + pos2.getX() * (pos3.getY() - pos1.getY()) + pos3.getX() * (pos1.getY() - pos2.getY()));
  }

  private boolean isNodeInCalcUnit( IFE1D2DNode pNode )
  {
    for( final IFE1D2DElement lElement : pNode.getAdjacentElements() )
    {
      if( isElementInCalcUnit( lElement ) )
//      if( m_calculationUnit.contains( lElement ) )
      {
        return true;
      }
    }
    return false;
  }

  private boolean isElementInCalcUnit( IFE1D2DElement lElement )
  {
    if( m_setElementsIds.contains( lElement.getId() ) )
      return true;
    
    return false;
  }

  private int writeNode( final GM_Position pNodeAct, final BoundaryConditionTelemac boundType, final int boundNodeCount ) throws IOException
  {
    m_formatterBoundNodes.format( "  %s  0.000  0.000  0.000  0.0   2  0.000  0.000  0.000     %8d     %8d%n", boundType.getTypeStr(), m_listAllNodes.indexOf( pNodeAct ) + 1, boundNodeCount ); //$NON-NLS-1$
    FormatterUtils.checkIoException( m_formatterBoundNodes );
    return m_intCounterNodes++;

  }

  public final long getGlobalMinX( )
  {
    return m_globalMinX;
  }

  public final long getGlobalMinY( )
  {
    return m_globalMinY;
  }

  public final List<GM_Position> getListAdditionalOuputCoord( )
  {
    return m_listAdditionalOuputCoord;
  }

  public final void setListAdditionalOuputCoord( final List<GM_Position> listAdditionalOuputCoord )
  {
    m_listAdditionalOuputCoord = listAdditionalOuputCoord;
  }

}
