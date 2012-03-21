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
package org.kalypso.kalypsomodel1d2d.conv.results;

import java.awt.Color;
import java.math.BigDecimal;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import org.kalypso.kalypsomodel1d2d.conv.TeschkeRelationConverter;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IContinuityLine2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.ITeschkeFlowRelation;
import org.kalypso.kalypsomodel1d2d.schema.binding.results.GMLNodeResult;
import org.kalypso.kalypsomodel1d2d.schema.binding.results.INodeResult;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.ProfilFactory;
import org.kalypso.model.wspm.core.profil.util.ProfilObsHelper;
import org.kalypso.model.wspm.core.profil.util.ProfilUtil;
import org.kalypso.model.wspm.core.profil.wrappers.IProfileRecord;
import org.kalypso.model.wspm.core.util.WspmProfileHelper;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.schema.schemata.IWspmTuhhQIntervallConstants;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.gml.binding.math.IPolynomial1D;
import org.kalypsodeegree_impl.gml.binding.math.PolynomialUtilities;
import org.kalypsodeegree_impl.tools.GeometryUtilities;

/**
 * @author Thomas Jung
 */
public class NodeResultHelper
{
  public static final String AMOUNT_OF_CLASSES_PREFIX = "defaultamountclasses"; //$NON-NLS-1$ 

  public static final String VALUE_MAX_PREFIX = "defaultvaluemax"; //$NON-NLS-1$ 

  public static final String VALUE_MIN_PREFIX = "defaultvaluemin"; //$NON-NLS-1$ 

  public static final String COLOR_MAX_PREFIX = "defaultcolormax"; //$NON-NLS-1$ 

  public static final String COLOR_MIN_PREFIX = "defaultcolormin"; //$NON-NLS-1$ 

  public static final String COLOR_MAP_PREFIX = "stepcolormap"; //$NON-NLS-1$ 

  public static final String WAVE_PERIOD_TYPE = "WavePeriod"; //$NON-NLS-1$

  public static final String WAVE_HSIG_TYPE = "WaveHsig"; //$NON-NLS-1$

  public static final String DEPTH_TYPE = "Depth"; //$NON-NLS-1$

  public static final String WATERLEVEL_TYPE = "Waterlevel"; //$NON-NLS-1$

  public static final String WAVE_DIRECTION_TYPE = "WaveDirection"; //$NON-NLS-1$

  public static final String VELO_TYPE = "Velo"; //$NON-NLS-1$

  public static final String POLYGON_TYPE = "Polygon"; //$NON-NLS-1$

  public static final String LINE_TYPE = "Line"; //$NON-NLS-1$

  public static final String NODE_TYPE = "Node"; //$NON-NLS-1$

  private static Map<String, Map<String, Map<String, Object>>> m_styleSettings = new HashMap<String, Map<String, Map<String, Object>>>();

  private static Map<String, Map<String, Object>> m_stepStyleSettings = new HashMap<String, Map<String, Object>>();

  public static final String[] NodeStyleTypes = new String[] { VELO_TYPE, WATERLEVEL_TYPE, DEPTH_TYPE, WAVE_HSIG_TYPE, WAVE_PERIOD_TYPE, WAVE_DIRECTION_TYPE };

  public static final Color DEFAULT_COLOR_MIN = new Color( Integer.parseInt( "ff0000", 16 ) ); //$NON-NLS-1$

  public static final Color DEFAULT_COLOR_MAX = new Color( Integer.parseInt( "0000ff", 16 ) ); //$NON-NLS-1$

  public static final Double DEFAULT_AMOUNT_CLASSES = 100.;

  public static final String NODESTYLE_TEMPLATE = "NODESTYLE_TEMPLATE"; //$NON-NLS-1$ 

  public static final String VECTORFACTOR = "VECTORFACTOR"; //$NON-NLS-1$ 

  public static final String SIZE_NORM_NODE_FUNC = "SIZE_NORM"; //$NON-NLS-1$

  public static final String VELOCITY = "velocity"; //$NON-NLS-1$

  /**
   * sets the mid-side node's water level and depth by interpolation between the corner nodes.
   * 
   * @param nodeDown
   *          first node of the corresponding arc.
   * @param nodeUp
   *          second node of the corresponding arc.
   * @param midsideNode
   *          the mid-side node
   */
  public static void checkMidsideNodeData( final INodeResult nodeDown, final INodeResult nodeUp, final INodeResult midsideNode )
  {
    // TODO check what to do if some of the nodes is null
    // (in the moment exception is thrown...)

    // TODO: handle velocity

    if( nodeDown.getDepth() <= 0 && nodeUp.getDepth() <= 0 )
    {
      interpolateMidsideNodeData( nodeDown, nodeUp, midsideNode );
      return;
    }

    if( nodeDown.getDepth() > 0 && nodeUp.getDepth() <= 0 )
    {
      assignMidsideNodeData( nodeDown, midsideNode ); // assignment leads into extrapolation of the water level!!
      return;
    }

    if( nodeDown.getDepth() <= 0 && nodeUp.getDepth() > 0 )
    {
      assignMidsideNodeData( nodeUp, midsideNode ); // assignment leads into extrapolation of the water level!!
    }

    if( nodeDown.getDepth() > 0 && nodeUp.getDepth() > 0 )
    {
      interpolateMidsideNodeData( nodeDown, nodeUp, midsideNode );
      return;
    }
  }

  /**
   * interpolates the water level for the midside node by using the water levels of the corner nodes. The depth will be
   * calculated as well, using the interpolated water level.
   * 
   * @param nodeDown
   *          first node of the arc on which the corner node lies.
   * @param nodeUp
   *          second node
   * @param midsideNode
   *          the midside node
   */
  private static void interpolateMidsideNodeData( final INodeResult nodeDown, final INodeResult nodeUp, final INodeResult midsideNode )
  {
    final List<Double> waterlevels = new LinkedList<Double>();

    waterlevels.add( nodeDown.getWaterlevel() );
    waterlevels.add( nodeUp.getWaterlevel() );

    final double waterlevel = getMeanValue( waterlevels );
    midsideNode.setWaterlevel( waterlevel );

    final double depth = waterlevel - midsideNode.getPoint().getZ();
    // double waveDirectionDown = nodeDown.getWaveDirection();
    // double waveDirectionUp = nodeUp.getWaveDirection();
    // if( waveDirectionDown > 180 || waveDirectionDown < -180 ){
    //
    // }
    midsideNode.setWaveDirection( 0 );
    midsideNode.setWaveHsig( 0 );
    midsideNode.setWavePeriod( 0 );
    midsideNode.setDepth( depth );
  }

  private static double getMeanValue( final List<Double> values )
  {
    double sum = 0;
    for( int i = 0; i < values.size(); i++ )
    {
      sum = sum + values.get( i );
    }
    return sum / values.size();
  }

  private static void assignMidsideNodeData( final INodeResult node, final INodeResult midsideNode )
  {
    final double waterlevel = node.getWaterlevel();
    midsideNode.setWaterlevel( waterlevel );

    final double depth = waterlevel - midsideNode.getPoint().getZ();
    midsideNode.setDepth( depth );

    // set the velocity of the midside node to a dummy value derived from the corner node, if the midside node itself
    // has no velocity value.
    // REMARK: This is dangerous, because we don't have any vector data anymore!
    // TODO: check if this is really necessary

    // final List<Double> velocity = midsideNode.getVelocity();
    // if( velocity.get( 0 ) == 0.0 && velocity.get( 1 ) == 0.0 )
    // {
    // final List<Double> velocities = new LinkedList<Double>();
    // velocities.add( node.getAbsoluteVelocity() );
    // velocities.add( 0.0 );
    //
    // setVelocity( midsideNode, getMeanValue( velocities ) );
    // }
  }

  /**
   * returns a simplified profile curve of a 1d-node, already cut at the intersection points with the water level
   * 
   * @param nodeResult
   *          1d-node
   */
  public static GM_Curve getProfileCurveFor1dNode( final IProfileFeature profile ) throws Exception
  {
    final IProfil profil = profile.getProfil();

    /* cut the profile at the intersection points with the water level */
    // get the intersection points
    // get the crs from the profile-gml
    final String srsName = profile.getSrsName();
    final String crs = srsName == null ? KalypsoDeegreePlugin.getDefault().getCoordinateSystem() : srsName;

    // final double waterlevel = nodeResult.getWaterlevel();

    /* REMARK: now we us the whole profile in order to get a bigger area for the flood modeler */

    // final GM_Point[] points = WspmProfileHelper.calculateWspPoints( profil, waterlevel );
    // final GM_Curve curve = cutProfileAtWaterlevel( waterlevel, profil, crs );
    final GM_Curve curve = ProfilUtil.getLine( profil, crs );
    final GM_Curve transformCurve = (GM_Curve) curve.transform( KalypsoDeegreePlugin.getDefault().getCoordinateSystem() );
    /* simplify the profile */
    final double epsThinning = 1.0;
    final GM_Curve thinnedCurve = GeometryUtilities.getThinnedCurve( transformCurve, epsThinning );
    thinnedCurve.setCoordinateSystem( KalypsoDeegreePlugin.getDefault().getCoordinateSystem() );

    /* set the water level as new z-coordinate of the profile line */
    // return GeometryUtilities.setValueZ( thinnedCurve.getAsLineString(), waterlevel );
    return thinnedCurve;

    // TODO: add some gml file for the points
    // TODO: King
  }

  public static BigDecimal getCrossSectionArea( final ITeschkeFlowRelation teschkeRelation, final BigDecimal depth )
  {
    final List<IPolynomial1D> polynomials = teschkeRelation.getPolynomials();
    final TeschkeRelationConverter teschkeConv = new TeschkeRelationConverter( polynomials.toArray( new IPolynomial1D[] {} ) );

    final IPolynomial1D[] polyArea = teschkeConv.getPolynomialsByType( IWspmTuhhQIntervallConstants.DICT_PHENOMENON_AREA );
    if( polyArea == null )
      return null;

    // TODO: for some reason there appears a depth that is not defined in the Polynomial Array!?
    final IPolynomial1D poly = PolynomialUtilities.getPoly( polyArea, depth.doubleValue() );
    if( poly == null )
      return null;
    final double computeResult = poly.computeResult( depth.doubleValue() );

    return new BigDecimal( computeResult ).setScale( 4, BigDecimal.ROUND_HALF_UP );
  }

  /**
   * gets the x-coordinate of the zero point of a line defined by y1 (>0), y2 (<0) and the difference of the
   * x-coordinates (x2-x1) = dx12.
   * 
   * @param dx12
   *          distance between x1 and x2.
   * @param y1
   *          the y-value of point 1 of the line. It has to be always > 0!
   * @param y2
   *          the y-value of point 2 of the line. It has to be always < 0!
   */
  public static double getZeroPoint( final double dx12, final double y1, final double y2 )
  {
    final double x3 = y1 / (Math.abs( y1 ) + Math.abs( y2 )) * dx12;
    return x3;
  }

  public static boolean checkTriangleArc( final INodeResult node1, final INodeResult node2 )
  {
    /* get the split point (inundation point) */
    if( node1.isWet() && !node2.isWet() || !node1.isWet() && node2.isWet() )
      return true;
    else
      return false;
  }

  public static GM_Curve getCurveForBoundaryLine( final GM_Point[] linePoints, final double waterlevel, final String crs ) throws GM_Exception, Exception
  {
    // we create a profile in order to use already implemented methods
    final IProfil boundaryProfil = ProfilFactory.createProfil( IWspmTuhhConstants.PROFIL_TYPE_PASCHE );
    boundaryProfil.addPointProperty( ProfilObsHelper.getPropertyFromId( boundaryProfil, IWspmConstants.POINT_PROPERTY_BREITE ) );
    boundaryProfil.addPointProperty( ProfilObsHelper.getPropertyFromId( boundaryProfil, IWspmConstants.POINT_PROPERTY_HOEHE ) );
    boundaryProfil.addPointProperty( ProfilObsHelper.getPropertyFromId( boundaryProfil, IWspmConstants.POINT_PROPERTY_RECHTSWERT ) );
    boundaryProfil.addPointProperty( ProfilObsHelper.getPropertyFromId( boundaryProfil, IWspmConstants.POINT_PROPERTY_HOCHWERT ) );

    double width = 0;
    for( int i = 0; i < linePoints.length; i++ )
    {
      final GM_Point geoPoint = linePoints[i];

      if( i > 0 )
        width = width + geoPoint.distance( linePoints[i - 1] );

      final IProfileRecord point = boundaryProfil.createProfilPoint();

      /* calculate the width of the intersected profile */
      // sort intersection points by width
      point.setValue( ProfilObsHelper.getPropertyFromId( point, IWspmConstants.POINT_PROPERTY_BREITE ), width );
      point.setValue( ProfilObsHelper.getPropertyFromId( point, IWspmConstants.POINT_PROPERTY_HOEHE ), geoPoint.getZ() );
      point.setValue( ProfilObsHelper.getPropertyFromId( point, IWspmConstants.POINT_PROPERTY_RECHTSWERT ), geoPoint.getX() );
      point.setValue( ProfilObsHelper.getPropertyFromId( point, IWspmConstants.POINT_PROPERTY_HOCHWERT ), geoPoint.getY() );

      boundaryProfil.addPoint( point );
    }
    return WspmProfileHelper.cutProfileAtWaterlevel( waterlevel, boundaryProfil, crs );

  }

  public static INodeResult getNodeResult( final GM_Point point, final FeatureList resultList, final double searchDistance )
  {
    final Feature feature = GeometryUtilities.findNearestFeature( point, searchDistance, resultList, GMLNodeResult.QNAME_PROP_LOCATION );
    return (INodeResult) feature.getAdapter( INodeResult.class );
  }

  public static GM_Point[] getLinePoints( final IContinuityLine2D continuityLine2D )
  {
    final List<IFE1D2DNode> nodes = continuityLine2D.getNodes();
    final GM_Point[] points = new GM_Point[nodes.size()];
    int i = 0;
    for( final IFE1D2DNode node : nodes )
    {
      points[i++] = node.getPoint();
    }
    return points;
  }

  public static Map<String, Object> getSldSettingsMapForStyleStep( final String styleType, final String stepDate )
  {
    Map<String, Map<String, Object>> mapStep = m_styleSettings.get( stepDate.toLowerCase() );
    if( mapStep != null )
    {
      Map<String, Object> mapStyle = mapStep.get( styleType.toLowerCase() );
      if( mapStyle == null )
      {
        mapStyle = createMapStyle();
        mapStep.put( styleType.toLowerCase(), mapStyle );
      }
      return mapStyle;
    }
    else
    {
      mapStep = createMapStep( styleType );
      m_styleSettings.put( stepDate.toLowerCase(), mapStep );
      return mapStep.get( styleType.toLowerCase() );
    }
  }

  public static Map<String, Object> getSldSettingsMapForStep( final String stepDate )
  {
    Map<String, Object> mapStep = m_stepStyleSettings.get( stepDate.toLowerCase() );
    if( mapStep == null )
    {
      mapStep = createMapStyle();
      m_stepStyleSettings.put( stepDate.toLowerCase(), mapStep );
    }
    return mapStep;
  }

  public static void setSldValueForStep( final String stepDate, final String key, final Object value )
  {
    Map<String, Object> mapStep = m_stepStyleSettings.get( stepDate.toLowerCase() );
    if( mapStep != null )
    {
      mapStep.put( key.toLowerCase(), value );
    }
    else
    {
      mapStep = createMapStyle();
      mapStep.put( key.toLowerCase(), value );
      m_stepStyleSettings.put( stepDate.toLowerCase(), mapStep );
    }
  }

  private static Map<String, Map<String, Object>> createMapStep( final String styleType )
  {
    final Map<String, Map<String, Object>> mapStep = new HashMap<String, Map<String, Object>>();
    final Map<String, Object> mapStyle = createMapStyle();
    mapStep.put( styleType.toLowerCase(), mapStyle );
    return mapStep;
  }

  private static Map<String, Object> createMapStyle( )
  {
    final Map<String, Object> mapStyle = new HashMap<String, Object>();
    fillMapStyleWithDefaults( mapStyle );
    return mapStyle;
  }

  private static void fillMapStyleWithDefaults( final Map<String, Object> mapStyle )
  {
    for( final String key : NodeStyleTypes )
    {
      mapStyle.put( COLOR_MIN_PREFIX + key.toLowerCase(), DEFAULT_COLOR_MIN );
      mapStyle.put( COLOR_MAX_PREFIX + key.toLowerCase(), DEFAULT_COLOR_MAX );
      mapStyle.put( VALUE_MIN_PREFIX + key.toLowerCase(), 0. );
      mapStyle.put( VALUE_MAX_PREFIX + key.toLowerCase(), 100. );
      mapStyle.put( AMOUNT_OF_CLASSES_PREFIX + key.toLowerCase(), DEFAULT_AMOUNT_CLASSES );
    }
  }

  public static void setSldValueForStyleStep( final String styleType, final String stepDate, final String key, final Object value )
  {
    Map<String, Map<String, Object>> mapStep = m_styleSettings.get( stepDate.toLowerCase() );
    if( mapStep != null )
    {
      Map<String, Object> mapStyle = mapStep.get( styleType.toLowerCase() );
      if( mapStyle == null )
      {
        mapStyle = createMapStyle();
        mapStep.put( styleType.toLowerCase(), mapStyle );
      }
      mapStyle.put( key.toLowerCase(), value );
    }
    else
    {
      mapStep = createMapStep( styleType );
      final Map<String, Object> mapStyle = mapStep.get( styleType.toLowerCase() );
      mapStyle.put( key.toLowerCase(), value );
      m_styleSettings.put( stepDate.toLowerCase(), mapStep );
    }
  }

}
