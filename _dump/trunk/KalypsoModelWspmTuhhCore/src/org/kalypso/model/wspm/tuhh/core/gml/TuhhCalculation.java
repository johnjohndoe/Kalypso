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
package org.kalypso.model.wspm.tuhh.core.gml;

import java.math.BigDecimal;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;

import javax.xml.namespace.QName;

import org.kalypso.gmlschema.IGMLSchema;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.profil.util.ProfilUtil;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.ogc.gml.om.ObservationFeatureFactory;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.gml.binding.commons.AbstractFeatureBinder;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

import com.sun.org.apache.xerces.internal.jaxp.datatype.XMLGregorianCalendarImpl;

/**
 * Binding class for CalculationReibConstWspmTuhhSteadyState AND CalculationWspmTuhhSteadyState
 * 
 * @author Gernot Belger
 */
public class TuhhCalculation extends AbstractFeatureBinder implements IWspmConstants, IWspmTuhhConstants
{
  public static enum ExeVersion
  {
    _2_0_6_6,
    _2_1_0_0,
    _2_1_1_0,
    _2_1_2_0,
    _2_1_2_1,
    _2_1_4_0,
    _2_1_4_1
  }

  public static final QName QNAME_TUHH_CALC = new QName( NS_WSPM_TUHH, "CalculationWspmTuhhSteadyState" );

  public static final QName QNAME_TUHH_CALC_REIB_CONST = new QName( NS_WSPM_TUHH, "CalculationReibConstWspmTuhhSteadyState" );

  private static final QName QNAME_PROP_POLYNOME_MEMBER = new QName( NS_WSPM_TUHH, "calcPolynomesMember" );

  private static final QName QNAME_PROP_WATERLEVEL_PARAMS = new QName( NS_WSPM_TUHH, "waterlevelParameterMember" );

  private static final QName QNAME_PROP_EXE_VERSION = new QName( NS_WSPM_TUHH, "exeVersion" );

  private static final QName QNAME_PROP_SPECIAL_OPTIONS_MEMBER = new QName( NS_WSPM_TUHH, "specialOptionsMember" );

  private static final QName QNAME_PROP_SPECIAL_PROP_USE_EXTREME_ROUGHNESS = new QName( NS_WSPM_TUHH, "useExtremeRoughness" );

  public static enum MODE
  {
    WATERLEVEL,
    BF_UNIFORM,
    BF_NON_UNIFORM,
    REIB_KONST;
  }

  public static enum FLIESSGESETZ
  {
    DARCY_WEISBACH_OHNE_FORMEINFLUSS,
    DARCY_WEISBACH_MIT_FORMEINFLUSS,
    MANNING_STRICKLER;
  }

  public static enum START_KONDITION_KIND
  {
    CRITICAL_WATER_DEPTH,
    UNIFORM_BOTTOM_SLOPE,
    WATERLEVEL;
  }

  public static enum WSP_ITERATION_TYPE
  {
    SIMPLE,
    EXACT;
  }

  public static enum VERZOEGERUNSVERLUST_TYPE
  {
    DVWK,
    BJOERNSEN,
    DFG,
    NON;
  }

  public static enum REIBUNGSVERLUST_TYPE
  {
    TRAPEZ_FORMULA,
    GEOMETRIC_FORMULA
  }

  public TuhhCalculation( final Feature calcFeature )
  {
    super( calcFeature, new QName( NS_WSPM, "AbstractCalculation" ) );
  }

  public void setCalcCreation( final String user, final Date date )
  {
    final QName qname = new QName( NS_WSPM, "calcCreationMember" );

    Feature calcCreationFeature = getProperty( qname, Feature.class );

    if( calcCreationFeature == null )
    {
      // neues machen
      final GMLWorkspace workspace = getFeature().getWorkspace();
      final IGMLSchema schema = workspace.getGMLSchema();
      final IFeatureType featureType = schema.getFeatureType( new QName( NS_WSPM, "CalcCreation" ) );
      final IRelationType parentRelation = (IRelationType) getFeature().getFeatureType().getProperty( qname );
      calcCreationFeature = workspace.createFeature( getFeature(), parentRelation, featureType );
      setProperty( qname, calcCreationFeature );
    }

    calcCreationFeature.setProperty( new QName( NS_WSPM, "user" ), user );

    final Calendar calendar = Calendar.getInstance();
    calendar.setTime( date );

    calcCreationFeature.setProperty( new QName( NS_WSPM, "date" ), new XMLGregorianCalendarImpl( (GregorianCalendar) calendar ) );
  }

  public void setReachRef( final TuhhReach reach )
  {
    setProperty( new QName( NS_WSPM_TUHH, "reachWspmTuhhSteadyStateMember" ), reach.getFeature().getId() );
  }

  public void setFliessgesetz( final FLIESSGESETZ gesetz )
  {
    setProperty( new QName( NS_WSPM_TUHH, "fliessgesetz" ), gesetz.name() );
  }

  public FLIESSGESETZ getFliessgesetz( )
  {
    switch( getCalcMode() )
    {
      case REIB_KONST:
        return FLIESSGESETZ.DARCY_WEISBACH_OHNE_FORMEINFLUSS;

      default:
        final String property = getProperty( new QName( NS_WSPM_TUHH, "fliessgesetz" ), String.class );
        return FLIESSGESETZ.valueOf( property );
    }

  }

  public void setSubReachDef( final double startStation, final double endStation )
  {
    final QName qname = new QName( NS_WSPM_TUHH, "subReachDefinitionMember" );
    final Feature subReachFeature = FeatureHelper.getSubFeature( getFeature(), qname );

    final BigDecimal bigStart = ProfilUtil.stationToBigDecimal( startStation );
    final BigDecimal bigEnd = ProfilUtil.stationToBigDecimal( endStation );
    subReachFeature.setProperty( new QName( NS_WSPM_TUHH, "startStation" ), bigStart );
    subReachFeature.setProperty( new QName( NS_WSPM_TUHH, "endStation" ), bigEnd );
  }

  public BigDecimal getStartStation( )
  {
    final QName qname = new QName( NS_WSPM_TUHH, "subReachDefinitionMember" );
    final Feature subReachFeature = FeatureHelper.getSubFeature( getFeature(), qname );

    return (BigDecimal) subReachFeature.getProperty( new QName( NS_WSPM_TUHH, "startStation" ) );
  }

  public BigDecimal getEndStation( )
  {
    final QName qname = new QName( NS_WSPM_TUHH, "subReachDefinitionMember" );
    final Feature subReachFeature = FeatureHelper.getSubFeature( getFeature(), qname );

    return (BigDecimal) subReachFeature.getProperty( new QName( NS_WSPM_TUHH, "endStation" ) );
  }

  public void setStartCondition( final START_KONDITION_KIND type, final double startWsp, final double startSlope )
  {
    final QName qname = new QName( NS_WSPM_TUHH, "startConditionMember" );
    final Feature conditionFeature = FeatureHelper.getSubFeature( getFeature(), qname );

    conditionFeature.setProperty( new QName( NS_WSPM_TUHH, "kind" ), type.name() );
    conditionFeature.setProperty( new QName( NS_WSPM_TUHH, "waterlevel" ), new Double( startWsp ) );
    conditionFeature.setProperty( new QName( NS_WSPM_TUHH, "bottomSlope" ), new BigDecimal( startSlope ) );
  }

  public START_KONDITION_KIND getStartKind( )
  {
    switch( getCalcMode() )
    {
      case REIB_KONST:
        return START_KONDITION_KIND.UNIFORM_BOTTOM_SLOPE;

      default:
        final QName qname = new QName( NS_WSPM_TUHH, "startConditionMember" );
        final Feature conditionFeature = FeatureHelper.getSubFeature( getFeature(), qname );

        return START_KONDITION_KIND.valueOf( (String) conditionFeature.getProperty( new QName( NS_WSPM_TUHH, "kind" ) ) );
    }
  }

  public Double getStartWaterlevel( )
  {
    switch( getCalcMode() )
    {
      case REIB_KONST:
        return null;

      default:
        final QName qname = new QName( NS_WSPM_TUHH, "startConditionMember" );
        final Feature conditionFeature = FeatureHelper.getSubFeature( getFeature(), qname );

        return (Double) conditionFeature.getProperty( new QName( NS_WSPM_TUHH, "waterlevel" ) );
    }
  }

  public void setStartSlope( final BigDecimal slope )
  {
    switch( getCalcMode() )
    {
      case REIB_KONST:
        setProperty( new QName( NS_WSPM_TUHH, "bottomSlope" ), slope );
        return;

      default:
        final QName qname = new QName( NS_WSPM_TUHH, "startConditionMember" );
        final Feature conditionFeature = FeatureHelper.getSubFeature( getFeature(), qname );
        conditionFeature.setProperty( new QName( NS_WSPM_TUHH, "bottomSlope" ), slope );
        return;
    }
  }

  public BigDecimal getStartSlope( )
  {
    switch( getCalcMode() )
    {
      case REIB_KONST:
        return getProperty( new QName( NS_WSPM_TUHH, "bottomSlope" ), BigDecimal.class );

      default:
        final QName qname = new QName( NS_WSPM_TUHH, "startConditionMember" );
        final Feature conditionFeature = FeatureHelper.getSubFeature( getFeature(), qname );
        return (BigDecimal) conditionFeature.getProperty( new QName( NS_WSPM_TUHH, "bottomSlope" ) );
    }
  }

  public void setWaterlevelParameters( final WSP_ITERATION_TYPE iterationType, final VERZOEGERUNSVERLUST_TYPE verzType, final REIBUNGSVERLUST_TYPE reibType, final boolean doCalcBridges, final boolean doCalcBarrages, final boolean useExtremeRoughness )
  {
    final Feature parameterFeature = FeatureHelper.getSubFeature( getFeature(), QNAME_PROP_WATERLEVEL_PARAMS );

    parameterFeature.setProperty( new QName( NS_WSPM_TUHH, "wspIteration" ), iterationType.name() );
    parameterFeature.setProperty( new QName( NS_WSPM_TUHH, "verzoegerungsverlust" ), verzType.name() );
    parameterFeature.setProperty( new QName( NS_WSPM_TUHH, "reibungsverlust" ), reibType.name() );

    final Feature specialFeature = FeatureHelper.getSubFeature( parameterFeature, QNAME_PROP_SPECIAL_OPTIONS_MEMBER );
    specialFeature.setProperty( new QName( NS_WSPM_TUHH, "doCalcBridges" ), new Boolean( doCalcBridges ) );
    specialFeature.setProperty( new QName( NS_WSPM_TUHH, "doCalcBarrages" ), new Boolean( doCalcBarrages ) );
    specialFeature.setProperty( QNAME_PROP_SPECIAL_PROP_USE_EXTREME_ROUGHNESS, new Boolean( useExtremeRoughness ) );
  }

  public WSP_ITERATION_TYPE getIterationType( )
  {
    final Feature parameterFeature = FeatureHelper.getSubFeature( getFeature(), QNAME_PROP_WATERLEVEL_PARAMS );

    return WSP_ITERATION_TYPE.valueOf( (String) parameterFeature.getProperty( new QName( NS_WSPM_TUHH, "wspIteration" ) ) );
  }

  public VERZOEGERUNSVERLUST_TYPE getVerzoegerungsverlust( )
  {
    switch( getCalcMode() )
    {
      case REIB_KONST:
        return VERZOEGERUNSVERLUST_TYPE.NON;

      default:
        final Feature parameterFeature = FeatureHelper.getSubFeature( getFeature(), QNAME_PROP_WATERLEVEL_PARAMS );

        return VERZOEGERUNSVERLUST_TYPE.valueOf( (String) parameterFeature.getProperty( new QName( NS_WSPM_TUHH, "verzoegerungsverlust" ) ) );
    }
  }

  public REIBUNGSVERLUST_TYPE getReibungsverlust( )
  {
    final Feature parameterFeature = FeatureHelper.getSubFeature( getFeature(), QNAME_PROP_WATERLEVEL_PARAMS );

    return REIBUNGSVERLUST_TYPE.valueOf( (String) parameterFeature.getProperty( new QName( NS_WSPM_TUHH, "reibungsverlust" ) ) );
  }

  public boolean isCalcBridges( )
  {
    final Feature parameterFeature = FeatureHelper.getSubFeature( getFeature(), QNAME_PROP_WATERLEVEL_PARAMS );
    final Feature specialFeature = FeatureHelper.getSubFeature( parameterFeature, QNAME_PROP_SPECIAL_OPTIONS_MEMBER );
    final Boolean value = (Boolean) specialFeature.getProperty( new QName( NS_WSPM_TUHH, "doCalcBridges" ) );
    if( value == null )
      return false;

    return value.booleanValue();
  }

  public boolean isCalcBarrages( )
  {
    final Feature parameterFeature = FeatureHelper.getSubFeature( getFeature(), QNAME_PROP_WATERLEVEL_PARAMS );
    final Feature specialFeature = FeatureHelper.getSubFeature( parameterFeature, QNAME_PROP_SPECIAL_OPTIONS_MEMBER );
    final Boolean value = (Boolean) specialFeature.getProperty( new QName( NS_WSPM_TUHH, "doCalcBarrages" ) );
    if( value == null )
      return false;

    return value.booleanValue();
  }

  public boolean isUseExtremeRoughness( )
  {
    final Feature parameterFeature = FeatureHelper.getSubFeature( getFeature(), QNAME_PROP_WATERLEVEL_PARAMS );
    final Feature specialFeature = FeatureHelper.getSubFeature( parameterFeature, QNAME_PROP_SPECIAL_OPTIONS_MEMBER );
    final Boolean useExtremeRoughness = (Boolean) specialFeature.getProperty( QNAME_PROP_SPECIAL_PROP_USE_EXTREME_ROUGHNESS );
    if( useExtremeRoughness == null )
      return false;

    return useExtremeRoughness.booleanValue();
  }

  public void setCalcMode( final MODE mode )
  {
    final Feature feature = getFeature();
    feature.setProperty( new QName( NS_WSPM_TUHH, "mode" ), mode.name() );
  }

  public void setQRange( final double minQ, final double maxQ, final double Qstep )
  {
    final Feature feature = FeatureHelper.getSubFeature( getFeature(), new QName( NS_WSPM_TUHH, "runOffIntervalMember" ) );

    feature.setProperty( new QName( NS_WSPM_TUHH, "minimalRunOff" ), new Double( minQ ) );
    feature.setProperty( new QName( NS_WSPM_TUHH, "maximalRunOff" ), new Double( maxQ ) );
    feature.setProperty( new QName( NS_WSPM_TUHH, "runOffStep" ), new Double( Qstep ) );
  }

  public Double getMinQ( )
  {
    final Feature feature = FeatureHelper.getSubFeature( getFeature(), new QName( NS_WSPM_TUHH, "runOffIntervalMember" ) );
    return (Double) feature.getProperty( new QName( NS_WSPM_TUHH, "minimalRunOff" ) );
  }

  public Double getMaxQ( )
  {
    final Feature feature = FeatureHelper.getSubFeature( getFeature(), new QName( NS_WSPM_TUHH, "runOffIntervalMember" ) );
    return (Double) feature.getProperty( new QName( NS_WSPM_TUHH, "maximalRunOff" ) );
  }

  public Double getQStep( )
  {
    final Feature feature = FeatureHelper.getSubFeature( getFeature(), new QName( NS_WSPM_TUHH, "runOffIntervalMember" ) );
    return (Double) feature.getProperty( new QName( NS_WSPM_TUHH, "runOffStep" ) );
  }

  public void setRunOffRef( final String runOffRef )
  {
    getFeature().setProperty( new QName( NS_WSPM_TUHH, "runOffEventMember" ), runOffRef );
  }

  public TuhhReach getReach( )
  {
    final Feature reachFeature = FeatureHelper.resolveLink( getFeature(), new QName( NS_WSPM_TUHH, "reachWspmTuhhSteadyStateMember" ) );
    if( reachFeature == null )
      return null;

    return new TuhhReach( reachFeature );
  }

  public IObservation<TupleResult> getRunOffEvent( )
  {
    final Feature runOffEvent = FeatureHelper.resolveLink( getFeature(), new QName( NS_WSPM_TUHH, "runOffEventMember" ) );
    if( runOffEvent == null )
      return null;

    return ObservationFeatureFactory.toObservation( runOffEvent );
  }

  public MODE getCalcMode( )
  {
    final Feature feature = getFeature();
    if( QNAME_TUHH_CALC_REIB_CONST.equals( getQName() ) )
      return MODE.REIB_KONST;

    final String value = (String) feature.getProperty( new QName( NS_WSPM_TUHH, "mode" ) );
    return MODE.valueOf( value );
  }

  private Object getQName( )
  {
    return getFeature().getFeatureType().getQName();
  }

  /** Only valid for REIB_KONST mode. */
  public PolynomeProperties getPolynomeProperties( )
  {
    final Feature polyFeature = (Feature) getFeature().getProperty( QNAME_PROP_POLYNOME_MEMBER );
    if( polyFeature == null )
      return null;

    return new PolynomeProperties( polyFeature );
  }

  public ExeVersion getVersion( )
  {
    final Feature parameterFeature = FeatureHelper.getSubFeature( getFeature(), QNAME_PROP_WATERLEVEL_PARAMS );
    final String version = (String) parameterFeature.getProperty( QNAME_PROP_EXE_VERSION );
    if( version == null )
      return null;

    return ExeVersion.valueOf( version );
  }

  public void setVersion( final ExeVersion version )
  {
    final Feature parameterFeature = FeatureHelper.getSubFeature( getFeature(), QNAME_PROP_WATERLEVEL_PARAMS );
    parameterFeature.setProperty( QNAME_PROP_EXE_VERSION, version == null ? null : version.name() );
  }
}
