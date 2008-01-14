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
import org.kalypso.model.wspm.core.gml.WspmProfile;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.ogc.gml.om.ObservationFeatureFactory;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.gml.binding.commons.NamedFeatureHelper;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

import com.sun.org.apache.xerces.internal.jaxp.datatype.XMLGregorianCalendarImpl;

/**
 * Binding class for CalculationReibConstWspmTuhhSteadyState AND CalculationWspmTuhhSteadyState
 * 
 * @author Gernot Belger
 */
public class TuhhCalculation implements IWspmConstants, IWspmTuhhConstants
{
  public static final QName QNAME_TUHH_CALC = new QName( NS_WSPM_TUHH, "CalculationWspmTuhhSteadyState" );

  public static final QName QNAME_TUHH_CALC_REIB_CONST = new QName( NS_WSPM_TUHH, "CalculationReibConstWspmTuhhSteadyState" );

  private static final QName QNAME_PROP_POLYNOME_MEMBER = new QName( NS_WSPM_TUHH, "calcPolynomesMember" );

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

  private final Feature m_calcFeature;

  public TuhhCalculation( final Feature calcFeature )
  {
    if( calcFeature == null )
      throw new IllegalStateException( "calcFeature is null" );

    if( !QNAME_TUHH_CALC.equals( calcFeature.getFeatureType().getQName() ) && !QNAME_TUHH_CALC_REIB_CONST.equals( calcFeature.getFeatureType().getQName() ) )
      throw new IllegalStateException( "calcfeature is not of type: " + QNAME_TUHH_CALC );

    m_calcFeature = calcFeature;
  }

  public Feature getFeature( )
  {
    return m_calcFeature;
  }

  public String getName( )
  {
    return NamedFeatureHelper.getName( getFeature() );
  }

  public void setName( final String name )
  {
    NamedFeatureHelper.setName( getFeature(), name );
  }

  public String getDescription( )
  {
    return NamedFeatureHelper.getDescription( getFeature() );
  }

  public void setDescription( final String desc )
  {
    NamedFeatureHelper.setDescription( getFeature(), desc );
  }

  public void setCalcCreation( final String user, final Date date )
  {
    final QName qname = new QName( NS_WSPM, "calcCreationMember" );

    Feature calcCreationFeature = (Feature) m_calcFeature.getProperty( qname );

    if( calcCreationFeature == null )
    {
      // neues machen
      final GMLWorkspace workspace = m_calcFeature.getWorkspace();
      final IGMLSchema schema = workspace.getGMLSchema();
      final IFeatureType featureType = schema.getFeatureType( new QName( NS_WSPM, "CalcCreation" ) );
      final IRelationType parentRelation = (IRelationType) m_calcFeature.getFeatureType().getProperty( qname );
      calcCreationFeature = workspace.createFeature( m_calcFeature, parentRelation, featureType );
      m_calcFeature.setProperty( qname, calcCreationFeature );
    }

    calcCreationFeature.setProperty( new QName( NS_WSPM, "user" ), user );

    final Calendar calendar = Calendar.getInstance();
    calendar.setTime( date );

    calcCreationFeature.setProperty( new QName( NS_WSPM, "date" ), new XMLGregorianCalendarImpl( (GregorianCalendar) calendar ) );
  }

  public void setReachRef( final TuhhReach reach )
  {
    m_calcFeature.setProperty( new QName( NS_WSPM_TUHH, "reachWspmTuhhSteadyStateMember" ), reach.getFeature().getId() );
  }

  public void setFliessgesetz( final FLIESSGESETZ gesetz )
  {
    m_calcFeature.setProperty( new QName( NS_WSPM_TUHH, "fliessgesetz" ), gesetz.name() );
  }

  public FLIESSGESETZ getFliessgesetz( )
  {
    switch( getCalcMode() )
    {
      case REIB_KONST:
        return FLIESSGESETZ.DARCY_WEISBACH_OHNE_FORMEINFLUSS;

      default:
        final String property = (String) m_calcFeature.getProperty( new QName( NS_WSPM_TUHH, "fliessgesetz" ) );
        return FLIESSGESETZ.valueOf( property );
    }

  }

  public void setSubReachDef( final double startStation, final double endStation )
  {
    final QName qname = new QName( NS_WSPM_TUHH, "subReachDefinitionMember" );
    final Feature subReachFeature = FeatureHelper.getSubFeature( m_calcFeature, qname );

    final BigDecimal bigStart = WspmProfile.stationToBigDecimal( startStation );
    final BigDecimal bigEnd = WspmProfile.stationToBigDecimal( endStation );
    subReachFeature.setProperty( new QName( NS_WSPM_TUHH, "startStation" ), bigStart );
    subReachFeature.setProperty( new QName( NS_WSPM_TUHH, "endStation" ), bigEnd );
  }

  public BigDecimal getStartStation( )
  {
    final QName qname = new QName( NS_WSPM_TUHH, "subReachDefinitionMember" );
    final Feature subReachFeature = FeatureHelper.getSubFeature( m_calcFeature, qname );

    return (BigDecimal) subReachFeature.getProperty( new QName( NS_WSPM_TUHH, "startStation" ) );
  }

  public BigDecimal getEndStation( )
  {
    final QName qname = new QName( NS_WSPM_TUHH, "subReachDefinitionMember" );
    final Feature subReachFeature = FeatureHelper.getSubFeature( m_calcFeature, qname );

    return (BigDecimal) subReachFeature.getProperty( new QName( NS_WSPM_TUHH, "endStation" ) );
  }

  public void setStartCondition( final START_KONDITION_KIND type, final double startWsp, final double startSlope )
  {
    final QName qname = new QName( NS_WSPM_TUHH, "startConditionMember" );
    final Feature conditionFeature = FeatureHelper.getSubFeature( m_calcFeature, qname );

    conditionFeature.setProperty( new QName( NS_WSPM_TUHH, "kind" ), type.name() );
    conditionFeature.setProperty( new QName( NS_WSPM_TUHH, "waterlevel" ), new Double( startWsp ) );
    conditionFeature.setProperty( new QName( NS_WSPM_TUHH, "bottomSlope" ), new Double( startSlope ) );
  }

  public START_KONDITION_KIND getStartKind( )
  {
    switch( getCalcMode() )
    {
      case REIB_KONST:
        return START_KONDITION_KIND.UNIFORM_BOTTOM_SLOPE;

      default:
        final QName qname = new QName( NS_WSPM_TUHH, "startConditionMember" );
        final Feature conditionFeature = FeatureHelper.getSubFeature( m_calcFeature, qname );

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
        final Feature conditionFeature = FeatureHelper.getSubFeature( m_calcFeature, qname );

        return (Double) conditionFeature.getProperty( new QName( NS_WSPM_TUHH, "waterlevel" ) );
    }
  }

  public Double getStartSlope( )
  {
    switch( getCalcMode() )
    {
      case REIB_KONST:
        return (Double) getFeature().getProperty( new QName( NS_WSPM_TUHH, "bottomSlope" ) );

      default:
        final QName qname = new QName( NS_WSPM_TUHH, "startConditionMember" );
        final Feature conditionFeature = FeatureHelper.getSubFeature( m_calcFeature, qname );

        return (Double) conditionFeature.getProperty( new QName( NS_WSPM_TUHH, "bottomSlope" ) );
    }
  }

  public void setWaterlevelParameters( final WSP_ITERATION_TYPE iterationType, final VERZOEGERUNSVERLUST_TYPE verzType, final REIBUNGSVERLUST_TYPE reibType, final boolean doCalcBridges, final boolean doCalcBarrages )
  {
    final QName qname = new QName( NS_WSPM_TUHH, "waterlevelParameterMember" );
    final Feature parameterFeature = FeatureHelper.getSubFeature( m_calcFeature, qname );

    parameterFeature.setProperty( new QName( NS_WSPM_TUHH, "wspIteration" ), iterationType.name() );
    parameterFeature.setProperty( new QName( NS_WSPM_TUHH, "verzoegerungsverlust" ), verzType.name() );
    parameterFeature.setProperty( new QName( NS_WSPM_TUHH, "reibungsverlust" ), reibType.name() );

    final QName specialQname = new QName( NS_WSPM_TUHH, "specialOptionsMember" );
    final Feature specialFeature = FeatureHelper.getSubFeature( parameterFeature, specialQname );
    specialFeature.setProperty( new QName( NS_WSPM_TUHH, "doCalcBridges" ), new Boolean( doCalcBridges ) );
    specialFeature.setProperty( new QName( NS_WSPM_TUHH, "doCalcBarrages" ), new Boolean( doCalcBarrages ) );
  }

  public WSP_ITERATION_TYPE getIterationType( )
  {
    final QName qname = new QName( NS_WSPM_TUHH, "waterlevelParameterMember" );
    final Feature parameterFeature = FeatureHelper.getSubFeature( m_calcFeature, qname );

    return WSP_ITERATION_TYPE.valueOf( (String) parameterFeature.getProperty( new QName( NS_WSPM_TUHH, "wspIteration" ) ) );
  }

  public VERZOEGERUNSVERLUST_TYPE getVerzoegerungsverlust( )
  {
    switch( getCalcMode() )
    {
      case REIB_KONST:
        return VERZOEGERUNSVERLUST_TYPE.NON;

      default:
        final QName qname = new QName( NS_WSPM_TUHH, "waterlevelParameterMember" );
        final Feature parameterFeature = FeatureHelper.getSubFeature( m_calcFeature, qname );

        return VERZOEGERUNSVERLUST_TYPE.valueOf( (String) parameterFeature.getProperty( new QName( NS_WSPM_TUHH, "verzoegerungsverlust" ) ) );
    }
  }

  public REIBUNGSVERLUST_TYPE getReibungsverlust( )
  {
    final QName qname = new QName( NS_WSPM_TUHH, "waterlevelParameterMember" );
    final Feature parameterFeature = FeatureHelper.getSubFeature( m_calcFeature, qname );

    return REIBUNGSVERLUST_TYPE.valueOf( (String) parameterFeature.getProperty( new QName( NS_WSPM_TUHH, "reibungsverlust" ) ) );
  }

  public Boolean isCalcBridges( )
  {
    final QName qname = new QName( NS_WSPM_TUHH, "waterlevelParameterMember" );
    final Feature parameterFeature = FeatureHelper.getSubFeature( m_calcFeature, qname );

    final QName specialQname = new QName( NS_WSPM_TUHH, "specialOptionsMember" );
    final Feature specialFeature = FeatureHelper.getSubFeature( parameterFeature, specialQname );
    return (Boolean) specialFeature.getProperty( new QName( NS_WSPM_TUHH, "doCalcBridges" ) );
  }

  public Boolean isCalcBarrages( )
  {
    final QName qname = new QName( NS_WSPM_TUHH, "waterlevelParameterMember" );
    final Feature parameterFeature = FeatureHelper.getSubFeature( m_calcFeature, qname );

    final QName specialQname = new QName( NS_WSPM_TUHH, "specialOptionsMember" );
    final Feature specialFeature = FeatureHelper.getSubFeature( parameterFeature, specialQname );
    return (Boolean) specialFeature.getProperty( new QName( NS_WSPM_TUHH, "doCalcBarrages" ) );
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
    final Feature reachFeature = FeatureHelper.resolveLink( m_calcFeature, new QName( NS_WSPM_TUHH, "reachWspmTuhhSteadyStateMember" ) );
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
}
