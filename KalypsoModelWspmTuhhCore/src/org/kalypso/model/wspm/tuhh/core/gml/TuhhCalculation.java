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
import java.util.Date;

import javax.xml.namespace.QName;

import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.kalypso.commons.java.lang.Objects;
import org.kalypso.contribs.java.util.DateUtilities;
import org.kalypso.gmlschema.GMLSchemaUtilities;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.wspm.core.gml.IRunOffEvent;
import org.kalypso.model.wspm.core.profil.util.ProfileUtil;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
import org.kalypsodeegree_impl.model.feature.Feature_Impl;

/**
 * Binding class for CalculationReibConstWspmTuhhSteadyState AND CalculationWspmTuhhSteadyState
 * 
 * @author Gernot Belger
 */
public abstract class TuhhCalculation extends Feature_Impl implements ITuhhCalculation
{
  public TuhhCalculation( final Object parent, final IRelationType parentRelation, final IFeatureType ft, final String id, final Object[] propValues )
  {
    super( parent, parentRelation, ft, id, propValues );
  }

  public void setCalcCreation( final String user, final Date date )
  {
    final QName qname = new QName( NS_WSPM, "calcCreationMember" ); //$NON-NLS-1$

    Feature calcCreationFeature = getProperty( qname, Feature.class );

    if( calcCreationFeature == null )
    {
      // neues machen
      final GMLWorkspace workspace = getWorkspace();
      final IFeatureType featureType = GMLSchemaUtilities.getFeatureTypeQuiet( new QName( NS_WSPM, "CalcCreation" ) ); //$NON-NLS-1$
      final IRelationType parentRelation = (IRelationType)getFeatureType().getProperty( qname );
      calcCreationFeature = workspace.createFeature( this, parentRelation, featureType );
      setProperty( qname, calcCreationFeature );
    }

    calcCreationFeature.setProperty( new QName( NS_WSPM, "user" ), user ); //$NON-NLS-1$
    calcCreationFeature.setProperty( new QName( NS_WSPM, "date" ), DateUtilities.toXMLGregorianCalendar( date ) ); //$NON-NLS-1$
  }

  public void setReachRef( final TuhhReach reach )
  {
    setProperty( QN_PROPERTY_STEADY_STATE_MEMBER, reach.getId() ); //$NON-NLS-1$
  }

  public void setFliessgesetz( final FLIESSGESETZ gesetz )
  {
    setProperty( QN_PROPERTY_FLIESSGESETZ, gesetz.name() ); //$NON-NLS-1$
  }

  public FLIESSGESETZ getFliessgesetz( )
  {
    switch( getCalcMode() )
    {
      case REIB_KONST:
        return FLIESSGESETZ.DARCY_WEISBACH_OHNE_FORMEINFLUSS;

      default:
        final String property = getProperty( QN_PROPERTY_FLIESSGESETZ, String.class ); //$NON-NLS-1$
        return FLIESSGESETZ.valueOf( property );
    }
  }

  @Override
  public boolean isPreferingRoughnessClasses( )
  {
    final Object property = getProperty( QN_PROPERTY_PREFERE_ROUGHNESS_CLASSES );
    if( Objects.isNull( property ) )
      return false;

    return Boolean.valueOf( property.toString() );
  }

  @Override
  public boolean isPreferingVegetationClasses( )
  {
    final Object property = getProperty( QN_PROPERTY_PREFERE_VEGETATION_CLASSES );
    if( Objects.isNull( property ) )
      return false;

    return Boolean.valueOf( property.toString() );
  }

  public void setSubReachDef( final double startStation, final double endStation )
  {
    final Feature subReachFeature = FeatureHelper.getSubFeature( this, QN_PROPERTY_SUB_REACH_DEFINITION_MEMBER );

    final BigDecimal bigStart = ProfileUtil.stationToBigDecimal( startStation );
    final BigDecimal bigEnd = ProfileUtil.stationToBigDecimal( endStation );
    subReachFeature.setProperty( new QName( NS_WSPM_TUHH, "startStation" ), bigStart ); //$NON-NLS-1$
    subReachFeature.setProperty( new QName( NS_WSPM_TUHH, "endStation" ), bigEnd ); //$NON-NLS-1$
  }

  public BigDecimal getStartStation( )
  {
    final Feature subReachFeature = FeatureHelper.getSubFeature( this, QN_PROPERTY_SUB_REACH_DEFINITION_MEMBER );

    return (BigDecimal)subReachFeature.getProperty( new QName( NS_WSPM_TUHH, "startStation" ) ); //$NON-NLS-1$
  }

  public BigDecimal getEndStation( )
  {
    final Feature subReachFeature = FeatureHelper.getSubFeature( this, QN_PROPERTY_SUB_REACH_DEFINITION_MEMBER );

    return (BigDecimal)subReachFeature.getProperty( new QName( NS_WSPM_TUHH, "endStation" ) ); //$NON-NLS-1$
  }

  public void setStartCondition( final START_KONDITION_KIND type, final Double startWsp, final BigDecimal startSlope )
  {
    final QName qname = new QName( NS_WSPM_TUHH, "startConditionMember" ); //$NON-NLS-1$
    final Feature conditionFeature = FeatureHelper.getSubFeature( this, qname );

    conditionFeature.setProperty( new QName( NS_WSPM_TUHH, "kind" ), type.name() ); //$NON-NLS-1$
    conditionFeature.setProperty( new QName( NS_WSPM_TUHH, "waterlevel" ), startWsp ); //$NON-NLS-1$
    conditionFeature.setProperty( new QName( NS_WSPM_TUHH, "bottomSlope" ), startSlope ); //$NON-NLS-1$
  }

  public START_KONDITION_KIND getStartKind( )
  {
    switch( getCalcMode() )
    {
      case REIB_KONST:
        return START_KONDITION_KIND.UNIFORM_BOTTOM_SLOPE;

      default:
        final QName qname = new QName( NS_WSPM_TUHH, "startConditionMember" ); //$NON-NLS-1$
        final Feature conditionFeature = FeatureHelper.getSubFeature( this, qname );

        return START_KONDITION_KIND.valueOf( (String)conditionFeature.getProperty( new QName( NS_WSPM_TUHH, "kind" ) ) ); //$NON-NLS-1$
    }
  }

  public Double getStartWaterlevel( )
  {
    switch( getCalcMode() )
    {
      case REIB_KONST:
        return null;

      default:
        final QName qname = new QName( NS_WSPM_TUHH, "startConditionMember" ); //$NON-NLS-1$
        final Feature conditionFeature = FeatureHelper.getSubFeature( this, qname );

        return (Double)conditionFeature.getProperty( new QName( NS_WSPM_TUHH, "waterlevel" ) ); //$NON-NLS-1$
    }
  }

  public void setStartSlope( final BigDecimal slope )
  {
    switch( getCalcMode() )
    {
      case REIB_KONST:
        setProperty( new QName( NS_WSPM_TUHH, "bottomSlope" ), slope ); //$NON-NLS-1$
        return;

      default:
        final QName qname = new QName( NS_WSPM_TUHH, "startConditionMember" ); //$NON-NLS-1$
        final Feature conditionFeature = FeatureHelper.getSubFeature( this, qname );
        conditionFeature.setProperty( new QName( NS_WSPM_TUHH, "bottomSlope" ), slope ); //$NON-NLS-1$
        return;
    }
  }

  public BigDecimal getStartSlope( )
  {
    switch( getCalcMode() )
    {
      case REIB_KONST:
        return getProperty( new QName( NS_WSPM_TUHH, "bottomSlope" ), BigDecimal.class ); //$NON-NLS-1$

      default:
        final QName qname = new QName( NS_WSPM_TUHH, "startConditionMember" ); //$NON-NLS-1$
        final Feature conditionFeature = FeatureHelper.getSubFeature( this, qname );
        return (BigDecimal)conditionFeature.getProperty( new QName( NS_WSPM_TUHH, "bottomSlope" ) ); //$NON-NLS-1$
    }
  }

  public void setWaterlevelParameters( final WSP_ITERATION_TYPE iterationType, final VERZOEGERUNSVERLUST_TYPE verzType, final REIBUNGSVERLUST_TYPE reibType, final boolean doCalcBridges, final boolean doCalcBarrages, final boolean useExtremeRoughness )
  {
    final Feature parameterFeature = FeatureHelper.getSubFeature( this, QN_PROP_WATERLEVEL_PARAMS );

    parameterFeature.setProperty( new QName( NS_WSPM_TUHH, "wspIteration" ), iterationType.name() ); //$NON-NLS-1$
    parameterFeature.setProperty( new QName( NS_WSPM_TUHH, "verzoegerungsverlust" ), verzType.name() ); //$NON-NLS-1$
    parameterFeature.setProperty( new QName( NS_WSPM_TUHH, "reibungsverlust" ), reibType.name() ); //$NON-NLS-1$

    final Feature specialFeature = FeatureHelper.getSubFeature( parameterFeature, QN_PROP_SPECIAL_OPTIONS_MEMBER );
    specialFeature.setProperty( new QName( NS_WSPM_TUHH, "doCalcBridges" ), Boolean.valueOf( doCalcBridges ) ); //$NON-NLS-1$
    specialFeature.setProperty( new QName( NS_WSPM_TUHH, "doCalcBarrages" ), Boolean.valueOf( doCalcBarrages ) ); //$NON-NLS-1$
    specialFeature.setProperty( QN_PROP_SPECIAL_PROP_USE_EXTREME_ROUGHNESS, Boolean.valueOf( useExtremeRoughness ) );
  }

  public WSP_ITERATION_TYPE getIterationType( )
  {
    final Feature parameterFeature = FeatureHelper.getSubFeature( this, QN_PROP_WATERLEVEL_PARAMS );

    return WSP_ITERATION_TYPE.valueOf( (String)parameterFeature.getProperty( new QName( NS_WSPM_TUHH, "wspIteration" ) ) ); //$NON-NLS-1$
  }

  public VERZOEGERUNSVERLUST_TYPE getVerzoegerungsverlust( )
  {
    switch( getCalcMode() )
    {
      case REIB_KONST:
        return VERZOEGERUNSVERLUST_TYPE.NON;

      default:
        final Feature parameterFeature = FeatureHelper.getSubFeature( this, QN_PROP_WATERLEVEL_PARAMS );

        return VERZOEGERUNSVERLUST_TYPE.valueOf( (String)parameterFeature.getProperty( new QName( NS_WSPM_TUHH, "verzoegerungsverlust" ) ) ); //$NON-NLS-1$
    }
  }

  public REIBUNGSVERLUST_TYPE getReibungsverlust( )
  {
    final Feature parameterFeature = FeatureHelper.getSubFeature( this, QN_PROP_WATERLEVEL_PARAMS );

    return REIBUNGSVERLUST_TYPE.valueOf( (String)parameterFeature.getProperty( new QName( NS_WSPM_TUHH, "reibungsverlust" ) ) ); //$NON-NLS-1$
  }

  public boolean isCalcBridges( )
  {
    final Feature parameterFeature = FeatureHelper.getSubFeature( this, QN_PROP_WATERLEVEL_PARAMS );
    final Feature specialFeature = FeatureHelper.getSubFeature( parameterFeature, QN_PROP_SPECIAL_OPTIONS_MEMBER );
    final Boolean value = (Boolean)specialFeature.getProperty( new QName( NS_WSPM_TUHH, "doCalcBridges" ) ); //$NON-NLS-1$
    if( value == null )
      return false;

    return value.booleanValue();
  }

  public boolean isCalcBarrages( )
  {
    final Feature parameterFeature = FeatureHelper.getSubFeature( this, QN_PROP_WATERLEVEL_PARAMS );
    final Feature specialFeature = FeatureHelper.getSubFeature( parameterFeature, QN_PROP_SPECIAL_OPTIONS_MEMBER );
    final Boolean value = (Boolean)specialFeature.getProperty( new QName( NS_WSPM_TUHH, "doCalcBarrages" ) ); //$NON-NLS-1$
    if( value == null )
      return false;

    return value.booleanValue();
  }

  public boolean isUseExtremeRoughness( )
  {
    final Feature parameterFeature = FeatureHelper.getSubFeature( this, QN_PROP_WATERLEVEL_PARAMS );
    final Feature specialFeature = FeatureHelper.getSubFeature( parameterFeature, QN_PROP_SPECIAL_OPTIONS_MEMBER );
    final Boolean useExtremeRoughness = (Boolean)specialFeature.getProperty( QN_PROP_SPECIAL_PROP_USE_EXTREME_ROUGHNESS );
    if( useExtremeRoughness == null )
      return false;

    return useExtremeRoughness.booleanValue();
  }

  public void setCalcMode( final MODE mode )
  {
    setProperty( new QName( NS_WSPM_TUHH, "mode" ), mode.name() ); //$NON-NLS-1$
  }

  public void setQRange( final Double minQ, final Double maxQ, final Double qStep )
  {
    final Feature feature = FeatureHelper.getSubFeature( this, new QName( NS_WSPM_TUHH, "runOffIntervalMember" ) ); //$NON-NLS-1$

    feature.setProperty( new QName( NS_WSPM_TUHH, "minimalRunOff" ), minQ ); //$NON-NLS-1$
    feature.setProperty( new QName( NS_WSPM_TUHH, "maximalRunOff" ), maxQ ); //$NON-NLS-1$
    feature.setProperty( new QName( NS_WSPM_TUHH, "runOffStep" ), qStep ); //$NON-NLS-1$
  }

  public Double getMinQ( )
  {
    final Feature feature = FeatureHelper.getSubFeature( this, new QName( NS_WSPM_TUHH, "runOffIntervalMember" ) ); //$NON-NLS-1$
    return (Double)feature.getProperty( new QName( NS_WSPM_TUHH, "minimalRunOff" ) ); //$NON-NLS-1$
  }

  public Double getMaxQ( )
  {
    final Feature feature = FeatureHelper.getSubFeature( this, new QName( NS_WSPM_TUHH, "runOffIntervalMember" ) ); //$NON-NLS-1$
    return (Double)feature.getProperty( new QName( NS_WSPM_TUHH, "maximalRunOff" ) ); //$NON-NLS-1$
  }

  public Double getQStep( )
  {
    final Feature feature = FeatureHelper.getSubFeature( this, new QName( NS_WSPM_TUHH, "runOffIntervalMember" ) ); //$NON-NLS-1$
    return (Double)feature.getProperty( new QName( NS_WSPM_TUHH, "runOffStep" ) ); //$NON-NLS-1$
  }

  public void setRunOffRef( final String runOffRef )
  {
    setProperty( QN_PROPERTY_RUN_OFF_EVENT_MEMBER, runOffRef );
  }

  public TuhhReach getReach( )
  {
    final Feature reachFeature = FeatureHelper.resolveLink( this, QN_PROPERTY_STEADY_STATE_MEMBER, true ); //$NON-NLS-1$
    return (TuhhReach)reachFeature;
  }

  public IRunOffEvent getRunOffEvent( )
  {
    return (IRunOffEvent)FeatureHelper.resolveLink( this, QN_PROPERTY_RUN_OFF_EVENT_MEMBER, true );
  }

  public MODE getCalcMode( )
  {
    if( QN_TUHH_CALC_REIB_CONST.equals( getQName() ) )
      return MODE.REIB_KONST;

    final String value = (String)getProperty( PROPERTY_MODE ); //$NON-NLS-1$
    return MODE.valueOf( value );
  }

  private Object getQName( )
  {
    return getFeatureType().getQName();
  }

  /** Only valid for REIB_KONST mode. */
  public PolynomeProperties getPolynomeProperties( )
  {
    final Feature polyFeature = (Feature)getProperty( QN_PROP_POLYNOME_MEMBER );
    if( polyFeature == null )
      return null;

    return (PolynomeProperties)polyFeature;
  }

  public String getVersion( )
  {
    final Feature parameterFeature = FeatureHelper.getSubFeature( this, QN_PROP_WATERLEVEL_PARAMS );
    return (String)parameterFeature.getProperty( QN_PROP_EXE_VERSION );
  }

  public void setVersion( final String version )
  {
    final Feature parameterFeature = FeatureHelper.getSubFeature( this, QN_PROP_WATERLEVEL_PARAMS );
    parameterFeature.setProperty( QN_PROP_EXE_VERSION, version == null ? null : version );
  }

  public IPath getResultFolder( )
  {
    final String calcCaseName = getName();
    return new Path( IWspmTuhhConstants.FOLDER_RESULTS ).append( calcCaseName );
  }
}
