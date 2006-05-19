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
package org.kalypso.ui.model.wspm.abstraction;

import java.math.BigDecimal;
import java.math.MathContext;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;

import javax.xml.namespace.QName;

import org.kalypso.gmlschema.IGMLSchema;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.ui.model.wspm.IWspmConstants;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;

import com.sun.org.apache.xerces.internal.jaxp.datatype.XMLGregorianCalendarImpl;

/**
 * @author Belger
 */
public class TuhhCalculation implements IWspmConstants
{
  public static enum MODE
  {
    WSP,
    BF_UNIFORM,
    BF_NON_UNIFORM;
  }

  public static enum FLIESSGESETZ
  {
    DARCY_WEISSBACH,
    DARCY_WEISSBACH_MIT_FORMEINFLUSS,
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
    DFG;
  }

  public static enum REIBUNGSVERLUST_TYPE
  {
    TRAPEZ_FORMULA,
    GEOMETRIC_FORMULA
  }

  private final Feature m_calcFeature;

  // TODO: get number of fractions digits from gml-schema
  private static final MathContext STATION_MATH_CONTEXT = new MathContext( 4 );

  public TuhhCalculation( final Feature calcFeature )
  {
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

    final IPropertyType createProp = m_calcFeature.getFeatureType().getProperty( qname );

    Feature calcCreationFeature = (Feature) m_calcFeature.getProperty( createProp );

    if( calcCreationFeature == null )
    {
      // neues machen
      final GMLWorkspace workspace = m_calcFeature.getWorkspace();
      final IGMLSchema schema = workspace.getGMLSchema();
      final IFeatureType featureType = schema.getFeatureType( new QName( NS_WSPM, "CalcCreation" ) );
      calcCreationFeature = workspace.createFeature( m_calcFeature, featureType );
      m_calcFeature.setProperty( createProp, calcCreationFeature );
    }

    final IFeatureType featureType = calcCreationFeature.getFeatureType();
    final IPropertyType userProp = featureType.getProperty( new QName( NS_WSPM, "user" ) );
    calcCreationFeature.setProperty( userProp, user );

    final IPropertyType dateProp = featureType.getProperty( new QName( NS_WSPM, "date" ) );
    final Calendar calendar = Calendar.getInstance();
    calendar.setTime( date );

    calcCreationFeature.setProperty( dateProp, new XMLGregorianCalendarImpl( (GregorianCalendar) calendar ) );
  }

  public void setReachRef( final TuhhReach reach )
  {
    final IPropertyType reachProp = m_calcFeature.getFeatureType().getProperty( new QName( NS_WSPM_TUHH, "reachWspmTuhhSteadyStateMember" ) );
    m_calcFeature.setProperty( reachProp, reach.getFeature().getId() );
  }

  public void setFliessgesetz( final FLIESSGESETZ gesetz )
  {
    final IPropertyType fliessProp = m_calcFeature.getFeatureType().getProperty( new QName( NS_WSPM_TUHH, "fliessgesetz" ) );

    switch( gesetz )
    {
      case MANNING_STRICKLER:
        m_calcFeature.setProperty( fliessProp, "manningStrickler" );
        break;
      case DARCY_WEISSBACH:
        m_calcFeature.setProperty( fliessProp, "darcyRohr" );
        break;
      case DARCY_WEISSBACH_MIT_FORMEINFLUSS:
        m_calcFeature.setProperty( fliessProp, "darcyOffeneGerinne" );
        break;
    }
  }

  public void setSubReachDef( final double startStation, final double endStation )
  {
    final QName qname = new QName( NS_WSPM_TUHH, "subReachDefinitionMember" );

    final IPropertyType subProp = m_calcFeature.getFeatureType().getProperty( qname );

    Feature subReachFeature = (Feature) m_calcFeature.getProperty( subProp );

    if( subReachFeature == null )
    {
      // neues machen
      final GMLWorkspace workspace = m_calcFeature.getWorkspace();
      final IGMLSchema schema = workspace.getGMLSchema();
      final IFeatureType featureType = schema.getFeatureType( new QName( NS_WSPM_TUHH, "SubReachDefinition" ) );
      subReachFeature = workspace.createFeature( m_calcFeature, featureType );
      m_calcFeature.setProperty( subProp, subReachFeature );
    }

    final IFeatureType featureType = subReachFeature.getFeatureType();

    final IPropertyType startProp = featureType.getProperty( new QName( NS_WSPM_TUHH, "startStation" ) );
    subReachFeature.setProperty( startProp, new BigDecimal( startStation, STATION_MATH_CONTEXT ) );

    final IPropertyType endProp = featureType.getProperty( new QName( NS_WSPM_TUHH, "endStation" ) );
    subReachFeature.setProperty( endProp, new BigDecimal( endStation, STATION_MATH_CONTEXT ) );
  }

  public void setStartCondition( final START_KONDITION_KIND type, final double startWsp, final double startSlope )
  {
    final QName qname = new QName( NS_WSPM_TUHH, "startConditionMember" );

    final IPropertyType conditionProp = m_calcFeature.getFeatureType().getProperty( qname );

    Feature conditionFeature = (Feature) m_calcFeature.getProperty( conditionProp );

    if( conditionFeature == null )
    {
      // neues machen
      final GMLWorkspace workspace = m_calcFeature.getWorkspace();
      final IGMLSchema schema = workspace.getGMLSchema();
      final IFeatureType featureType = schema.getFeatureType( new QName( NS_WSPM_TUHH, "StartCondition" ) );
      conditionFeature = workspace.createFeature( m_calcFeature, featureType );
      m_calcFeature.setProperty( conditionProp, conditionFeature );
    }

    final IFeatureType featureType = conditionFeature.getFeatureType();

    final String kind;
    switch( type )
    {
      default:
      case CRITICAL_WATER_DEPTH:
        kind = "criticalWaterDepth";
        break;

      case UNIFORM_BOTTOM_SLOPE:
        kind = "uniformBottomSlope";
        break;

      case WATERLEVEL:
        kind = "startWaterlevel";
        break;
    }

    final IPropertyType kindProp = featureType.getProperty( new QName( NS_WSPM_TUHH, "kind" ) );
    conditionFeature.setProperty( kindProp, kind );

    final IPropertyType waterlevelProp = featureType.getProperty( new QName( NS_WSPM_TUHH, "waterlevel" ) );
    conditionFeature.setProperty( waterlevelProp, new Double( startWsp ) );

    final IPropertyType slopeProp = featureType.getProperty( new QName( NS_WSPM_TUHH, "bottomSlope" ) );
    conditionFeature.setProperty( slopeProp, new Double( startSlope ) );
  }

  public void setWaterlevelParameters( final WSP_ITERATION_TYPE iterationType, final VERZOEGERUNSVERLUST_TYPE verzType, final REIBUNGSVERLUST_TYPE reibType, final boolean doCalcBridges, boolean doCalcBarrages )
  {
    final QName qname = new QName( NS_WSPM_TUHH, "waterlevelParameterMember" );

    final IPropertyType parameterProp = m_calcFeature.getFeatureType().getProperty( qname );

    Feature parameterFeature = (Feature) m_calcFeature.getProperty( parameterProp );

    if( parameterFeature == null )
    {
      // neues machen
      final GMLWorkspace workspace = m_calcFeature.getWorkspace();
      final IGMLSchema schema = workspace.getGMLSchema();
      final IFeatureType featureType = schema.getFeatureType( new QName( NS_WSPM_TUHH, "WaterlevelParameter" ) );
      parameterFeature = workspace.createFeature( m_calcFeature, featureType );
      m_calcFeature.setProperty( parameterProp, parameterFeature );
    }

    final IFeatureType featureType = parameterFeature.getFeatureType();

    final IPropertyType iterationProp = featureType.getProperty( new QName( NS_WSPM_TUHH, "wspIteration" ) );
    switch( iterationType )
    {
      default:
      case SIMPLE:
        parameterFeature.setProperty( iterationProp, "simpleIteration" );
        break;

      case EXACT:
        parameterFeature.setProperty( iterationProp, "exactIteration" );
        break;
    }

    final IPropertyType verzProp = featureType.getProperty( new QName( NS_WSPM_TUHH, "verzoegerungsverlust" ) );
    switch( verzType )
    {
      default:
      case BJOERNSEN:
        parameterFeature.setProperty( verzProp, "bjoernsen" );
        break;

      case DVWK:
        parameterFeature.setProperty( verzProp, "dvwk" );
        break;

      case DFG:
        parameterFeature.setProperty( verzProp, "dfg" );
        break;
    }

    final IPropertyType reibProp = featureType.getProperty( new QName( NS_WSPM_TUHH, "reibungsverlust" ) );
    switch( reibType )
    {
      default:
      case TRAPEZ_FORMULA:
        parameterFeature.setProperty( reibProp, "trapezFormula" );
        break;

      case GEOMETRIC_FORMULA:
        parameterFeature.setProperty( reibProp, "geometricFormula" );
        break;
    }

    final QName specialQname = new QName( NS_WSPM_TUHH, "specialOptionsMember" );

    final IPropertyType specialProp = parameterFeature.getFeatureType().getProperty( specialQname );

    Feature specialFeature = (Feature) parameterFeature.getProperty( specialProp );

    if( specialFeature == null )
    {
      // neues machen
      final GMLWorkspace workspace = parameterFeature.getWorkspace();
      final IGMLSchema schema = workspace.getGMLSchema();
      final IFeatureType specialType = schema.getFeatureType( new QName( NS_WSPM_TUHH, "SpecialOptions" ) );
      specialFeature = workspace.createFeature( parameterFeature, specialType );
      parameterFeature.setProperty( specialProp, specialFeature );
    }

    final IFeatureType specialType = specialFeature.getFeatureType();

    final IPropertyType bridgesProp = specialType.getProperty( new QName( NS_WSPM_TUHH, "doCalcBridges" ) );
    specialFeature.setProperty( bridgesProp, new Boolean( doCalcBridges ) );

    final IPropertyType barragesProp = specialType.getProperty( new QName( NS_WSPM_TUHH, "doCalcBarrages" ) );
    specialFeature.setProperty( barragesProp, new Boolean( doCalcBarrages ) );

  }

  public void setCalcMode( final MODE mode )
  {
    final Feature feature = getFeature();
    final IFeatureType featureType = feature.getFeatureType();

    final IPropertyType modeProp = featureType.getProperty( new QName( NS_WSPM_TUHH, "mode" ) );

    switch( mode )
    {
      case WSP:
        feature.setProperty( modeProp, "waterlevel" );
        break;
      case BF_UNIFORM:
        feature.setProperty( modeProp, "bankfullUniform" );
        break;

      case BF_NON_UNIFORM:
        feature.setProperty( modeProp, "bankfullNonUniform" );
        break;

      default:
        break;
    }
  }

  public void setQRange( final double minQ, final double maxQ, final double Qstep )
  {
    final Feature feature = getFeature();
    final IFeatureType featureType = feature.getFeatureType();

    final IPropertyType minProp = featureType.getProperty( new QName( NS_WSPM_TUHH, "minimalRunOff" ) );
    feature.setProperty( minProp, new Double( minQ ) );

    final IPropertyType maxProp = featureType.getProperty( new QName( NS_WSPM_TUHH, "maximalRunOff" ) );
    feature.setProperty( maxProp, new Double( maxQ ) );

    final IPropertyType stepProp = featureType.getProperty( new QName( NS_WSPM_TUHH, "runOffStep" ) );
    feature.setProperty( stepProp, new Double( Qstep ) );
  }

  public void setRunOffRef( final String runOffRef )
  {
    final Feature feature = getFeature();
    final IFeatureType featureType = feature.getFeatureType();

    final IPropertyType prop = featureType.getProperty( new QName( NS_WSPM_TUHH, "runOffEventMember" ) );
    feature.setProperty( prop, runOffRef );
  }

}
