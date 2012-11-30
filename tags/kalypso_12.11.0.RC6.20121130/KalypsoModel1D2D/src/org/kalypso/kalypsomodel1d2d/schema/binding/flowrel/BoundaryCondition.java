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
package org.kalypso.kalypsomodel1d2d.schema.binding.flowrel;

import java.math.BigInteger;
import java.util.List;

import javax.xml.namespace.QName;

import org.kalypso.contribs.java.lang.NumberUtils;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.kalypsomodel1d2d.schema.UrlCatalog1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFELine;
import org.kalypso.kalypsomodel1d2d.schema.dict.Kalypso1D2DDictConstants;
import org.kalypso.kalypsosimulationmodel.core.flowrel.FlowRelationship;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.observation.result.TupleResultUtilities;
import org.kalypso.ogc.gml.om.ObservationFeatureFactory;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author Gernot Belger
 */
public class BoundaryCondition extends FlowRelationship implements IBoundaryCondition
{
  
  private IObservation<TupleResult> m_observation;

  private BOUNDARY_TYPE m_boundType = null;

  public static final QName OP1D2D_PROP_STATIONARY_COND = new QName( UrlCatalog1D2D.MODEL_1D2DOperational_NS, "stationaryCondition" ); //$NON-NLS-1$

  public BoundaryCondition( final Object parent, final IRelationType parentRelation, final IFeatureType ft, final String id, final Object[] propValues )
  {
    super( parent, parentRelation, ft, id, propValues );
  }
  
  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IBoundaryCondition#getTimeserie1DFeature()
   */
  private Feature getTimeserieFeature( )
  {
    return (Feature) getProperty( QNAME_P_OBSERVATION );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IBoundaryCondition#initializeObservation(java.lang.String,
   *      java.lang.String)
   */
  @Override
  public IObservation<TupleResult> initializeObservation( final String domainComponentUrn, final String valueComponentUrn )
  {
    final Feature obsFeature = getTimeserieFeature();

    if( (domainComponentUrn.equals( Kalypso1D2DDictConstants.DICT_COMPONENT_TIME ) && valueComponentUrn.equals( Kalypso1D2DDictConstants.DICT_COMPONENT_DISCHARGE ))
        || (domainComponentUrn.equals( Kalypso1D2DDictConstants.DICT_COMPONENT_TIME ) && valueComponentUrn.equals( Kalypso1D2DDictConstants.DICT_COMPONENT_SPECIFIC_DISCHARGE_1D ))
        || (domainComponentUrn.equals( Kalypso1D2DDictConstants.DICT_COMPONENT_TIME ) && valueComponentUrn.equals( Kalypso1D2DDictConstants.DICT_COMPONENT_SPECIFIC_DISCHARGE_2D ))
        || (domainComponentUrn.equals( Kalypso1D2DDictConstants.DICT_COMPONENT_WATERLEVEL ) && valueComponentUrn.equals( Kalypso1D2DDictConstants.DICT_COMPONENT_DISCHARGE )) )
    {
      setProperty( QNAME_P_DIRECTION, new BigInteger( "0" ) ); //$NON-NLS-1$
      setHasDirection( true );
    }
    else
    {
      setProperty( QNAME_P_DIRECTION, null );
      setProperty( QNAME_P_HASDIRECTION, null );
    }

    if( (domainComponentUrn.equals( Kalypso1D2DDictConstants.DICT_COMPONENT_TIME ) && valueComponentUrn.equals( Kalypso1D2DDictConstants.DICT_COMPONENT_SPECIFIC_DISCHARGE_1D ))
        || (domainComponentUrn.equals( Kalypso1D2DDictConstants.DICT_COMPONENT_TIME ) && valueComponentUrn.equals( Kalypso1D2DDictConstants.DICT_COMPONENT_SPECIFIC_DISCHARGE_2D ))
        || (domainComponentUrn.equals( Kalypso1D2DDictConstants.DICT_COMPONENT_TIME ) && valueComponentUrn.equals( Kalypso1D2DDictConstants.DICT_COMPONENT_DISCHARGE )) )
      setIsAbsolute( true );
    else
      setIsAbsolute( null );

    final String[] componentUrns;
    if( valueComponentUrn.equals( Kalypso1D2DDictConstants.DICT_COMPONENT_WAVE_HSIG ) )
    {
      m_boundType = BOUNDARY_TYPE.WavesBoundary;
      componentUrns = new String[] { domainComponentUrn, valueComponentUrn, Kalypso1D2DDictConstants.DICT_COMPONENT_WAVE_PER, Kalypso1D2DDictConstants.DICT_COMPONENT_WAVE_DIR,
          Kalypso1D2DDictConstants.DICT_COMPONENT_WAVE_DD };
      setProperty( QNAME_P_DIRECTION, null );
      setProperty( QNAME_P_HASDIRECTION, null );
    }
    else
    {
      m_boundType = BOUNDARY_TYPE.HydroBoundary;
      componentUrns = new String[] { domainComponentUrn, valueComponentUrn };
    }

    final IComponent[] components = new IComponent[componentUrns.length];

    for( int i = 0; i < components.length; i++ )
      components[i] = ObservationFeatureFactory.createDictionaryComponent( obsFeature, componentUrns[i] );

    final IObservation<TupleResult> obs = ObservationFeatureFactory.toObservation( obsFeature );

    // removed because of double declaration of components in all boundary conditions.
    // final TupleResult result = obs.getResult();
    // for( final IComponent component : components )
    // result.addComponent( component );

    return obs;
  }

  @Override
  public void setObservation( final IObservation<TupleResult> obs )
  {
    ObservationFeatureFactory.toFeature( obs, getTimeserieFeature() );
    m_observation = null;
  }

  @Override
  public IObservation<TupleResult> getObservation( )
  {
    // for big timeseries this operation is verrrry slow, and function is called several times, so a bit of cache-ing..
    if( m_observation == null )
      m_observation = ObservationFeatureFactory.toObservation( getTimeserieFeature() );
    return m_observation;
  }

  @Override
  public String getStationaryCondition( )
  {
    final Feature feature = this;
    final Object property = feature.getProperty( BoundaryCondition.OP1D2D_PROP_STATIONARY_COND );
    if( property instanceof Double )
    {
      // return ((Double) property).doubleValue();
      return "" + ((Double) property).doubleValue(); //$NON-NLS-1$
    }
    else if( property instanceof String )
    {
      final double parseQuietDouble = NumberUtils.parseQuietDouble( (String) property );
      if( !Double.isNaN( parseQuietDouble ) )
      {
        return "" + parseQuietDouble; //$NON-NLS-1$
      } //$NON-NLS-1$
      else{
        return (String) property;
      }
    }
    else
    {
      return "" + Double.NaN; //$NON-NLS-1$
    }
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IBoundaryCondition#setStationaryCondition(java.lang.String)
   */
  @Override
  public void setStationaryCondition( final String statCond )
  {
    // Double dValue;
    // if( statCond == null || "".equals( statCond ) )
    // {
    // dValue = null;
    // }
    // else
    // {
    // dValue = Double.valueOf( statCond );
    // }
    final Feature feature = this;
    feature.setProperty( BoundaryCondition.OP1D2D_PROP_STATIONARY_COND, statCond.trim() );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IBoundaryCondition#getTheta()
   */
  @Override
  public BigInteger getDirection( )
  {
    return (BigInteger) getProperty( QNAME_P_DIRECTION );
  }

  @Override
  public void setParentElement( final Feature parentElement )
  {
    if( parentElement == null )
      return;
    String parentType = null;

    if( parentElement instanceof IFELine )
      parentType = IBoundaryCondition.PARENT_TYPE_LINE1D2D;
    else if( parentElement instanceof IFE1D2DNode )
      parentType = IBoundaryCondition.PARENT_TYPE_NODE1D2D;
    else if( parentElement instanceof IFE1D2DElement )
      parentType = IBoundaryCondition.PARENT_TYPE_ELEMENT1D2D;
    if( parentType != null )
    {
      setProperty( IBoundaryCondition.PROP_PARENT_TYPE, parentType );
      setProperty( IBoundaryCondition.PROP_PARENT_MODEL_ELEMENT, parentElement.getId() );
    }
    // TODO consider what to do if parentElement is something else
  }

  @Override
  public String getParentElementID( )
  {
    return getProperty( IBoundaryCondition.PROP_PARENT_MODEL_ELEMENT ).toString();
  }

  @Override
  public String getTypeByLocation( )
  {
    return getProperty( IBoundaryCondition.PROP_PARENT_TYPE ).toString();
  }

  @Override
  public List<String> getParentCalculationUnitIDs( )
  {
    return (List<String>) getProperty( PROP_PARENT_CALCULATION_UNIT );
  }

  @Override
  public boolean isMemberOf( final String calculationUnitId )
  {
    final List<String> parentCalculationUnits = getParentCalculationUnitIDs();
    for( final String parentCalculationUnit : parentCalculationUnits )
    {
      if( calculationUnitId.equals( parentCalculationUnit ) )
        return true;
    }

    return false;
  }

  @Override
  public Boolean isAbsolute( )
  {
    return (Boolean) getProperty( QNAME_P_ISABSOLUTE );
  }

  @Override
  public void setIsAbsolute( final Boolean value )
  {
    setProperty( QNAME_P_ISABSOLUTE, value );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IBoundaryCondition#hasDirection()
   */
  @Override
  public Boolean hasDirection( )
  {
    // if( getProperty( QNAME_P_HASDIRECTION ) == null )
    // return false;

    return (Boolean) getProperty( QNAME_P_HASDIRECTION );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IBoundaryCondition#setHasDirection(java.lang.Boolean)
   */
  @Override
  public void setHasDirection( final Boolean value )
  {
    setProperty( QNAME_P_HASDIRECTION, value );

  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IBoundaryCondition#getInflowVelocity()
   */
  @Override
  public double getInflowVelocity( )
  {
    final boolean hasDirection = (Boolean) getProperty( QNAME_P_HASDIRECTION );
    if( hasDirection )
      return (Double) getProperty( QNAME_P_INFLOWVELOCITY );
    else
      return 0.0;

  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IBoundaryCondition#setDirection()
   */
  @Override
  public void setDirection( final BigInteger value )
  {
    setProperty( QNAME_P_DIRECTION, value );

  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IBoundaryCondition#setInflowVelocity(double)
   */
  @Override
  public void setInflowVelocity( final double value )
  {
    setProperty( QNAME_P_INFLOWVELOCITY, value );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IBoundaryCondition#getBoundaryType()
   */
  @Override
  public BOUNDARY_TYPE getBoundaryType( )
  {
    if( m_boundType == null )
    {
      final IObservation<TupleResult> obs = getObservation();
      final TupleResult obsResult = obs.getResult();
      final IComponent abscissaComponent = TupleResultUtilities.findComponentById( obsResult, Kalypso1D2DDictConstants.DICT_COMPONENT_TIME );
      final IComponent ordinateComponent = TupleResultUtilities.findComponentById( obsResult, Kalypso1D2DDictConstants.DICT_COMPONENT_WAVE_HSIG );
      if( getTypeByLocation().equals( IBoundaryCondition.PARENT_TYPE_LINE1D2D ) && abscissaComponent != null && ordinateComponent != null )
      {
        m_boundType = BOUNDARY_TYPE.WavesBoundary;
      }
      else
      {
        m_boundType = BOUNDARY_TYPE.HydroBoundary;
      }
    }
    return m_boundType;
  }

}
