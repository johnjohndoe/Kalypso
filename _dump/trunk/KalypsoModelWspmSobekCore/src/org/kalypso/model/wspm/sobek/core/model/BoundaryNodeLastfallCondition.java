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
package org.kalypso.model.wspm.sobek.core.model;

import java.util.GregorianCalendar;

import javax.xml.datatype.XMLGregorianCalendar;

import org.apache.commons.lang.NotImplementedException;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.wspm.sobek.core.interfaces.IBoundaryNode;
import org.kalypso.model.wspm.sobek.core.interfaces.IBoundaryNodeLastfallCondition;
import org.kalypso.model.wspm.sobek.core.interfaces.ILastfall;
import org.kalypso.model.wspm.sobek.core.interfaces.ISobekConstants;
import org.kalypso.model.wspm.sobek.core.interfaces.IBoundaryNode.BOUNDARY_TYPE;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.Observation;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.ogc.gml.FeatureUtils;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.om.ObservationFeatureFactory;
import org.kalypso.zml.obslink.TimeseriesLinkType;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author Dirk Kuch
 */
public class BoundaryNodeLastfallCondition implements IBoundaryNodeLastfallCondition
{

  private final ILastfall m_lastfall;

  private final BoundaryNode m_boundaryNode;

  private final Feature m_feature;

  public BoundaryNodeLastfallCondition( final ILastfall lastfall, final BoundaryNode boundaryNode, final Feature feature )
  {
    m_lastfall = lastfall;
    m_boundaryNode = boundaryNode;
    m_feature = feature;
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.IBoundaryNodeLastfallCondition#getBoundaryNode()
   */
  public IBoundaryNode getBoundaryNode( )
  {
    return m_boundaryNode;
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.IBoundaryNodeLastfallCondition#getConstantValue()
   */
  public Double getConstantValue( )
  {
    return (Double) getFeature().getProperty( ISobekConstants.QN_HYDRAULIC_BOUNDARY_NODE_CONDITION_CONST_VALUE );
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.IBoundaryNodeLastfallCondition#getConstantValueInterveal()
   */
  public Integer getConstantValueInterveal( )
  {
    throw new NotImplementedException();
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.IBoundaryNodeLastfallCondition#getFeature()
   */
  public Feature getFeature( )
  {
    return m_feature;
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.IBoundaryNodeLastfallCondition#getLastfall()
   */
  public ILastfall getLastfall( )
  {
    return m_lastfall;
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.IBoundaryNodeLastfallCondition#getLastUsedType()
   */
  public BOUNDARY_CONDITION_TYPE getLastUsedType( )
  {
    final String name = (String) getFeature().getProperty( ISobekConstants.QN_HYDRAULIC_BOUNDARY_NODE_CONDITION_TYPE );

    return BOUNDARY_CONDITION_TYPE.getType( name );
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.IBoundaryNodeLastfallCondition#getObservationEnd()
   */
  public GregorianCalendar getObservationEnd( )
  {
    final Object property = getFeature().getProperty( ISobekConstants.QN_HYDRAULIC_BOUNDARY_NODE_CONDITION_ENDS );
    if( property instanceof XMLGregorianCalendar )
      return ((XMLGregorianCalendar) property).toGregorianCalendar();

    return null;
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.IBoundaryNodeLastfallCondition#getObservationStart()
   */
  public GregorianCalendar getObservationStart( )
  {
    final Object property = getFeature().getProperty( ISobekConstants.QN_HYDRAULIC_BOUNDARY_NODE_CONDITION_BEGINS );
    if( property instanceof XMLGregorianCalendar )
      return ((XMLGregorianCalendar) property).toGregorianCalendar();

    return null;
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.IBoundaryNodeLastfallCondition#getTimeseriesLink()
   */
  public TimeseriesLinkType getTimeseriesLink( )
  {
    return (TimeseriesLinkType) getFeature().getProperty( ISobekConstants.QN_HYDRAULIC_BOUNDARY_NODE_CONDITION_LNK_TIME_SERIES );
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.IBoundaryNodeLastfallCondition#getTimeSeriesObservation()
   */
  public IObservation<TupleResult> getTimeSeriesObservation( )
  {
    Feature feature = getTimeSeriesObservationFeature();
    if( feature == null )
    {
      feature = generateTimeSeriesObs();
    }

    return ObservationFeatureFactory.toObservation( feature );
  }

  private Feature generateTimeSeriesObs( )
  {
    try
    {
      final Feature feature = getFeature();
      final CommandableWorkspace workspace = m_lastfall.getModelMember().getWorkspace();

      final IPropertyType property = feature.getFeatureType().getProperty( ISobekConstants.QN_HYDRAULIC_BOUNDARY_NODE_CONDITION_OBSERVATION );
      final IRelationType relation = (IRelationType) property;

      final Feature fObs = workspace.createFeature( feature, relation, relation.getTargetFeatureType() );
      workspace.setFeatureAsComposition( feature, relation, fObs, true );

      /* new resultset */
      final TupleResult result = new TupleResult();

      /* add components to resultset */

      final BOUNDARY_TYPE boundaryType = getBoundaryNode().getBoundaryType();
      if( BOUNDARY_TYPE.eW.equals( boundaryType ) )
      {
        result.addComponent( ObservationFeatureFactory.createDictionaryComponent( fObs, OBS_DATE ) );
        result.addComponent( ObservationFeatureFactory.createDictionaryComponent( fObs, OBS_W ) );
      }
      else if( BOUNDARY_TYPE.eQ.equals( boundaryType ) )
      {
        result.addComponent( ObservationFeatureFactory.createDictionaryComponent( fObs, OBS_DATE ) );
        result.addComponent( ObservationFeatureFactory.createDictionaryComponent( fObs, OBS_Q ) );
      }
      else if( BOUNDARY_TYPE.eWQ.equals( boundaryType ) )
      {
        result.addComponent( ObservationFeatureFactory.createDictionaryComponent( fObs, OBS_W ) );
        result.addComponent( ObservationFeatureFactory.createDictionaryComponent( fObs, OBS_Q ) );
      }
      else
        throw new IllegalStateException();

      /* add observation to workspace */
      final IObservation<TupleResult> obs = new Observation<TupleResult>( "name", "description", result ); //$NON-NLS-1$ //$NON-NLS-2$

      // maybe set phenomenon?
      ObservationFeatureFactory.toFeature( obs, fObs );

      return fObs;
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      return null;
    }
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.IBoundaryNodeLastfallCondition#getTimeSeriesObservationFeature()
   */
  public Feature getTimeSeriesObservationFeature( )
  {
    final Object property = m_feature.getProperty( ISobekConstants.QN_HYDRAULIC_BOUNDARY_NODE_CONDITION_OBSERVATION );
    if( !(property instanceof Feature) )
      return null;

    return (Feature) property;
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.IBoundaryNodeLastfallCondition#hasTimeSeriesObservation()
   */
  public Boolean hasTimeSeriesObservation( )
  {
    final Feature obs = getTimeSeriesObservationFeature();
    if( obs == null )
      return false;

    return true;
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.IBoundaryNodeLastfallCondition#isConstantValueNode()
   */
  public boolean isConstantValueNode( )
  {
    final Object objValue = getFeature().getProperty( ISobekConstants.QN_HYDRAULIC_BOUNDARY_NODE_CONDITION_CONST_VALUE );
    if( objValue instanceof Double )
      return true;

    return false;
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.IBoundaryNodeLastfallCondition#isTimeSeriesNode()
   */
  public boolean isTimeSeriesNode( )
  {
    final Object lnkTS = getTimeseriesLink();
    if( lnkTS instanceof TimeseriesLinkType )
      return true;

    return false;
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.IBoundaryNodeLastfallCondition#setTimeSeriesLink(org.kalypso.zml.obslink.TimeseriesLinkType)
   */
  public void setTimeSeriesLink( final TimeseriesLinkType lnk )
  {
    try
    {
      FeatureUtils.updateProperty( getLastfall().getModelMember().getWorkspace(), getFeature(), ISobekConstants.QN_HYDRAULIC_BOUNDARY_NODE_CONDITION_LNK_TIME_SERIES, lnk );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }
  }
}
