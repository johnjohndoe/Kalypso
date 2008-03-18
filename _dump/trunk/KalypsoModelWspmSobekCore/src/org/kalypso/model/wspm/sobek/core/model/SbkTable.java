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
package org.kalypso.model.wspm.sobek.core.model;

import java.math.BigInteger;

import org.kalypso.model.wspm.sobek.core.interfaces.ISbkTable;
import org.kalypso.model.wspm.sobek.core.interfaces.ISobekConstants;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.ogc.gml.om.ObservationFeatureFactory;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author thuel2
 */
public class SbkTable implements ISbkTable
{
  private class Interpolation
  {
    private final String m_period;

    private final String m_type;

    private final BigInteger m_value;

    Interpolation( final Feature table )
    {
      // TODO ggf. noch "Übersetzung" der Strings... von nofdp -> SBK
      m_period = (String) table.getProperty( ISobekConstants.QN_HYDRAULIC_SBK_STRUCTURE_TABLE_INTERPOLATION_PERIOD );
      m_type = (String) table.getProperty( ISobekConstants.QN_HYDRAULIC_SBK_STRUCTURE_TABLE_INTERPOLATION_TYPE );

      final Object property = table.getProperty( ISobekConstants.QN_HYDRAULIC_SBK_STRUCTURE_TABLE_INTERPOLATION_VALUE );
      if( property instanceof Integer )
        m_value = new BigInteger( ((Number) property).toString() );
      else
        m_value = new BigInteger( "0" ); //$NON-NLS-1$
    }

    public String getPeriod( )
    {
      return m_period;
    }

    public String getType( )
    {
      return m_type;
    }

    public BigInteger getValue( )
    {
      return m_value;
    }
  }

  private class TabularData
  {
    private final long m_rowsCount;

    private final TupleResult m_result;

    TabularData( final Feature table )
    {

      final IObservation<TupleResult> tableObs = ObservationFeatureFactory.toObservation( table );
      m_result = tableObs.getResult();
      m_rowsCount = m_result.size();

      for( final IRecord record : m_result )
      {
// ObservationUtilities.

      }

      // TODO
// <catalog:system systemId="urn:ogc:gml:dict:kalypso:nofdp:idss:retardingBasinObservationDefs"
// uri="dict_retarding_basin_observation_defs.gml" />
// WATERLEVEL
// DISCHARGE

// - {http://www.opengis.net/om}result

// final IObservation<TupleResult> valBenObs = ObservationFeatureFactory.toObservation( valBenFunctFeat );
// if( valBenObs == null )
// return null;
//
// final TupleResult result = valBenObs.getResult();
// if( result == null )
// return null;
// final IComponent compX = TupleResultUtilities.findComponentById( result,
// CriterionValueBenefitRating.DICT_ID_VALUE_NUMBER );
// final IComponent compY = TupleResultUtilities.findComponentById( result, CriterionValueBenefitRating.DICT_ID_RATING
// );
// if( (compX == null) || (compY == null) )
// return null;
// if( !(XmlTypes.isNumber( compX.getValueTypeName() ) && XmlTypes.isNumber( compY.getValueTypeName() )) )
// return null;
//
// // compute valueBenefitFunction(value)
// return interpolate( result, value );

// private Object interpolate( final TupleResult result, final java.lang.Double value )
// {
// final Double vbFuncMax = tupleResultMax( result, CriterionValueBenefitRating.DICT_ID_VALUE_NUMBER,
// CriterionValueBenefitRating.DICT_ID_RATING );
// final Double vbFuncMin = tupleResultMin( result, CriterionValueBenefitRating.DICT_ID_VALUE_NUMBER,
// CriterionValueBenefitRating.DICT_ID_RATING );
//
// // is value to the left or to the right of the function? Then extrapolate
// if( (vbFuncMax == null) || (vbFuncMin == null) || (value == null) )
// return null;
//
// if( (vbFuncMax.x <= value) && (vbFuncMin.x >= value) )
// return interpolate( vbFuncMin, vbFuncMax, value );
// else if( vbFuncMax.x <= value )
// {
// final Point2D.Double[] neighbors = getNeighbors( result, vbFuncMax.x,
// CriterionValueBenefitRating.DICT_ID_VALUE_NUMBER, CriterionValueBenefitRating.DICT_ID_RATING, true, false );
// return interpolate( neighbors[0], vbFuncMax, value );
// }
// else if( vbFuncMin.x >= value )
// {
// final Point2D.Double[] neighbors = getNeighbors( result, vbFuncMin.x,
// CriterionValueBenefitRating.DICT_ID_VALUE_NUMBER, CriterionValueBenefitRating.DICT_ID_RATING, false, true );
// return interpolate( vbFuncMin, neighbors[1], value );
// }
// else
// {
// final Point2D.Double[] neighbors = getNeighbors( result, value, CriterionValueBenefitRating.DICT_ID_VALUE_NUMBER,
// CriterionValueBenefitRating.DICT_ID_RATING, false, false );
// return interpolate( neighbors[0], neighbors[1], value );
// }
// }

    }

    public TupleResult getResult( )
    {
      return m_result;
    }

    public long getRowsCount( )
    {
      return m_rowsCount;
    }
  }

  private final Interpolation m_interpolation;

  private final TabularData m_tabularData;

  public SbkTable( final Feature table )
  {
    m_interpolation = new Interpolation( table );
    m_tabularData = new TabularData( table );
  }

  private Interpolation getInterpolation( )
  {
    return m_interpolation;
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.ISbkTable#getInterpolationPeriod()
   */
  public String getInterpolationPeriod( )
  {
    return getInterpolation().getPeriod();
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.ISbkTable#getInterpolationType()
   */
  public String getInterpolationType( )
  {
    return getInterpolation().getType();
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.ISbkTable#getInterpolationValue()
   */
  public BigInteger getInterpolationValue( )
  {
    return getInterpolation().getValue();
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.ISbkTable#getRowsCount()
   */
  public long getRowsCount( )
  {
    return getTabularData().getRowsCount();
  }

  private TabularData getTabularData( )
  {
    return m_tabularData;
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.ISbkTable#getTupleResult()
   */
  public TupleResult getTupleResult( )
  {
    return getTabularData().getResult();
  }
}
