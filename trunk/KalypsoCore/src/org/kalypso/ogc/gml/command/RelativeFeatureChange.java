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
package org.kalypso.ogc.gml.command;

import java.lang.reflect.InvocationTargetException;
import java.math.BigDecimal;
import java.math.BigInteger;

import org.kalypso.commons.math.MathOperationFactory;
import org.kalypso.gmlschema.property.IValuePropertyType;
import org.kalypsodeegree.model.feature.Feature;

/**
 * This class models a feature change relative to the original value. This includes changes like adding a certain
 * percentage or constant value or multiplying by some value.
 * 
 * @author skurzbach
 */
public class RelativeFeatureChange extends FeatureChange
{

  private double m_operand;

  private String m_operator;

  /**
   * Creates a new RelativeFeatureChange that operates on a given feature and changes the given property to a new value.
   * The new value is calculated applying the given operator to the property value as the first and the parameter
   * <code>value</code> as the second operand. This is only valid if the propertyType is numeric.
   * 
   * @param feature
   *          the feature
   * @param propertyType
   *          the property of the feature to change
   * @param operator
   *          one of +, -, *, / and the empty string, which denotes that <code>value</code> is a constant that should
   *          be set to the property
   * @param value
   *          the operand
   */
  public RelativeFeatureChange( final Feature feature, final IValuePropertyType propertyType, final String operator, final double operand )
  {
    super( feature, propertyType, null );
    m_operator = operator;
    m_operand = operand;
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.FeatureChange#getNewValue()
   */
  @Override
  public Object getNewValue( )
  {
    final Feature feature = getFeature();
    final Object property = feature.getProperty( getProperty() );
    if( "".equals( m_operator ) )
    {
      return m_operand;
    }
    else if( "+".equals( m_operator ) )
    {
      return calculate( property, m_operand, "add" );
    }
    else if( "-".equals( m_operator ) )
    {
      return calculate( property, m_operand, "subtract" );
    }
    else if( "*".equals( m_operator ) )
    {
      return calculate( property, m_operand, "multiply" );
    }
    else if( "/".equals( m_operator ) )
    {
      return calculate( property, m_operand, "divide" );
    }
    else
    {
      throw new IllegalArgumentException( "operator was not one of empty string, +, -, *, /." );
    }
  }

  private Object calculate( Object firstOperand, double secondOperand, final String bigTypesMethodName )
  {
    final Class< ? > valueClass = firstOperand.getClass();
    if( firstOperand instanceof BigDecimal || firstOperand instanceof BigInteger )
    {
      try
      {
        final Object typedSecondOperand = castDouble( valueClass, secondOperand );
        return valueClass.getMethod( bigTypesMethodName, firstOperand.getClass() ).invoke( firstOperand, typedSecondOperand );
      }
      catch( Exception e )
      {
        e.printStackTrace();
        return null;
      }
    }
    else if( firstOperand instanceof Number )
    {
      final double typedSecondOperand = ((Number) castDouble( valueClass, secondOperand )).doubleValue();
      final Double calcResult = MathOperationFactory.createMathOperation( m_operator ).calculate( ((Number) firstOperand).doubleValue(), typedSecondOperand );
      return castDouble( valueClass, calcResult );
    }
    else
    {
      throw new IllegalArgumentException( "first argument not numeric, type was: " + firstOperand.getClass().getName() );
    }
  }

  private <T> T castDouble( final Class<T> valueClass, final double doubleValue )
  {
    T result;
    try
    {
      try
      {
        result = valueClass.getConstructor( String.class ).newInstance( "" + doubleValue );
      }
      catch( InvocationTargetException e )
      {
        result = valueClass.getConstructor( String.class ).newInstance( "" + (long) doubleValue );
      }
      return result;
    }
    catch( Exception e )
    {
      e.printStackTrace();
      return null;
    }
  }
}
