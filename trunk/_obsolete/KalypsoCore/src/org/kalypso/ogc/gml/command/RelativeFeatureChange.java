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

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.math.BigDecimal;
import java.math.BigInteger;

import org.kalypso.commons.math.MathOperationFactory;
import org.kalypso.core.i18n.Messages;
import org.kalypso.gmlschema.property.IPropertyType;
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
    final Object rawProperty = feature.getProperty( getProperty() );
    if( rawProperty instanceof Number )
    {
      final Number numericProperty = (Number) rawProperty;
      if( "".equals( m_operator ) ) //$NON-NLS-1$
      {
        return m_operand;
      }
      else if( "=".equals( m_operator ) ) //$NON-NLS-1$
      {        
        return castDoubleAsType( numericProperty.getClass(), m_operand );
      }
      else if( "+".equals( m_operator ) ) //$NON-NLS-1$
      {
        return calculate( numericProperty, m_operand, "add" ); //$NON-NLS-1$
      }
      else if( "-".equals( m_operator ) ) //$NON-NLS-1$
      {
        return calculate( numericProperty, m_operand, "subtract" ); //$NON-NLS-1$
      }
      else if( "*".equals( m_operator ) ) //$NON-NLS-1$
      {
        return calculate( numericProperty, m_operand, "multiply" ); //$NON-NLS-1$
      }
      else if( "/".equals( m_operator ) ) //$NON-NLS-1$
      {
        return calculate( numericProperty, m_operand, "divide" ); //$NON-NLS-1$
      }
      else
      {
        throw new IllegalArgumentException( Messages.getString("org.kalypso.ogc.gml.command.RelativeFeatureChange.10") ); //$NON-NLS-1$
      }
    }
    else
    {
      throw new IllegalArgumentException( Messages.getString("org.kalypso.ogc.gml.command.RelativeFeatureChange.11") + rawProperty.getClass().getName() ); //$NON-NLS-1$
    }

  }

  /**
   * Calculates the result either by treating both arguments as a primitive double value or as a
   * {@link java.math.BigDecimal}. The result will be of the same type as the first operand.
   */
  @SuppressWarnings("unchecked") //$NON-NLS-1$
  private <T extends Number> Number calculate( final T firstOperand, final double secondOperand, final String bigTypesMethodName )
  {
    final Number result;
    final Class< ? extends Number> valueClass = firstOperand.getClass();
    if( firstOperand instanceof BigDecimal || firstOperand instanceof BigInteger )
    {
      final Number typedSecondOperand = castDoubleAsType( BigDecimal.class, secondOperand );
      final BigDecimal typedFirstOperand = new BigDecimal( firstOperand.toString() );
      final BigDecimal bigDecimalResult;
      try
      {
        // call specified method (add, subtract, multiply, divide) on BigDecimal
        bigDecimalResult = (BigDecimal) BigDecimal.class.getMethod( bigTypesMethodName, BigDecimal.class ).invoke( typedFirstOperand, typedSecondOperand );
      }
      catch( Exception e )
      {
        throw new IllegalArgumentException( Messages.getString("org.kalypso.ogc.gml.command.RelativeFeatureChange.13") + secondOperand + Messages.getString("org.kalypso.ogc.gml.command.RelativeFeatureChange.14") + bigTypesMethodName + Messages.getString("org.kalypso.ogc.gml.command.RelativeFeatureChange.15") + firstOperand, e ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
      }
      if( firstOperand instanceof BigInteger )
      {
        result = bigDecimalResult.toBigInteger();
      }
      else
      // firstOperand instanceof BigDecimal
      {
        result = bigDecimalResult;
      }
    }
    else
    // firstOperand instanceof Number
    {
      final Double calcResult = MathOperationFactory.createMathOperation( m_operator ).calculate( firstOperand.doubleValue(), secondOperand );
      result = castDoubleAsType( valueClass, calcResult );
    }
    return result;
  }

  /**
   * Tries to construct an Object of class <code>type</code> using its constructor that takes a String argument. Such
   * a constructor exists for all subtypes of {@link Number} in the java library, i.e. the primitive wrapper types
   * {@link Byte}, {@link Double}, {@link Float}, {@link Integer}, {@link Long}, {@link Short} and the big number
   * types {@link java.math.BigInteger} and {@link java.math.BigDecimal}. Both the double value and the double value
   * cast as a long are tried. If both strings are not of an appropriate format, a {@link NumberFormatException} will be
   * thrown. If any other error occurs, an {@link IllegalArgumentException} will be thrown.
   */
  private <T extends Number> Number castDoubleAsType( final Class<? extends Number> type, final double doubleValue ) throws NumberFormatException
  {
    Number result;
    try
    {
      final Constructor<? extends Number> stringConstructor = type.getConstructor( String.class );
      try
      {
        result = stringConstructor.newInstance( "" + doubleValue ); //$NON-NLS-1$
      }
      catch( InvocationTargetException e )
      {
        result = stringConstructor.newInstance( "" + (long) doubleValue ); //$NON-NLS-1$
      }
    }
    catch( InvocationTargetException e )
    {
      // the underlying constructor has probably thrown a NumberFormatException
      final NumberFormatException newException = new NumberFormatException( Messages.getString("org.kalypso.ogc.gml.command.RelativeFeatureChange.18") + doubleValue + Messages.getString("org.kalypso.ogc.gml.command.RelativeFeatureChange.19") + type.getClass().getName() ); //$NON-NLS-1$ //$NON-NLS-2$
      newException.initCause( e );
      throw newException;
    }
    catch( Exception e )
    {
      throw new IllegalArgumentException( Messages.getString("org.kalypso.ogc.gml.command.RelativeFeatureChange.20") + doubleValue + Messages.getString("org.kalypso.ogc.gml.command.RelativeFeatureChange.21") + type.getClass().getName(), e ); //$NON-NLS-1$ //$NON-NLS-2$
    }
    return result;
  }
  
  /**
   * checks if the property type is a {@link org.kalypso.gmlschema.property.IValuePropertyType} and can be cast as
   * {@link Number}
   */
  @SuppressWarnings("unchecked") //$NON-NLS-1$
  public static boolean isNumeric( IPropertyType propertyType )
  {
    return propertyType instanceof IValuePropertyType && Number.class.isAssignableFrom( ((IValuePropertyType) propertyType).getValueClass() );
  }
}
