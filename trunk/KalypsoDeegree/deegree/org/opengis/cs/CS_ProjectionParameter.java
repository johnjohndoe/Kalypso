/*
 * OpenGIS� Coordinate Transformation Services Implementation Specification
 * Copyright (2001) OpenGIS consortium
 * 
 * THIS COPYRIGHT NOTICE IS A TEMPORARY PATCH. Version 1.00 of official
 * OpenGIS's interface files doesn't contain a copyright notice yet. This file
 * is a slightly modified version of official OpenGIS's interface. Changes have
 * been done in order to fix RMI problems and are documented on the SEAGIS web
 * site (seagis.sourceforge.net). THIS FILE WILL LIKELY BE REPLACED BY NEXT
 * VERSION OF OPENGIS SPECIFICATIONS.
 */
package org.opengis.cs;

// JDK's classes
import java.io.Serializable;

/**
 * A named projection parameter value. The linear units of parameters' values
 * match the linear units of the containing projected coordinate system. The
 * angular units of parameter values match the angular units of the geographic
 * coordinate system that the projected coordinate system is based on. (Notice
 * that this is different from {@link org.opengis.ct.CT_Parameter}, where the
 * units are always meters and degrees.)
 * 
 * @version 1.01
 * @since 1.00
 * @author Martin Daly
 * @author Martin Desruisseaux
 */
public class CS_ProjectionParameter implements Cloneable, Serializable
{
  /**
   * Use <code>serialVersionUID</code> from first draft for interoperability
   * with CSS 1.00.
   */
  private static final long serialVersionUID = -167750008062828854L;

  /**
   * The parameter name.
   */
  public String name;

  /**
   * The parameter value.
   */
  public double value;

  /**
   * Construct an empty parameter. Caller must initialize {@link #name}and
   * {@link #value}.
   */
  public CS_ProjectionParameter()
  {}

  /**
   * Construct a named parameter.
   * 
   * @param name
   *          The parameter name.
   * @param value
   *          The parameter value.
   */
  public CS_ProjectionParameter( final String name, final double value )
  {
    this.name = name;
    this.value = value;
  }

  /**
   * Returns a hash value for this parameter. This value need not remain
   * consistent between different implementations of the same class.
   */
  public int hashCode()
  {
    final long longCode = Double.doubleToLongBits( value );
    int code = (int)( longCode >>> 32 ) ^ (int)longCode;
    if( name != null )
      code ^= name.hashCode();
    return code;
  }

  /**
   * Returns a copy of this parameter.
   */
  public Object clone()
  {
    try
    {
      return super.clone();
    }
    catch( CloneNotSupportedException exception )
    {
      // Should not happen, since we are cloneable.
      throw new InternalError( exception.getMessage() );
    }
  }

  /**
   * Compares the specified object with this parameter for equality.
   */
  public boolean equals( final Object object )
  {
    if( object != null && getClass().equals( object.getClass() ) )
    {
      final CS_ProjectionParameter that = (CS_ProjectionParameter)object;
      return Double.doubleToLongBits( this.value ) == Double.doubleToLongBits( that.value )
          && ( this.name == that.name || ( name != null && name.equals( that.name ) ) );
    }
    else
      return false;
  }

  /**
   * Returns a string repr�sentation of this parameter. The returned string is
   * implementation dependent. It is usually provided for debugging purposes
   * only.
   */
  public String toString()
  {
    final StringBuffer buffer = new StringBuffer( "CS_ProjectionParameter" );
    buffer.append( '[' );
    buffer.append( name );
    buffer.append( '=' );
    buffer.append( value );
    buffer.append( ']' );
    return buffer.toString();
  }
}