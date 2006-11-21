/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
 
 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.contribs.eclipse.jface.dialogs;

import java.text.DateFormat;
import java.text.ParseException;
import java.util.Date;

import org.eclipse.jface.dialogs.IInputValidator;

/**
 * TypeBasedInputValidator
 * 
 * @author schlienger
 */
public class TypeBasedInputValidator implements IInputValidator
{
  private Validator m_val;

  /**
   * Constructor
   * 
   * @param type
   */
  public TypeBasedInputValidator( final Class type )
  {
    m_val = createValidator( type );
  }

  /**
   * @param type
   * @return validator
   */
  private Validator createValidator( Class type )
  {
    if( Date.class.isAssignableFrom( type ) )
      return new DateValidator();

    return new PseudoValidator();
  }

  /**
   * @return a default value that is valid for this validator.
   */
  public String defaultValue()
  {
    return m_val.defaultValue();
  }

  /**
   * @see org.eclipse.jface.dialogs.IInputValidator#isValid(java.lang.String)
   */
  public String isValid( String newText )
  {
    return m_val.validate( newText );
  }

  /**
   * @param text
   * @return the real value behind the given text
   */
  public Object toValue( final String text )
  {
    return m_val.toValue( text );
  }

  /**
   * Validator
   * 
   * @author schlienger
   */
  private static interface Validator
  {
    /**
     * @return a default value
     */
    public String defaultValue();

    /**
     * @param value
     * @return error message or null if ok
     */
    public String validate( final String value );

    /**
     * @param text
     * @return real value behind the given text
     */
    public Object toValue( final String text );
  }

  /**
   * PseudoValidator that always validates to true.
   * 
   * @author schlienger
   */
  private static class PseudoValidator implements Validator
  {
    /**
     * @see org.kalypso.contribs.eclipse.jface.dialogs.TypeBasedInputValidator.Validator#validate(java.lang.String)
     */
    public String validate( String value )
    {
      return null;
    }

    /**
     * @see org.kalypso.contribs.eclipse.jface.dialogs.TypeBasedInputValidator.Validator#defaultValue()
     */
    public String defaultValue()
    {
      return "";
    }

    /**
     * @see org.kalypso.contribs.eclipse.jface.dialogs.TypeBasedInputValidator.Validator#toValue(java.lang.String)
     */
    public Object toValue( String text )
    {
      return text;
    }
  }

  /**
   * DateValidator that validates using the default locale DateFormat.
   * 
   * @author schlienger
   */
  private static class DateValidator implements Validator
  {
    private static final DateFormat DF = DateFormat.getDateTimeInstance();

    /**
     * @see org.kalypso.contribs.eclipse.jface.dialogs.TypeBasedInputValidator.Validator#validate(java.lang.String)
     */
    public String validate( String value )
    {
      try
      {
        DF.parse( value );

        return null;
      }
      catch( ParseException e )
      {
        return e.getLocalizedMessage();
      }
    }

    /**
     * @see org.kalypso.contribs.eclipse.jface.dialogs.TypeBasedInputValidator.Validator#defaultValue()
     */
    public String defaultValue()
    {
      return DF.format( new Date() );
    }

    /**
     * @see org.kalypso.contribs.eclipse.jface.dialogs.TypeBasedInputValidator.Validator#toValue(java.lang.String)
     */
    public Object toValue( String text )
    {
      try
      {
        return DF.parse( text );
      }
      catch( ParseException e )
      {
        // should not be the case
        e.printStackTrace();

        return null;
      }
    }
  }
}
