/** This file is part of kalypso/deegree.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 * history:
 * 
 * Files in this package are originally taken from deegree and modified here
 * to fit in kalypso. As goals of kalypso differ from that one in deegree
 * interface-compatibility to deegree is wanted but not retained always. 
 * 
 * If you intend to use this software in other ways than in kalypso 
 * (e.g. OGC-web services), you should consider the latest version of deegree,
 * see http://www.deegree.org .
 *
 * all modifications are licensed as deegree, 
 * original copyright:
 *
 * Copyright (C) 2001 by:
 * EXSE, Department of Geography, University of Bonn
 * http://www.giub.uni-bonn.de/exse/
 * lat/lon GmbH
 * http://www.lat-lon.de
 */
package org.kalypsodeegree_impl.filterencoding;

import org.kalypsodeegree.filterencoding.FilterConstructionException;
import org.kalypsodeegree.filterencoding.Operation;
import org.kalypsodeegree.filterencoding.visitor.FilterVisitor;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.xml.ElementList;
import org.kalypsodeegree.xml.XMLTools;
import org.w3c.dom.Element;

/**
 * Encapsulates the information of a <PropertyIsLike>-element (as defined in Filter DTD).
 * 
 * @author Markus Schneider
 * @version 10.08.2002
 */
public class PropertyIsLikeOperation extends ComparisonOperation
{

  private PropertyName m_propertyName;

  private Literal m_literal;

  // attributes of <PropertyIsLike>
  private char m_wildCard;

  private char m_singleChar;

  private char m_escapeChar;

  public PropertyIsLikeOperation( PropertyName propertyName, Literal literal, char wildCard, char singleChar, char escapeChar )
  {
    super( OperationDefines.PROPERTYISLIKE );
    m_propertyName = propertyName;
    m_literal = literal;
    m_wildCard = wildCard;
    m_singleChar = singleChar;
    m_escapeChar = escapeChar;
  }

  public char getWildCard( )
  {
    return m_wildCard;
  }

  public char getSingleChar( )
  {
    return m_singleChar;
  }

  public char getEscapeChar( )
  {
    return m_escapeChar;
  }

  public void setWildCard( char wildCard )
  {
    this.m_wildCard = wildCard;
  }

  public void setSingleChar( char singleChar )
  {
    this.m_singleChar = singleChar;
  }

  public void setEscapeChar( char escapeChar )
  {
    this.m_escapeChar = escapeChar;
  }

  /**
   * Given a DOM-fragment, a corresponding Operation-object is built. This method recursively calls other buildFromDOM () -
   * methods to validate the structure of the DOM-fragment.
   * 
   * @throws FilterConstructionException
   *           if the structure of the DOM-fragment is invalid
   */
  public static Operation buildFromDOM( Element element ) throws FilterConstructionException
  {

    // check if root element's name equals 'PropertyIsLike'
    if( !element.getLocalName().equals( "PropertyIsLike" ) )
      throw new FilterConstructionException( "Name of element does not equal 'PropertyIsLike'!" );

    ElementList children = XMLTools.getChildElements( element );
    if( children.getLength() != 2 )
      throw new FilterConstructionException( "'PropertyIsLike' requires exactly 2 elements!" );

    PropertyName propertyName = (PropertyName) PropertyName.buildFromDOM( children.item( 0 ) );
    Literal literal = (Literal) Literal.buildFromDOM( children.item( 1 ) );

    // determine the needed attributes
    String wildCard = element.getAttribute( "wildCard" );
    if( wildCard == null || wildCard.length() == 0 )
      throw new FilterConstructionException( "wildCard-Attribute is unspecified!" );
    if( wildCard.length() != 1 )
      throw new FilterConstructionException( "wildCard-Attribute must be exactly one character!" );
    String singleChar = element.getAttribute( "singleChar" );
    if( singleChar == null || singleChar.length() == 0 )
      throw new FilterConstructionException( "singleChar-Attribute is unspecified!" );
    if( singleChar.length() != 1 )
      throw new FilterConstructionException( "singleChar-Attribute must be exactly one character!" );
    String escapeChar = element.getAttribute( "escape" );
    if( escapeChar == null || escapeChar.length() == 0 )
      escapeChar = element.getAttribute( "escapeChar" );
    if( escapeChar == null || escapeChar.length() == 0 )
      throw new FilterConstructionException( "escape-Attribute is unspecified!" );
    if( escapeChar.length() != 1 )
      throw new FilterConstructionException( "escape-Attribute must be exactly one character!" );

    return new PropertyIsLikeOperation( propertyName, literal, wildCard.charAt( 0 ), singleChar.charAt( 0 ), escapeChar.charAt( 0 ) );
  }

  /**
   * returns the name of the property that shall be compared to the literal
   */
  public PropertyName getPropertyName( )
  {
    return m_propertyName;
  }

  public void setPropertyName( PropertyName propName )
  {
    m_propertyName = propName;
  }

  /**
   * returns the literal the property shall be compared to
   */
  public Literal getLiteral( )
  {
    return m_literal;
  }

  public void setLiteral( Literal literal )
  {
    this.m_literal = literal;
  }

  /** Produces an indented XML representation of this object. */
  public StringBuffer toXML( )
  {
    StringBuffer sb = new StringBuffer( 500 );
    sb.append( "<ogc:" ).append( getOperatorName() ).append( " wildCard=\"" ).append( m_wildCard ).append( "\" singleChar=\"" ).append( m_singleChar ).append( "\" escape=\"" ).append( m_escapeChar ).append( "\">" ).append( m_propertyName.toXML() ).append( m_literal.toXML() );
    sb.append( "</ogc:" ).append( getOperatorName() ).append( ">" );
    return sb;
  }

  /**
   * Calculates the <tt>PropertyIsLike</tt>'s logical value based on the certain property values of the given
   * <tt>Feature</tt>.
   * <p>
   * 
   * @param feature
   *          that determines the property values
   * @return true, if the <tt>Literal</tt> matches the <tt>PropertyName</tt>'s value
   */
  public boolean evaluate( Feature feature )
  {

    Object value1 = null;
    Object value2 = null;
    try
    {
      value1 = m_propertyName.evaluate( feature );
      value2 = m_literal.getValue();
      if( value1 == null || value2 == null )
        return false;
      return matches( value2.toString(), value1.toString() );
    }
    catch( Exception e )
    {
      // nothing
    }
    return false;
  }

  /**
   * Checks if a given <tt>String<tt> matches a pattern that is a sequence
   * of:
   * <ul>
   *   <li>standard characters</li>
   *   <li>wildcard characters (like * in most shells)</li>
   *   <li>singlechar characters (like ? in most shells)</li>
   * </ul>
   * @param pattern the pattern to compare to
   * @param buffer the <tt>String</tt> to test
   * @return true, if the <tt>String</tt> matches the pattern
   */
  public boolean matches( String pattern, String buffer )
  {
    // match was successful if both the pattern and the buffer are empty
    if( pattern.length() == 0 && buffer.length() == 0 )
      return true;

    // build the prefix that has to match the beginning of the buffer
    // prefix ends at the first (unescaped!) wildcard / singlechar character
    StringBuffer sb = new StringBuffer();
    boolean escapeMode = false;
    int length = pattern.length();
    char specialChar = '\0';

    for( int i = 0; i < length; i++ )
    {
      char c = pattern.charAt( i );

      if( escapeMode )
      {
        // just append every character (except the escape character)
        if( c != m_escapeChar )
          sb.append( c );
        escapeMode = false;
      }
      else
      {
        // escapeChar means: switch to escapeMode
        if( c == m_escapeChar )
          escapeMode = true;
        // wildCard / singleChar means: prefix ends here
        else if( c == m_wildCard || c == m_singleChar )
        {
          specialChar = c;
          break;
        }
        else
          sb.append( c );
      }
    }
    String prefix = sb.toString();
    int skip = prefix.length();

    // the buffer must begin with the prefix or else there is no match
    if( !buffer.startsWith( prefix ) )
      return false;

    if( specialChar == m_wildCard )
    {
      // the prefix is terminated by a wildcard-character
      pattern = pattern.substring( skip + 1, pattern.length() );
      // try to find a match for the rest of the pattern
      for( int i = skip; i <= buffer.length(); i++ )
      {
        String rest = buffer.substring( i, buffer.length() );
        if( matches( pattern, rest ) )
          return true;
      }
    }
    else if( specialChar == m_singleChar )
    {
      // the prefix is terminated by a singlechar-character
      pattern = pattern.substring( skip + 1, pattern.length() );
      if( skip + 1 > buffer.length() )
        return false;
      String rest = buffer.substring( skip + 1, buffer.length() );
      if( matches( pattern, rest ) )
        return true;
    }
    else if( specialChar == '\0' )
    {
      // the prefix is terminated by the end of the pattern
      if( buffer.length() == prefix.length() )
        return true;
    }
    return false;
  }

  /**
   * @see org.kalypsodeegree.filterencoding.Operation#accept(org.kalypsodeegree.filterencoding.visitor.FilterVisitor,
   *      org.kalypsodeegree.filterencoding.Operation, int)
   */
  public void accept( FilterVisitor fv, Operation operation, int depth )
  {
    fv.visit( this );
  }
}