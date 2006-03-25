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
package org.kalypsodeegree_impl.model.feature.xpath;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * a segment is something in the featurepath between two '/' <br>
 * <br>
 * syntax: propertyName[conditions]<br>
 * <br>
 * <li>propertyName can be substitued by '*'</li>
 * <br>
 * <br>
 * conditions are:
 * <li>'@fid='foobar' : selects a featureID</li>
 * <li>name()='foobar' : name of featuretype </li>
 * <li>// : any featureassociation inline any levels</li>
 * <br>
 * <br>
 * list of deprecated syntax (new syntax): <br>
 * <li>/#fid#foobar ( /*[@fid='foobar'] )</li>
 * <li>/member[Catchment] ( /member[name()='Catchment'] )</li>
 * <br>
 * old syntax will be converted to new syntax for backward compatibility
 * 
 * @author doemming
 */
final class Segment
{
  /**
   * @deprecated
   */
  private final static String ID_MARKER = "#fid#";

  private final static Pattern DEPRECATED_FT_PATTERN = Pattern.compile( "^(.*)\\[([^=\\(\\)@']+)\\]" );

  private final static FeaturePathConditionFactory m_fac = new FeaturePathConditionFactory();

  private final IXElement m_addressXElement;

  private final IXElement m_conditionXElement;

  private final String m_condition;

  private final String m_address;

  private final String m_segment;

  public Segment( final Feature feature )
  {
    this( "id( '" + feature.getId() + "' )" );
  }

  public Segment( final String segment )
  {
    m_segment = segment;
    final String segmentString = convertDeprecatedFormat( segment );
    int index = segmentString.indexOf( "[" );
    final String address;
    final String condition;
    if( index < 0 )
    {
      address = segmentString;
      condition = null;
    }
    else
    {
      address = segmentString.substring( 0, index );
      condition = segmentString.substring( index );
    }
    final Cond addressCond = new Cond( address );
    m_addressXElement = m_fac.create( addressCond );
    if( condition != null )
    {
      final Cond conditionCond = new Cond( condition );
      m_conditionXElement = m_fac.create( conditionCond );
    }
    else
      m_conditionXElement = null;
    m_condition = condition;
    m_address = address;
  }

  private String convertDeprecatedFormat( String segment )
  {
    if( segment.startsWith( ID_MARKER ) )
    {
      final String fid = segment.substring( ID_MARKER.length() );
      return "id( '" + fid + "' )";
    }
    final Matcher matcher = DEPRECATED_FT_PATTERN.matcher( segment );
    if( matcher.matches() )
    {
      final String newPath = matcher.group( 1 ) + "[ name() = '" + matcher.group( 2 ) + "' ]";
      return newPath;
    }
    return segment;
  }

  public Object getValue( final GMLWorkspace workspace, final Feature feature ) throws FeaturePathException
  {
    final Object result1 = m_addressXElement.evaluate( workspace, feature );
    if( m_conditionXElement == null )
      return result1;

    if( result1 instanceof Feature )
    {
      final Feature resultFE = (Feature) result1;
      Boolean b = (Boolean) m_conditionXElement.evaluate( workspace, resultFE );
      if( b )
        return result1;
      return null;
    }
    if( result1 instanceof FeatureList )
    {
      final FeatureList feList = (FeatureList) result1;
      return new FeatureListWithXPathCondition( workspace, feList, m_conditionXElement );
    }
    // TODO featurelist...
    return result1;
  }

  // public IFeatureType getType( final GMLWorkspace workspace, final IFeatureType featureType )
  // {
  // if( isID() )
  // return workspace.getFeature( getName() ).getFeatureType();
  //
  // final IPropertyType ftp = featureType.getProperty( getName() );
  // if( ftp instanceof IRelationType )
  // {
  // final IRelationType relationPT = (IRelationType) ftp;
  // if( m_typename != null )
  // {
  // final IFeatureType[] associationFeatureTypes = relationPT.getTargetFeatureTypes( null, false );
  // for( int i = 0; i < associationFeatureTypes.length; i++ )
  // {
  // final IFeatureType type = associationFeatureTypes[i];
  // if( m_typename.equals( type.getName() ) )
  // return type;
  // }
  //
  // return null;
  // }
  //
  // final IFeatureType[] targetFeatureTypes = relationPT.getTargetFeatureTypes( null, true );
  // if( targetFeatureTypes.length > 0 )
  // return targetFeatureTypes[0];
  // }
  // return null;
  // }

  /**
   * @see java.lang.Object#toString()
   */
  @Override
  public String toString( )
  {
    return m_segment;
    // final StringBuffer buffer = new StringBuffer();
    // buffer.append( m_address );
    // if( m_condition != null )
    // buffer.append( "[" + m_condition + "]" );
    // return buffer.toString();
  }

}