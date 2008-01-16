/*--------------- Kalypso-Deegree-Header ------------------------------------------------------------

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
 
 
 history:
 
 Files in this package are originally taken from deegree and modified here
 to fit in kalypso. As goals of kalypso differ from that one in deegree
 interface-compatibility to deegree is wanted but not retained always. 
 
 If you intend to use this software in other ways than in kalypso 
 (e.g. OGC-web services), you should consider the latest version of deegree,
 see http://www.deegree.org .

 all modifications are licensed as deegree, 
 original copyright:
 
 Copyright (C) 2001 by:
 EXSE, Department of Geography, University of Bonn
 http://www.giub.uni-bonn.de/exse/
 lat/lon GmbH
 http://www.lat-lon.de
 
 ---------------------------------------------------------------------------------------------------*/
package org.kalypsodeegree_impl.filterencoding;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.CoreException;
import org.kalypsodeegree.KalypsoDeegreeExtensions;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.filterencoding.Expression;
import org.kalypsodeegree.filterencoding.FilterConstructionException;
import org.kalypsodeegree.filterencoding.FilterEvaluationException;
import org.kalypsodeegree.filterencoding.IFunctionExpression;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.xml.ElementList;
import org.kalypsodeegree.xml.XMLTools;
import org.w3c.dom.Element;

/**
 * Encapsulates the information of a <Function>element as defined in the Expression DTD.
 * 
 * @author Markus Schneider
 * @version 07.08.2002
 */
public class Function extends Expression_Impl
{
  /** The Function's name (as specified in it's name attribute). */
  private String m_name;

  /** The Function's arguments. */
  private final List<Expression> m_args = new ArrayList<Expression>();

  private IFunctionExpression m_function;

  /** Constructs a new Function. */
  public Function( final String name, final List<Expression> args )
  {
    m_id = ExpressionDefines.FUNCTION;
    m_name = name;
    m_args.addAll( args );
  }

  /**
   * Given a DOM-fragment, a corresponding Expression-object is built. This method recursively calls other buildFromDOM () -
   * methods to validate the structure of the DOM-fragment.
   * 
   * @throws FilterConstructionException
   *             if the structure of the DOM-fragment is invalid
   */
  public static Expression buildFromDOM( final Element element ) throws FilterConstructionException
  {
    // check if root element's name equals 'Function'
    final String localName = element.getLocalName();
    if( !localName.toLowerCase().equals( "function" ) )
      throw new FilterConstructionException( "Name of element does not equal 'Function'!" );

    // determine the name of the Function
    final String name = element.getAttribute( "name" );
    if( name == null )
      throw new FilterConstructionException( "Function's name (-attribute) is unspecified!" );

    // determine the arguments of the Function
    final ElementList children = XMLTools.getChildElements( element );

    final List<Expression> args = new ArrayList<Expression>( children.getLength() );
    for( int i = 0; i < children.getLength(); i++ )
      args.add( Expression_Impl.buildFromDOM( children.item( i ) ) );

    return new Function( name, args );
  }

  /**
   * Returns the Function's name.
   */
  public String getName( )
  {
    return m_name;
  }

  /**
   * @see org.kalypsodeegree_impl.filterencoding.Function#getName()
   */
  public void setName( final String name )
  {
    this.m_name = name;
  }

  /**
   * returns the arguments of the function
   */
  public List<Expression> getArguments( )
  {
    return m_args;
  }

  /** Produces an indented XML representation of this object. */
  @Override
  public StringBuffer toXML( )
  {
    final StringBuffer sb = new StringBuffer( 1000 );
    sb.append( "<ogc:Function name=\"" ).append( m_name ).append( "\">" );

    for( final Expression expr : m_args )
      sb.append( expr.toXML() );

    sb.append( "</ogc:Function>" );
    return sb;
  }

  /**
   * Returns the <tt>Function</tt>'s value (to be used in the evaluation of a complexer <tt>Expression</tt>).
   * 
   * @param feature
   *            that determines the concrete values of <tt>PropertyNames</tt> found in the expression
   * @return the resulting value
   */
  public Object evaluate( final Feature feature ) throws FilterEvaluationException
  {
    try
    {
      if( m_function == null )
      {
        m_function = KalypsoDeegreeExtensions.createFunctionExpression( m_name );
        if( m_function == null )
          throw new FilterEvaluationException( "Unknown function: " + m_name );
      }

      return m_function.evaluate( feature, m_args );
    }
    catch( final CoreException e )
    {
      KalypsoDeegreePlugin.getDefault().getLog().log( e.getStatus() );

      throw new FilterEvaluationException( "Failed to creeate function: " + m_name, e );
    }
  }
}