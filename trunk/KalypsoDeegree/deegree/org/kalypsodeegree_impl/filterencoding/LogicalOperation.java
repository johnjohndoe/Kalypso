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

import org.kalypsodeegree.filterencoding.FilterConstructionException;
import org.kalypsodeegree.filterencoding.FilterEvaluationException;
import org.kalypsodeegree.filterencoding.Operation;
import org.kalypsodeegree.filterencoding.visitor.FilterVisitor;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.xml.ElementList;
import org.kalypsodeegree.xml.XMLTools;
import org.w3c.dom.Element;

/**
 * Encapsulates the information of a logical_ops entity (as defined in the Filter DTD).
 * 
 * @author Markus Schneider
 * @version 10.08.2002
 */
public class LogicalOperation extends AbstractOperation
{
  /** Arguments of the Operation. */
  private List<Operation> m_arguments = new ArrayList<Operation>();

  /**
   * Constructs a new LogicalOperation.
   * 
   * @see OperationDefines
   */
  public LogicalOperation( final int operatorId, final ArrayList<Operation> arguments )
  {
    super( operatorId );
    m_arguments = arguments;
  }

  /**
   * Returns the arguments of the operation. These are <tt>Operations</tt> as well.
   */
  public List<Operation> getArguments( )
  {
    return m_arguments;
  }

  public void setArguments( final List<Operation> arguments )
  {
    m_arguments = arguments;
  }

  /**
   * Given a DOM-fragment, a corresponding Operation-object is built. This method recursively calls other buildFromDOM () -
   * methods to validate the structure of the DOM-fragment.
   * 
   * @throws FilterConstructionException
   *             if the structure of the DOM-fragment is invalid
   */
  public static Operation buildFromDOM( final Element element ) throws FilterConstructionException
  {

    // check if root element's name is a known operator
    final String name = element.getLocalName();
    final int operatorId = OperationDefines.getIdByName( name );
    final ArrayList<Operation> arguments = new ArrayList<Operation>();

    switch( operatorId )
    {
      case OperationDefines.AND:
      case OperationDefines.OR:
      {
        final ElementList children = XMLTools.getChildElements( element );
        if( children.getLength() < 2 )
          throw new FilterConstructionException( "'" + name + "' requires at least 2 elements!" );
        for( int i = 0; i < children.getLength(); i++ )
        {
          final Element child = children.item( i );
          final Operation childOperation = AbstractOperation.buildFromDOM( child );
          arguments.add( childOperation );
        }
        break;
      }
      case OperationDefines.NOT:
      {
        final ElementList children = XMLTools.getChildElements( element );
        if( children.getLength() != 1 )
          throw new FilterConstructionException( "'" + name + "' requires exactly 1 element!" );
        final Element child = children.item( 0 );
        final Operation childOperation = AbstractOperation.buildFromDOM( child );
        arguments.add( childOperation );
        break;
      }
      default:
      {
        throw new FilterConstructionException( "'" + name + "' is not a logical operator!" );
      }
    }
    return new LogicalOperation( operatorId, arguments );
  }

  /** Produces an indented XML representation of this object. */
  public StringBuffer toXML( )
  {
    final StringBuffer sb = new StringBuffer( 1000 );
    sb.append( "<ogc:" ).append( getOperatorName() ).append( ">" );

    for( int i = 0; i < m_arguments.size(); i++ )
    {
      final Operation operation = m_arguments.get( i );
      sb.append( operation.toXML() );
    }

    sb.append( "</ogc:" ).append( getOperatorName() ).append( ">" );
    return sb;
  }

  /**
   * Calculates the <tt>LogicalOperation</tt>'s logical value based on the certain property values of the given
   * <tt>Feature</tt>.
   * 
   * @param feature
   *            that determines the property values
   * @return true, if the <tt>LogicalOperation</tt> evaluates to true, else false
   * @throws FilterEvaluationException
   *             if the evaluation fails
   */
  public boolean evaluate( final Feature feature ) throws FilterEvaluationException
  {
    switch( getOperatorId() )
    {
      case OperationDefines.AND:
      {
        for( int i = 0; i < m_arguments.size(); i++ )
        {
          final Operation operation = m_arguments.get( i );
          if( !operation.evaluate( feature ) )
            return false;
        }
        return true;
      }
      case OperationDefines.OR:
      {
        for( int i = 0; i < m_arguments.size(); i++ )
        {
          final Operation operation = m_arguments.get( i );
          if( operation.evaluate( feature ) )
            return true;
        }
        return false;
      }
      case OperationDefines.NOT:
      {
        final Operation operation = m_arguments.get( 0 );
        return !operation.evaluate( feature );
      }
      default:
      {
        throw new FilterEvaluationException( "Unknown LogicalOperation encountered: '" + getOperatorName() + "'" );
      }
    }
  }

  /**
   * @see org.kalypsodeegree.filterencoding.Operation#accept(org.kalypsodeegree.filterencoding.visitor.FilterVisitor,
   *      org.kalypsodeegree.filterencoding.Operation, int)
   */
  public void accept( final FilterVisitor fv, final Operation operation, final int depth )
  {
    if( operation != null && OperationDefines.getTypeById( operation.getOperatorId() ) == OperationDefines.TYPE_LOGICAL )
    {
      final List<Operation> ops = getArguments();
      for( final Operation o : ops )
      {
        final boolean recurse = fv.visit( o );
        if( recurse && depth != FilterVisitor.DEPTH_ZERO )
          accept( fv, o, depth );
      }
    }
  }
}