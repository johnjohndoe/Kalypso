package org.kalypso.ogc.gml.typehandler;

import org.deegree.xml.XMLTools;
import org.deegree_impl.extension.ITypeHandler;
import org.deegree_impl.extension.TypeRegistryException;
import org.kalypso.java.xml.XMLUtilities;
import org.w3c.dom.Node;

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

public class DiagramTypeHandler implements ITypeHandler
{

  private final String m_typeName = "DiagramType";

  private final String m_typeNamespace = "http://www.tuhh.de/kalypsoNA";

  private final Class m_typeClass = DiagramProperty.class;

  /*
   * 
   * @author huebsch
   */

  /**
   * @see org.deegree_impl.extension.ITypeHandler#getClassName()
   */
  public String getClassName()
  {
    return m_typeClass.getName();
  }

  /**
   * @see org.deegree_impl.extension.ITypeHandler#getTypeName()
   */
  public String getTypeName()
  {
    return m_typeNamespace + ":" + m_typeName;
  }

  /**
   * @see org.deegree_impl.extension.ITypeHandler#marshall(java.lang.Object,
   *      org.w3c.dom.Node)
   */
  public void marshall( Object object, Node node ) throws TypeRegistryException
  {
    try
    {
      DiagramProperty diagram = (DiagramProperty)object;
      StringBuffer content = new StringBuffer();
      for( int i = 0; i < diagram.size(); i++ )
      {
        Double xValue = diagram.getXValue( i );
        Double yValue = diagram.getYValue( i );
        Double zValue = diagram.getZValue( i );
        content.append( xValue );
        content.append( "," );
        content.append( yValue );
        content.append( "," );
        content.append( zValue );
        if( i + 1 < diagram.size() )
          content.append( " " );
      }

      XMLUtilities.setTextNode( node.getOwnerDocument(), node, content.toString() );
    }
    catch( Exception e )
    {
      throw new TypeRegistryException( e );
    }
  }

  /**
   * @see org.deegree_impl.extension.ITypeHandler#unmarshall(org.w3c.dom.Node)
   */
  public Object unmarshall( Node node ) throws TypeRegistryException
  {
    try
    {
      DiagramProperty result = new DiagramProperty();
      String content = XMLTools.getStringValue( node );
      if( content.length() > 0 )
      {
        String[] tuppel = content.split( "\\s" );
        for( int i = 0; i < tuppel.length; i++ )
        {
          String[] values = tuppel[i].split( "," );
          result
              .addValue( new Double( values[0] ), new Double( values[1] ), new Double( values[2] ) );
        }
      }
      return result;
    }
    catch( Exception e )
    {
      throw new TypeRegistryException( e );
    }
  }

  /**
   * @see org.deegree_impl.extension.ITypeHandler#getShortname()
   */
  public String getShortname()
  {
    return "Diagramm";
  }

}