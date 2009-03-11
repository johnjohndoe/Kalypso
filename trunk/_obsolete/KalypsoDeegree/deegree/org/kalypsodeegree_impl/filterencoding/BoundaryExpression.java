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

import org.kalypsodeegree.filterencoding.Expression;
import org.kalypsodeegree.filterencoding.FilterEvaluationException;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author F.Lindemann
 *  
 */
public class BoundaryExpression extends Expression_Impl implements Expression
{

  private String m_value = null;

  public BoundaryExpression( String value1 )
  {
    this.m_value = value1;
  }

  @Override
  public StringBuffer toXML()
  {
    return new StringBuffer( m_value.toString() );
  }

  public Object evaluate( Feature feature ) throws FilterEvaluationException
  {
    Double returnValue = null;
    try
    {
      returnValue = new Double( m_value );
    }
    catch( NumberFormatException e )
    {
      throw new FilterEvaluationException( "BoundaryExpression:  can only be applied to numerical " + "expressions!" );
    }
    return returnValue;
  }

  public String getValue()
  {
    return m_value;
  }

  public void setValue( String value )
  {
    m_value = value;
  }
}