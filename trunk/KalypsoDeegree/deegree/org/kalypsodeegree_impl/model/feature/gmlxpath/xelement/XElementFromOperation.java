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
package org.kalypsodeegree_impl.model.feature.gmlxpath.xelement;

import org.kalypsodeegree_impl.model.feature.gmlxpath.GMLXPathException;
import org.kalypsodeegree_impl.model.feature.gmlxpath.IGMLXPathOperation;

/**
 * xelement that represents a operation xpath element
 * 
 * @author doemming
 */
public class XElementFromOperation implements IXElement
{
  private final IXElement m_operand1;

  private final IXElement m_operand2;

  private final IGMLXPathOperation m_operation;

  public XElementFromOperation( final IXElement operand1, final IGMLXPathOperation operation, final IXElement operand2 )
  {
    m_operand1 = operand1;
    m_operation = operation;
    m_operand2 = operand2;
  }

  /**
   * @see org.kalypsodeegree_impl.model.feature.xpath.IXElement#evaluate(java.lang.Object, boolean)
   */
  public Object evaluate( final Object context, final boolean featureTypeLevel ) throws GMLXPathException
  {
    final Object value1 = m_operand1.evaluate( context, featureTypeLevel );
    final Object value2 = m_operand2.evaluate( context, featureTypeLevel );
    return m_operation.operate( value1, value2 );
  }

}
