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
package org.kalypso.ui.editor.styleeditor.dialogs.filterencoding;

import org.deegree.filterencoding.FilterEvaluationException;
import org.deegree.model.feature.Feature;
import org.deegree_impl.filterencoding.Expression_Impl;

/**
 * @author F.Lindemann
 *  
 */
public class BoundaryExpression extends Expression_Impl
{

  private String value = null;

  public BoundaryExpression( String m_value )
  {
    this.value = m_value;
  }

  public StringBuffer toXML()
  {
    return new StringBuffer( value.toString() );
  }

  public Object evaluate( Feature feature ) throws FilterEvaluationException
  {
    Double returnValue = null;
    try
    {
      returnValue = new Double( value );
    }
    catch( NumberFormatException e )
    {
      throw new FilterEvaluationException( "BoundaryExpression:  can only be applied to numerical "
          + "expressions!" );
    }
    return returnValue;
  }

  public String getValue()
  {
    return value;
  }

  public void setValue( String m_value )
  {
    this.value = m_value;
  }
}