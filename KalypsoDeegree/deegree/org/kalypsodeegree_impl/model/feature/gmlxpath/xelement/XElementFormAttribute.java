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

import org.kalypsodeegree.model.feature.Feature;

/**
 * XElement to handle attribute xpath elements<br>
 * 
 * @author doemming
 */
public class XElementFormAttribute extends AbstractXElement
{
  private String m_attribute;

  public XElementFormAttribute( String condition )
  {
    m_attribute = condition.replaceFirst( "@", "" ).trim();
  }

  /**
   * @see org.kalypsodeegree_impl.model.feature.xpath.AbstractXElement#evaluateOther(java.lang.Object, boolean)
   */
  @Override
  public Object evaluateOther( Object context, boolean featureTypeLevel )
  {
    // TODO Auto-generated method stub
    return null;
  }

  /**
   * @see org.kalypsodeegree_impl.model.feature.xpath.AbstractXElement#evaluateFeature(org.kalypsodeegree.model.feature.Feature)
   */
  @Override
  public Object evaluateFeature( Feature contextFE, boolean featureTypeLevel )
  {
    if( featureTypeLevel )
    {
      if( "fid".equals( m_attribute ) )
        return contextFE.getId();
      if( "id".equals( m_attribute ) )
        return contextFE.getId();
    }
    // TODO once units of measure are supported here, its here to implement it for xPath
    return "";
  }

}
