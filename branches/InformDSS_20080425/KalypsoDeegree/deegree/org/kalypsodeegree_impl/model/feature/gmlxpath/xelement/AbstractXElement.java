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
import org.kalypsodeegree_impl.model.feature.gmlxpath.GMLXPathException;

/**
 * abstract Class intended to be subclassed by implementors of XElements
 * 
 * @author doemming
 */
public abstract class AbstractXElement implements IXElement
{
  /**
   * @see org.kalypsodeegree_impl.model.feature.xpath.IXElement#evaluate(java.lang.Object)
   */
  public Object evaluate( Object context, boolean featureTypeLevel ) throws GMLXPathException
  {
    if( context instanceof Feature )
      return evaluateFeature( (Feature) context, featureTypeLevel );
    return evaluateOther( context, featureTypeLevel );
  }

  public abstract Object evaluateOther( Object context, boolean featureTypeLevel ) throws GMLXPathException;

  public abstract Object evaluateFeature( Feature contextFeature, boolean featureTypeLevel ) throws GMLXPathException;
}
