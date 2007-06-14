/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
package org.kalypso.observation.result;

import org.kalypso.ogc.gml.om.ObservationFeatureFactory;
import org.kalypsodeegree.model.typeHandler.XsdBaseTypeHandler;

/**
 * @author kuch
 */
public abstract class AbstractComponent implements IComponent
{

  /**
   * @see java.util.Comparator#compare(java.lang.Object, java.lang.Object)
   */
  public int compare( final Object objFirst, final Object objSecond )
  {
    final XsdBaseTypeHandler handler = ObservationFeatureFactory.typeHanderForComponent( this );

    if( handler == null )
    {
      // TODO what to do now??
      return 0;
    }

    if( (objFirst == null) && (objSecond == null) )
    {
      return 0; // equals
    }
    else if( objFirst == null )
    {
      return -1; // lesser
    }
    else if( objSecond == null )
    {
      return 1; // greater
    }

    return handler.compare( objFirst, objSecond );
  }

}
