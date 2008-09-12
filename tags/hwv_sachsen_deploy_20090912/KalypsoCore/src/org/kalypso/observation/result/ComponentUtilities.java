/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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

import javax.xml.namespace.QName;

import org.kalypso.gmlschema.property.restriction.EnumerationRestriction;
import org.kalypso.gmlschema.property.restriction.IRestriction;

/**
 * Utility methods for components
 * 
 * @author schlienger
 */
public final class ComponentUtilities
{
  private ComponentUtilities( )
  {
    // utility class
  }

  /**
   * @return the first component of the given type if found, else null
   */
  public static IComponent findComponent( final IComponent[] comps, final QName typeName )
  {
    for( final IComponent element : comps )
    {
      if( element.getValueTypeName().equals( typeName ) )
        return element;
    }

    return null;
  }

  /**
   * @return the first component found that is not of the given type
   */
  public static IComponent otherComponent( final IComponent[] comps, final QName typeName )
  {
    for( int i = 0; i < comps.length; i++ )
    {
      if( !comps[i].getValueTypeName().equals( typeName ) )
        return comps[i];
    }

    return null;
  }

  /**
   * @return the first component found that equals the given one
   */
  public static IComponent sameComponent( final IComponent[] comps, final IComponent comp )
  {
    for( final IComponent element : comps )
    {
      if( element.equals( comp ) )
        return element;
    }

    return null;
  }

  /**
   * TODO: move into helper class of restrictions!
   * 
   * @author kuch
   */
  static public boolean restrictionContainsEnumeration( final IRestriction[] restrictions )
  {
    for( final IRestriction restriction : restrictions )
    {
      if( restriction instanceof EnumerationRestriction )
        return true;
    }

    return false;
  }

  /**
   * Searches for a component by id.
   * 
   * @return <code>null</code>, if no compoentn with the given id is found, else the first found component.
   */
  public static IComponent findComponentByID( final IComponent[] components, final String componentID )
  {
    for( final IComponent component : components )
    {
      if( componentID.equals( component.getId() ) )
        return component;
    }

    return null;
  }
}
