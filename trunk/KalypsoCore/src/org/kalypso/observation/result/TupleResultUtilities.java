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
package org.kalypso.observation.result;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Map;

import javax.xml.namespace.QName;

import org.kalypso.commons.xml.XmlTypes;

/**
 * TODO: Merge most of the stuff with {@link ComponentUtilities}.
 * 
 * @author Gernot Belger
 */
public class TupleResultUtilities
{
  private TupleResultUtilities( )
  {
    // helper class, do not instantiate
  }

  /**
   * Get component by name.
   * 
   * @return null, if no component with the given name was found.
   */
  public static IComponent findComponentByName( final TupleResult result, final String name )
  {
    final IComponent[] components = result.getComponents();
    for( final IComponent comp : components )
    {
      if( comp.getName().equals( name ) )
      {
        return comp;
      }
    }

    return null;
  }

  /**
   * Find component by id.
   * 
   * @return The first component with the given id, null, none was found.
   */
  public static IComponent findComponentById( final TupleResult result, final String id )
  {
    final IComponent[] components = result.getComponents();
    for( final IComponent comp : components )
    {
      if( comp.getId().equals( id ) )
      {
        return comp;
      }
    }

    return null;
  }

  /**
   * @author thuel2
   * @return returns minimum value for component of a tupleResult.<br>
   *         Works for components of XmlType XS_BOOLEAN, XS_DOUBLE, XS_DATE, XS_STRING. <br>
   *         For all others <code>object.toString</code> will be used for comparison.
   */
  public static Object findComponentMinById( final TupleResult result, final String compID )
  {
    final IComponent comp = TupleResultUtilities.findComponentById( result, compID );
    if( comp == null )
    {
      return null;
    }
    final QName valueTypeName = comp.getValueTypeName();

    if( XmlTypes.XS_BOOLEAN.equals( valueTypeName ) )
    {
      final List<Boolean> values = new ArrayList<Boolean>();
      for( final IRecord record : result )
      {
        values.add( (Boolean) record.getValue( comp ) );
      }
      if( values.size() < 1 )
      {
        return null;
      }
      return Collections.min( values );
    }
    else if( XmlTypes.XS_DOUBLE.equals( valueTypeName ) )
    {
      // TODO think about other numerical types:
      // XmlTypes.XS_BYTE, XmlTypes.XS_DECIMAL, XmlTypes.XS_FLOAT, XmlTypes.XS_INT, XmlTypes.XS_INTEGER,
      // XmlTypes.XS_LONG, XmlTypes.XS_SHORT
      final List<java.lang.Double> values = new ArrayList<java.lang.Double>();
      for( final IRecord record : result )
      {
        values.add( (java.lang.Double) record.getValue( comp ) );
      }
      if( values.size() < 1 )
      {
        return null;
      }
      return Collections.min( values );
    }
    else if( XmlTypes.XS_DATE.equals( valueTypeName ) )
    {
      // TODO think about other date types
      // XmlTypes.XS_DATETIME, XmlTypes.XS_DURATION, XmlTypes.XS_TIME
      final List<Date> values = new ArrayList<Date>();
      for( final IRecord record : result )
      {
        values.add( (Date) record.getValue( comp ) );
      }
      if( values.size() < 1 )
      {
        return null;
      }
      return Collections.min( values );
    }
    else if( XmlTypes.XS_STRING.equals( valueTypeName ) )
    {
      final List<String> values = new ArrayList<String>();
      for( final IRecord record : result )
      {
        values.add( (String) record.getValue( comp ) );
      }
      if( values.size() < 1 )
      {
        return null;
      }
      return Collections.min( values );
    }
    else
    {
      final List<String> values = new ArrayList<String>();
      for( final IRecord record : result )
      {
        values.add( (record.getValue( comp )).toString() );
      }
      if( values.size() < 1 )
      {
        return null;
      }
      return Collections.min( values );
    }
  }

  /**
   * @author thuel2
   * @return returns maximum value for component of a tupleResult. <br>
   *         Works for components of XmlType XS_BOOLEAN, XS_DOUBLE, XS_DATE, XS_STRING. <br>
   *         For all others <code>object.toString()</code> will be used for comparison.
   */
  public static Object findComponentMaxById( final TupleResult result, final String compID )
  {
    final IComponent comp = TupleResultUtilities.findComponentById( result, compID );
    if( comp == null )
    {
      return null;
    }
    final QName valueTypeName = comp.getValueTypeName();

    if( XmlTypes.XS_BOOLEAN.equals( valueTypeName ) )
    {
      final List<Boolean> values = new ArrayList<Boolean>();
      for( final IRecord record : result )
      {
        values.add( (Boolean) record.getValue( comp ) );
      }
      if( values.size() < 1 )
      {
        return null;
      }
      return Collections.max( values );
    }
    else if( XmlTypes.XS_DOUBLE.equals( valueTypeName ) )
    {
      // TODO think about other numerical types:
      // XmlTypes.XS_BYTE, XmlTypes.XS_DECIMAL, XmlTypes.XS_FLOAT, XmlTypes.XS_INT, XmlTypes.XS_INTEGER,
      // XmlTypes.XS_LONG, XmlTypes.XS_SHORT
      final List<java.lang.Double> values = new ArrayList<java.lang.Double>();
      for( final IRecord record : result )
      {
        values.add( (java.lang.Double) record.getValue( comp ) );
      }
      if( values.size() < 1 )
      {
        return null;
      }
      return Collections.max( values );
    }
    else if( XmlTypes.XS_DATE.equals( valueTypeName ) )
    {
      // TODO think about other date types
      // XmlTypes.XS_DATETIME, XmlTypes.XS_DURATION, XmlTypes.XS_TIME
      final List<Date> values = new ArrayList<Date>();
      for( final IRecord record : result )
      {
        values.add( (Date) record.getValue( comp ) );
      }
      if( values.size() < 1 )
      {
        return null;
      }
      return Collections.max( values );
    }
    else if( XmlTypes.XS_STRING.equals( valueTypeName ) )
    {
      final List<String> values = new ArrayList<String>();
      for( final IRecord record : result )
      {
        values.add( (String) record.getValue( comp ) );
      }
      if( values.size() < 1 )
      {
        return null;
      }
      return Collections.max( values );
    }
    else
    {
      final List<String> values = new ArrayList<String>();
      for( final IRecord record : result )
      {
        values.add( (record.getValue( comp )).toString() );
      }
      if( values.size() < 1 )
      {
        return null;
      }
      return Collections.max( values );
    }
  }

  /**
   * Copies records from one {@link TupleResult} to another.
   * 
   * @param componentMap
   *            Map of component ids.
   * @throws IllegalArgumentException
   *             If for an id from the map no component is found.
   */
  public static void copyValues( final TupleResult sourceResult, final TupleResult targetResult, final Map<String, String> componentMap )
  {
    /* Find Components */
    final IComponent[] sourceComponents = new IComponent[componentMap.size()];
    final IComponent[] targetComponents = new IComponent[componentMap.size()];

    int count = 0;
    for( final Map.Entry<String, String> entry : componentMap.entrySet() )
    {
      final String sourceID = entry.getKey();
      final String targetID = entry.getValue();

      final IComponent sourceComponent = ComponentUtilities.findComponentByID( sourceResult.getComponents(), sourceID );
      if( sourceComponent == null )
        throw new IllegalArgumentException( "Source component not found: " + sourceID );

      final IComponent targetComponent = ComponentUtilities.findComponentByID( targetResult.getComponents(), targetID );
      if( targetComponent == null )
        throw new IllegalArgumentException( "Source component not found: " + targetID );

      sourceComponents[count] = sourceComponent;
      targetComponents[count] = targetComponent;
      count++;
    }

    /* Copy values */
    for( final IRecord sourceRecord : sourceResult )
    {
      final IRecord targetRecord = targetResult.createRecord();

      for( int i = 0; i < sourceComponents.length; i++ )
      {
        final Object value = sourceRecord.getValue( sourceComponents[i] );
        targetRecord.setValue( targetComponents[i], value );
      }

      targetResult.add( targetRecord );
    }
  }
}
