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

import javax.xml.namespace.QName;

import org.kalypso.commons.xml.XmlTypes;
import org.kalypso.ogc.gml.om.ObservationFeatureFactory;
import org.kalypso.ogc.swe.RepresentationType.KIND;

/**
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

  public static void sortRecordList( final List<IRecord> list, final IComponent[] sortColumns )
  {
    /*******************************************************************************************************************
     * Bubblesort algorithm
     ******************************************************************************************************************/
    boolean bSorted = false;

    while( bSorted == false )
    {
      bSorted = true;

      for( int i = 1; i < list.size(); i++ )
      {
        /* values to compare */
        final Object valMinus = list.get( i - 1 ).getValue( sortColumns[0] );
        final Object val = list.get( i ).getValue( sortColumns[0] );
        final IComponent component = sortColumns[0];
        final KIND kind = ObservationFeatureFactory.createRepresentationType( component ).getKind();

        // -1 -> valMinus > val,
        // 1 -> valMinus < val,
        // 0 -> valMinus == val
        int result;

        if( kind.equals( KIND.Boolean ) )
        {
          // howto sort boolean=?!?
          return;
        }
        else if( kind.equals( KIND.Number ) )
        {
          result = ((Double) valMinus).compareTo( (Double) val );
        }
        else if( kind.equals( KIND.Word ) )
        {

          result = ((String) valMinus).compareTo( (String) val );
        }
        else if( kind.equals( KIND.SimpleType ) )
        {
          // simpleType can be any object - howto sort?
          return;
        }
        else
        {
          return;
        }

        /* valMinus > val */
        if( result == -1 )
        {
          /* switch elements and set list dirty */
          final IRecord tmp = list.get( i - 1 );
          list.set( i - 1, list.get( i ) );
          list.set( i, tmp );

          bSorted = false;
        }
      }
    }
  }

}
