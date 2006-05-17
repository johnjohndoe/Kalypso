/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and Coastal Engineering
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
package org.kalypso.observation;

import java.util.ArrayList;
import java.util.NoSuchElementException;

/**
 * Utilities around IObservation.
 * 
 * @author schlienger
 */
public final class ObservationUtilities
{
  private static final String MSG_ERROR_NOAXISTYPE = "Keine Achse gefunden vom Typ: ";

  private ObservationUtilities( )
  {
    // not intended to be instanciated
  }

//  /**
//   * Finds the axis of the given observation that has the given name.
//   * 
//   * @param axes
//   *          the list of axes to scan
//   * @param axisName
//   *          the name of the axis which is searched
//   * @return first axis found
//   * @throws NoSuchElementException
//   *           when no axis matches the name
//   */
//  public static IAxis findAxisByName( final IAxis[] axes, final String axisName ) throws NoSuchElementException
//  {
//    for( int i = 0; i < axes.length; i++ )
//    {
//      if( axes[i].getName().equalsIgnoreCase( axisName ) )
//        return axes[i];
//    }
//
//    throw new NoSuchElementException( MSG_ERROR_NOAXISTYPE + axisName );
//  }
//
//  /**
//   * Find an axis with the given name, if it could not be found, tries to find it by type. If it still could not be
//   * found, then a NoSuchElementException is thrown
//   * 
//   * @see ObservationUtilities#findAxisByName(IAxis[], String)
//   * @see ObservationUtilities#findAxisByType(IAxis[], String)
//   * @throws NoSuchElementException
//   *           when neither name nor type matched
//   */
//  public static IAxis findAxisByNameThenByType( final IAxis[] axes, final String axisNameOrType ) throws NoSuchElementException
//  {
//    try
//    {
//      return findAxisByName( axes, axisNameOrType );
//    }
//    catch( final NoSuchElementException ignored )
//    {
//      return findAxisByType( axes, axisNameOrType );
//    }
//  }
//
//  /**
//   * returns null when no axis found instead of throwing an exception
//   * 
//   * @return axis or null if not found
//   * @see ObservationUtilities#findAxisByName(IAxis[], String)
//   */
//  public static IAxis findAxisByNameNoEx( final IAxis[] axes, final String axisName )
//  {
//    try
//    {
//      return findAxisByName( axes, axisName );
//    }
//    catch( NoSuchElementException e )
//    {
//      return null;
//    }
//  }
//
//  /**
//   * Finds the axis of the given observation that has the given type.
//   * 
//   * @param axes
//   *          the list of axes to scan
//   * @param axisType
//   *          the type of the axis which is searched
//   * @return the first axis found
//   * @throws NoSuchElementException
//   *           when no axis matches the name
//   */
//  public static IAxis findAxisByType( final IAxis[] axes, final String axisType ) throws NoSuchElementException
//  {
//    for( int i = 0; i < axes.length; i++ )
//    {
//      if( axes[i].getType().equalsIgnoreCase( axisType ) )
//        return axes[i];
//    }
//
//    throw new NoSuchElementException( MSG_ERROR_NOAXISTYPE + axisType );
//  }
//
//  /**
//   * Return true if one of the axis is of the given type
//   */
//  public static boolean hasAxisOfType( final IAxis[] axes, final String axisType )
//  {
//    for( int i = 0; i < axes.length; i++ )
//    {
//      if( axes[i].getType().equalsIgnoreCase( axisType ) )
//        return true;
//    }
//
//    return false;
//  }
//
//  /**
//   * Returns the axes that are compatible with the desired Dataclass
//   * 
//   * @return all axes which are compatible with desired Classtype
//   */
//  public static IAxis[] findAxesByClass( final IAxis[] axes, final Class< ? > desired )
//  {
//    final ArrayList<IAxis> list = new ArrayList<IAxis>( axes == null ? 0 : axes.length );
//
//    for( int i = 0; i < axes.length; i++ )
//    {
//      if( desired.isAssignableFrom( axes[i].getDataClass() ) )
//        list.add( axes[i] );
//    }
//
//    if( list.size() == 0 )
//      throw new NoSuchElementException( MSG_ERROR_NOAXISTYPE + desired );
//
//    return list.toArray( new IAxis[list.size()] );
//  }
//
//  /**
//   * Returns the first axis that is compatible with the desired Dataclass
//   */
//  public static IAxis findAxisByClass( final IAxis[] axes, final Class< ? > desired )
//  {
//    for( int i = 0; i < axes.length; i++ )
//    {
//      if( desired.isAssignableFrom( axes[i].getDataClass() ) )
//        return axes[i];
//    }
//
//    throw new NoSuchElementException( MSG_ERROR_NOAXISTYPE + desired );
//  }
//
//  /**
//   * Returns the first axis that is compatible with the desired Dataclass, does not throw an exception if no such axis
//   * found. Returns null if no axis found.
//   */
//  public static IAxis findAxisByClassNoEx( final IAxis[] axes, final Class< ? > desired )
//  {
//    for( int i = 0; i < axes.length; i++ )
//    {
//      if( desired.isAssignableFrom( axes[i].getDataClass() ) )
//        return axes[i];
//    }
//
//    return null;
//  }
}