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
package org.kalypso.transformation;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.deegree.model.crs.CoordinateSystem;
import org.deegree.model.crs.UnknownCRSException;
import org.eclipse.core.runtime.Preferences;
import org.kalypso.preferences.IKalypsoDeegreePreferences;
import org.kalypsodeegree.KalypsoDeegreePlugin;

/**
 * This class is a helper for dealing with coordinate systems.
 * 
 * @author Holger Albert
 */
public class CRSHelper
{
  /**
   * The constructor.
   */
  private CRSHelper( )
  {
  }

  /**
   * This function returns true, if the given name is one of the known coordinate systems.
   * 
   * @param name
   *            The name of the coordinate system to check.
   * @return True, if the coordinate system exists.
   */
  public static boolean isKnownCRS( String name )
  {
    if( name == null )
      return false;

    try
    {
      /* In case it is asked often, it is better to used the cached crs factory. */
      CachedCRSFactory factory = CachedCRSFactory.getInstance();

      CoordinateSystem crs = factory.create( name );
      if( crs == null )
        return false;

      return true;
    }
    catch( UnknownCRSException e )
    {
      return false;
    }
  }

  /**
   * This function returns all known names of coordinate systems.
   * 
   * @return A list of all known names of coordinate systems.
   */
  public static List<String> getAllNames( )
  {
    Preferences preferences = KalypsoDeegreePlugin.getDefault().getPluginPreferences();
    String preferenceNames = preferences.getString( IKalypsoDeegreePreferences.AVAILABLE_CRS_SETTING );

    String[] availableNames = preferenceNames.split( ";" );

    return Arrays.asList( availableNames );
  }

  /**
   * This function returns the dimension of the coordinate system with the given name.
   * 
   * @return The dimension of the coordinate system.
   */
  public static int getDimension( String name ) throws UnknownCRSException
  {
    CoordinateSystem coordinateSystem = CachedCRSFactory.getInstance().create( name );

    return coordinateSystem.getDimension();
  }

  /**
   * This function returns a list of coordinate systems with the given names. <br>
   * <br>
   * ATTENTION: Outside of this plugin, only the names should be used.
   * 
   * @param names
   *            The list of names.
   * @return The list of coordinate systems.
   */
  public static List<CoordinateSystem> getCRSListByNames( List<String> names ) throws UnknownCRSException
  {
    /* Memory for the coordinate systems. */
    ArrayList<CoordinateSystem> coordinateSystems = new ArrayList<CoordinateSystem>();

    for( int i = 0; i < names.size(); i++ )
    {
      CoordinateSystem coordinateSystem = CachedCRSFactory.getInstance().create( names.get( i ) );
      coordinateSystems.add( coordinateSystem );
    }

    return coordinateSystems;
  }
}