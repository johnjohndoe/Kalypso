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
package org.kalypso.model.wspm.core.imports;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.StringTokenizer;

import org.apache.commons.io.IOUtils;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.KalypsoModelWspmCoreExtensions;
import org.kalypso.model.wspm.core.i18n.Messages;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilPointPropertyProvider;
import org.kalypso.model.wspm.core.profil.ProfilFactory;
import org.kalypso.model.wspm.core.profil.util.ProfilUtil;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.Point;

/**
 * small refactoring Lanu and nofdp uses this helper!
 * 
 * @author Dirk Kuch
 */
public class ImportTrippleHelper
{
  /**
   * imports the profile trippel data and converts it into IProfils
   * 
   * @param trippleFile
   *          file with profile tripples
   */
  public static List<IProfil> importTrippelData( final File trippleFile, final String separator, final String profileType )
  {
    final IProfilPointPropertyProvider provider = KalypsoModelWspmCoreExtensions.getPointPropertyProviders( profileType );

    final IComponent rechtswert = provider.getPointProperty( IWspmConstants.POINT_PROPERTY_RECHTSWERT );
    final IComponent hochwert = provider.getPointProperty( IWspmConstants.POINT_PROPERTY_HOCHWERT );

    IProfil profile = ProfilFactory.createProfil( profileType );

    profile.addPointProperty( rechtswert );
    profile.addPointProperty( hochwert );

    if( trippleFile == null )
      return new ArrayList<IProfil>();

    /* read profiles, show warnings */
    final List<IProfil> profiles = new ArrayList<IProfil>();
    final List<IRecord> profilPointList = new ArrayList<IRecord>();

    /* file loading */
    BufferedReader fileReader = null;
    try
    {
      fileReader = new BufferedReader( new InputStreamReader( new FileInputStream( trippleFile ) ) );

      String line = null;
      StringTokenizer tokenizer;

      /* parameter */
      double station = 0;

      double currentStation = Double.NaN;
      int numStations = 0;

      /* File Header */
      fileReader.readLine();
      int count = 1;
      while( (line = fileReader.readLine()) != null )
      {
        count++;

        /* ignore empty lines */
        if( line.length() == 0 )
          continue;

        /* trippel-format should be: station, x, y, z */
        tokenizer = new StringTokenizer( line, separator );

        /* continue just if there are enough values in the trippel file */
        if( tokenizer.countTokens() == 4 )
        {

          /* first value = profile station */
          station = Double.parseDouble( tokenizer.nextToken() );

          if( station != currentStation )
          {
            if( !Double.isNaN( currentStation ) )
            {
              // store current profile points as IProfil
              ImportTrippleHelper.storeProfile( profile, profilPointList, currentStation, profiles );
              profilPointList.clear();
              numStations = numStations + 1;
            }

            profile = ProfilFactory.createProfil( profileType );

            profile.addPointProperty( rechtswert );
            profile.addPointProperty( hochwert );
// profile.addPointProperty( hoehe );
// profile.addPointProperty( breite );

            // update profile station
            currentStation = station;
          }

          final IRecord point = profile.createProfilPoint();
          if( ImportTrippleHelper.createProfilePoint( point, profilPointList, tokenizer ) )
            profilPointList.add( point );
        }
        else
        {
          // inform the user that his profile has not enough values...
          final String message = Messages.getString( "org.kalypso.model.wspm.core.imports.ImportTrippleHelper.0" ,(numStations + 1),count ); //$NON-NLS-1$
          throw new Exception( message );
          // continue;
        }
      }

      fileReader.close();

      /* store the last profile */
      ImportTrippleHelper.storeProfile( profile, profilPointList, station, profiles );
      profilPointList.clear();
      numStations = numStations + 1;

    }
    catch( final Exception e )
    {
      e.printStackTrace();
      throw new IllegalStateException( e.getMessage() );
      // return new ArrayList<IProfil>();
    }
    finally
    {
      IOUtils.closeQuietly( fileReader );
    }

    return profiles;
  }

  /**
   * stores all gathered profile points in a new profile and adds it to the profile list
   * 
   * @param profilPointList
   *          the list of the profile points of one profile
   * @param station
   *          the current profile station
   * @param profiles
   *          the list of the already imported profiles
   */
  private static void storeProfile( final IProfil profile, final List<IRecord> profilPointList, final double station, final List<IProfil> profiles )
  {

    for( final IRecord point : profilPointList )
      profile.addPoint( point );

    profile.setStation( station );
    profile.setName( Messages.getString( "org.kalypso.model.wspm.core.imports.ImportTrippleHelper.1" ) ); //$NON-NLS-1$
    profile.setDescription( Messages.getString( "org.kalypso.model.wspm.core.imports.ImportTrippleHelper.2" ) ); //$NON-NLS-1$

    profiles.add( profile );
  }

  /**
   * creates a new profile point and adds it to the point list of the current profile
   * 
   * @param profilPointList
   *          point list of the current profile
   * @param tokenizer
   *          holds the point data (x, y, z)
   */
  private static boolean createProfilePoint( final IRecord point, final List<IRecord> profilPointList, final StringTokenizer tokenizer )
  {
    final double x;
    final double y;
    final double z;
    /* observation of profile */

    final HashMap<String, IComponent> components = ProfilUtil.getComponentsFromRecord( point );

    final IComponent cRechtswert = components.get( IWspmConstants.POINT_PROPERTY_RECHTSWERT );
    final IComponent cHochwert = components.get( IWspmConstants.POINT_PROPERTY_HOCHWERT );
    final IComponent cBreite = components.get( IWspmConstants.POINT_PROPERTY_BREITE );
    final IComponent cHoehe = components.get( IWspmConstants.POINT_PROPERTY_HOEHE );

    final TupleResult owner = point.getOwner();
    final int iRechtswert = owner.indexOf( cRechtswert );
    final int iHochwert = owner.indexOf( cHochwert );
    final int iBreite = owner.indexOf( cBreite );
    final int iHoehe = owner.indexOf( cHoehe );

    if( tokenizer.hasMoreElements() )
    {
      x = Double.parseDouble( tokenizer.nextToken() );

      point.setValue( iRechtswert, x );
    }
    else
      return false;

    if( tokenizer.hasMoreElements() )
    {
      y = Double.parseDouble( tokenizer.nextToken() );
      point.setValue( iHochwert, y );
    }
    else
      return false;

    if( tokenizer.hasMoreElements() )
    {
      z = Double.parseDouble( tokenizer.nextToken() );
      point.setValue( iHoehe, z );
    }
    else
      return false;

    final double width;
    /* calculate width coordinate */
    if( profilPointList.size() > 0 )
      width = ImportTrippleHelper.calculateSegmentLength( profilPointList, point );
    else
      width = 0;

    point.setValue( iBreite, width );

    return true;
  }

  /**
   * calculates the width coordinate by the segment length (2-dim distance of the profile points)
   * 
   * @param profilPointList
   *          point list of the current profile
   */
  private static double calculateSegmentLength( final List<IRecord> profilPointList, final IRecord profilPoint )
  {
    double distance = 0;
    final GeometryFactory factory = new GeometryFactory();

    /* get the segment length of the already imported profile points */
    final IRecord last = profilPointList.get( profilPointList.size() - 1 );
    distance = ProfilUtil.getDoubleValueFor( IWspmConstants.POINT_PROPERTY_BREITE, last );

    /* add the segment length of the segment defined by the last imported profile point and the new to add profile point */
    final Double x1 = ProfilUtil.getDoubleValueFor( IWspmConstants.POINT_PROPERTY_RECHTSWERT, last );
    final Double y1 = ProfilUtil.getDoubleValueFor( IWspmConstants.POINT_PROPERTY_HOCHWERT, last );
    final Double x2 = ProfilUtil.getDoubleValueFor( IWspmConstants.POINT_PROPERTY_RECHTSWERT, profilPoint );
    final Double y2 = ProfilUtil.getDoubleValueFor( IWspmConstants.POINT_PROPERTY_HOCHWERT, profilPoint );

    final Coordinate coordinates1 = new Coordinate( x1, y1 );
    final Coordinate coordinates2 = new Coordinate( x2, y2 );

    final Point point1 = factory.createPoint( coordinates1 );
    final Point point2 = factory.createPoint( coordinates2 );

    distance = distance + point1.distance( point2 );

    return distance;
  }
}
