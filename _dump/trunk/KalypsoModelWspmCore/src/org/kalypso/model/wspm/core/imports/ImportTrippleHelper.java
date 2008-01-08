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
import java.util.List;
import java.util.StringTokenizer;

import org.apache.commons.io.IOUtils;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilPoint;
import org.kalypso.model.wspm.core.profil.ProfilFactory;
import org.kalypso.model.wspm.core.profil.impl.points.ProfilPoint;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.Point;

/**
 * small refactoring Lanu and nofdp uses this helper!
 * 
 * @author kuch
 */
public class ImportTrippleHelper
{
  /**
   * imports the profile trippel data and converts it into IProfils
   * 
   * @param trippleFile
   *            file with profile tripples
   */
  public static List<IProfil> importTrippelData( final File trippleFile, final String separator, final String profileType )
  {
    if( trippleFile == null )
      return new ArrayList<IProfil>();

    /* read profiles, show warnings */
    final List<IProfil> profiles = new ArrayList<IProfil>();
    final List<IProfilPoint> profilPointList = new ArrayList<IProfilPoint>();

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
              ImportTrippleHelper.storeProfile( profilPointList, currentStation, profiles, profileType );
              profilPointList.clear();
              numStations = numStations + 1;
            }

            // update profile station
            currentStation = station;
          }

          final IProfilPoint point = ImportTrippleHelper.createProfilePoint( profilPointList, tokenizer );
          if( point != null )
            profilPointList.add( point );

        }
        else
        {
          // inform the user that his profile has not enough values...
          final String message = "Import failed; profile no: \n" + (numStations + 1) + "(line no. " + count + ") has wrong number of values;\n necessary are: station, x, y, z";
          throw new Exception( message );
          // continue;
        }

      }

      fileReader.close();

      /* store the last profile */
      ImportTrippleHelper.storeProfile( profilPointList, station, profiles, profileType );
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
   *            the list of the profile points of one profile
   * @param station
   *            the current profile station
   * @param profiles
   *            the list of the already imported profiles
   */
  private static void storeProfile( final List<IProfilPoint> profilPointList, final double station, final List<IProfil> profiles, final String profiletype )
  {
    final IProfil profile = ProfilFactory.createProfil( profiletype );

    profile.addPointProperty( IWspmConstants.POINT_PROPERTY_BREITE );
    profile.addPointProperty( IWspmConstants.POINT_PROPERTY_HOEHE );
    profile.addPointProperty( IWspmConstants.POINT_PROPERTY_HOCHWERT );
    profile.addPointProperty( IWspmConstants.POINT_PROPERTY_RECHTSWERT );

    for( final IProfilPoint point : profilPointList )
      profile.addPoint( point );

    profile.setStation( station );
    profile.setName( "Trippel-Import" );

    profiles.add( profile );
  }

  /**
   * creates a new profile point and adds it to the point list of the current profile
   * 
   * @param profilPointList
   *            point list of the current profile
   * @param tokenizer
   *            holds the point data (x, y, z)
   */
  private static IProfilPoint createProfilePoint( final List<IProfilPoint> profilPointList, final StringTokenizer tokenizer )
  {
    double x;
    double y;
    double z;

    final ProfilPoint profilPoint = new ProfilPoint();
    profilPoint.addProperty( IWspmConstants.POINT_PROPERTY_RECHTSWERT );
    profilPoint.addProperty( IWspmConstants.POINT_PROPERTY_HOCHWERT );
    profilPoint.addProperty( IWspmConstants.POINT_PROPERTY_HOEHE );
    profilPoint.addProperty( IWspmConstants.POINT_PROPERTY_BREITE );

    if( tokenizer.hasMoreElements() )
    {
      x = Double.parseDouble( tokenizer.nextToken() );
      profilPoint.setValueFor( IWspmConstants.POINT_PROPERTY_RECHTSWERT, x );
    }
    else
      return null;

    if( tokenizer.hasMoreElements() )
    {
      y = Double.parseDouble( tokenizer.nextToken() );
      profilPoint.setValueFor( IWspmConstants.POINT_PROPERTY_HOCHWERT, y );
    }
    else
      return null;

    if( tokenizer.hasMoreElements() )
    {
      z = Double.parseDouble( tokenizer.nextToken() );
      profilPoint.setValueFor( IWspmConstants.POINT_PROPERTY_HOEHE, z );
    }
    else
      return null;

    final double width;
    /* calculate width coordinate */
    if( profilPointList.size() > 0 )
      width = ImportTrippleHelper.calculateSegmentLength( profilPointList, profilPoint );
    else
      width = 0;

    profilPoint.setValueFor( IWspmConstants.POINT_PROPERTY_BREITE, width );

    return profilPoint;
  }

  /**
   * calculates the width coordinate by the segment length (2-dim distance of the profile points)
   * 
   * @param profilPointList
   *            point list of the current profile
   */
  private static double calculateSegmentLength( final List<IProfilPoint> profilPointList, final ProfilPoint profilPoint )
  {
    double distance = 0;
    final GeometryFactory factory = new GeometryFactory();

    /* get the segment length of the already imported profile points */
    distance = profilPointList.get( profilPointList.size() - 1 ).getValueFor( IWspmConstants.POINT_PROPERTY_BREITE );

    /* add the segment length of the segment defined by the last imported profile point and the new to add profile point */
    final double x1 = profilPointList.get( profilPointList.size() - 1 ).getValueFor( IWspmConstants.POINT_PROPERTY_RECHTSWERT );
    final double y1 = profilPointList.get( profilPointList.size() - 1 ).getValueFor( IWspmConstants.POINT_PROPERTY_HOCHWERT );
    final double x2 = profilPoint.getValueFor( IWspmConstants.POINT_PROPERTY_RECHTSWERT );
    final double y2 = profilPoint.getValueFor( IWspmConstants.POINT_PROPERTY_HOCHWERT );

    final Coordinate coordinates1 = new Coordinate( x1, y1 );
    final Coordinate coordinates2 = new Coordinate( x2, y2 );

    final Point point1 = factory.createPoint( coordinates1 );
    final Point point2 = factory.createPoint( coordinates2 );

    distance = distance + point1.distance( point2 );

    return distance;
  }
}
