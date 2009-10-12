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
package org.kalypso.model.wspm.tuhh.core.profile.importer;

import java.math.BigDecimal;
import java.net.URL;
import java.util.LinkedHashMap;
import java.util.Map;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.gmlschema.GMLSchemaException;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.KalypsoModelWspmCoreExtensions;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilPointMarker;
import org.kalypso.model.wspm.core.profil.IProfilPointPropertyProvider;
import org.kalypso.model.wspm.core.profil.util.ProfilUtil;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhWspmProject;
import org.kalypso.model.wspm.tuhh.core.i18n.Messages;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypso.transformation.GeoTransformer;
import org.kalypsodeegree.model.geometry.GM_Point;

/**
 * @author Gernot Belger
 */
public class TuhhProfileWProfContentHandler implements IWProfContentHandler, IWspmTuhhConstants
{
  private final Map<String, ProfileInCreation> m_profiles = new LinkedHashMap<String, ProfileInCreation>();

  private final TuhhWspmProject m_project;

  private final GeoTransformer m_transformer;

  public TuhhProfileWProfContentHandler( final TuhhWspmProject project, final String srs )
  {
    m_project = project;
    m_transformer = new GeoTransformer( srs );
  }

  public void finished( )
  {
    /* Write all changed profiles back to the features */
    for( final ProfileInCreation pic : m_profiles.values() )
    {
      pic.cleanupProfile();
      pic.finish();
    }
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.core.profile.importer.IWProfContentHandler#newPoint(java.lang.String,
   *      java.math.BigDecimal, java.lang.String, java.math.BigDecimal, org.kalypsodeegree.model.geometry.GM_Point,
   *      double, java.lang.String, java.lang.String, java.net.URL, java.lang.String, int, int, int)
   */
  @Override
  public void newPoint( final String riverId, final BigDecimal station, final String profileName, final BigDecimal distance, final GM_Point location, final double value, final String comment, final String profileComment, final URL photoURL, final String objectType, final int attributeType, final int ord, final int partOrd ) throws CoreException
  {
    final String componentId = getComponent( objectType, attributeType );
    final String markerID = getMarker( attributeType );

    try
    {
      final IProfil profil = findProfil( station, riverId, profileName, profileComment, photoURL );
      if( profil == null )
        throw new CoreException( StatusUtilities.createStatus( IStatus.ERROR, Messages.getString("org.kalypso.model.wspm.tuhh.core.profile.importer.TuhhProfileWProfContentHandler.0", station ), null ) ); //$NON-NLS-1$

      if( componentId != null && !componentId.isEmpty() )
      {
        final IProfilPointPropertyProvider provider = KalypsoModelWspmCoreExtensions.getPointPropertyProviders( profil.getType() );
        final IComponent component = provider.getPointProperty( componentId );

        profil.addPointProperty( component );
        final int compIndex = profil.indexOfProperty( componentId );

        final IRecord point = createOrFindPoint( distance, profil, componentId );
        point.setValue( compIndex, value );

        if( location != null )
        {
          final GM_Point transformedLocation = (GM_Point) m_transformer.transform( location );
          final int rwIndex = profil.indexOfProperty( POINT_PROPERTY_RECHTSWERT );
          final int hwIndex = profil.indexOfProperty( POINT_PROPERTY_HOCHWERT );

          point.setValue( rwIndex, transformedLocation.getX() );
          point.setValue( hwIndex, transformedLocation.getY() );
        }

        if( markerID != null && !markerID.isEmpty() )
        {
          final IProfilPointMarker marker = profil.createPointMarker( markerID, point );
          final Object defaultValue = provider.getDefaultValue( markerID );
          marker.setValue( defaultValue );
        }

        if( comment != null && !comment.isEmpty() )
        {
          if( profil.hasPointProperty( POINT_PROPERTY_COMMENT ) == null )
          {
            final IComponent pointProperty = provider.getPointProperty( POINT_PROPERTY_COMMENT );
            profil.addPointProperty( pointProperty );
          }
          final int commentIndex = profil.indexOfProperty( POINT_PROPERTY_COMMENT );

          final String existingComment = (String) point.getValue( commentIndex );
          final String commentToSet = existingComment == null ? comment : existingComment + " - " + comment; //$NON-NLS-1$
          point.setValue( commentIndex, commentToSet );
        }
      }
    }
    catch( final GMLSchemaException e )
    {
      e.printStackTrace();

      throw new CoreException( StatusUtilities.createStatus( IStatus.ERROR, Messages.getString("org.kalypso.model.wspm.tuhh.core.profile.importer.TuhhProfileWProfContentHandler.2"), e ) ); //$NON-NLS-1$
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      throw new CoreException( StatusUtilities.createStatus( IStatus.ERROR, Messages.getString("org.kalypso.model.wspm.tuhh.core.profile.importer.TuhhProfileWProfContentHandler.3"), e ) ); //$NON-NLS-1$
    }

  }

  private IRecord createOrFindPoint( final BigDecimal distance, final IProfil profil, final String componentId )
  {
    if( componentId == POINT_PROPERTY_HOEHE )
    {
      final int indexOfDistance = profil.indexOfProperty( IWspmConstants.POINT_PROPERTY_BREITE );

      // H�he values always get added as new points; we assume that the points are in the right order
      // This is necessary the preserve 'R�ckspr�nge' in the soil-layer
      final IRecord newPoint = profil.createProfilPoint();
      newPoint.setValue( indexOfDistance, new Double( distance.doubleValue() ) );
      profil.addPoint( newPoint );
      return newPoint;
    }

    return ProfilUtil.findOrInsertPointAt( profil, distance );
  }

  private IProfil findProfil( final BigDecimal station, final String riverId, final String name, final String comment, final URL photoURL ) throws GMLSchemaException
  {
    final String key = String.format( "%s - %s - %s", riverId, station, name ); //$NON-NLS-1$

    final ProfileInCreation pic = m_profiles.get( key );
    if( pic != null )
      return pic.getProfil();

    final IProfileFeature profileFeature = m_project.createNewProfile( riverId, true );
    profileFeature.setSrsName( m_transformer.getTarget() );
    profileFeature.setImage( photoURL );

    final ProfileInCreation newPic = new ProfileInCreation( profileFeature );

    m_profiles.put( key, newPic );

    final IProfil newProfile = newPic.getProfil();
    newProfile.setName( name );
    newProfile.setComment( comment );
    newProfile.setStation( station.doubleValue() );
    final IProfilPointPropertyProvider provider = KalypsoModelWspmCoreExtensions.getPointPropertyProviders( newProfile.getType() );
    newProfile.addPointProperty( provider.getPointProperty( POINT_PROPERTY_RECHTSWERT ) );
    newProfile.addPointProperty( provider.getPointProperty( POINT_PROPERTY_HOCHWERT ) );

    System.out.println(  Messages.getString("org.kalypso.model.wspm.tuhh.core.profile.importer.TuhhProfileWProfContentHandler.5", riverId, station ) ); //$NON-NLS-1$

    return newProfile;
  }

  private String getComponent( final String objectType, final int type )
  {
    if( "V01".equals( objectType ) ) //$NON-NLS-1$
      return POINT_PROPERTY_HOEHE;
    if( "21".equals( objectType ) ) //$NON-NLS-1$
      return POINT_PROPERTY_HOEHE;

    /* Verdohlung */
    if( objectType.startsWith( "D" ) ) //$NON-NLS-1$
    {
      switch( type )
      {
        case 41:
          return POINT_PROPERTY_OBERKANTEBRUECKE;

        case 42: // Oberkante Gel�nder
          return null;

        default:
          return POINT_PROPERTY_HOEHE;
      }
    }

    if( "V02".equals( objectType ) ) //$NON-NLS-1$
      return POINT_PROPERTY_UNTERKANTEBRUECKE;
    if( "V03".equals( objectType ) ) //$NON-NLS-1$
      return POINT_PROPERTY_OBERKANTEBRUECKE;

    return null;
  }

  private String getMarker( final int type )
  {
    switch( type )
    {
      case 1:
      case 3:
      case 10:
      case 12:
        return MARKER_TYP_BORDVOLL;
      case 2:
      case 5:
      case 8:
      case 11:
        return MARKER_TYP_TRENNFLAECHE;

      default:
        return null;
    }
  }

}
