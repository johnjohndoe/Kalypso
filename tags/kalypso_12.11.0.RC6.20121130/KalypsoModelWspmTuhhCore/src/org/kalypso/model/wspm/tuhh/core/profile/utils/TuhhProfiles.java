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
package org.kalypso.model.wspm.tuhh.core.profile.utils;

import org.kalypso.commons.java.lang.Objects;
import org.kalypso.model.wspm.core.IWspmPointProperties;
import org.kalypso.model.wspm.core.profil.IProfile;
import org.kalypso.model.wspm.tuhh.core.profile.TuhhProfil;
import org.kalypso.observation.result.TupleResultUtilities;

/**
 * @author Dirk Kuch
 */
public final class TuhhProfiles
{
  private TuhhProfiles( )
  {
  }

  public static boolean hasRoughness( final IProfile profile )
  {
    if( Objects.isNotNull( profile.hasPointProperty( IWspmPointProperties.POINT_PROPERTY_RAUHEIT_KS ) ) )
      return true;
    else if( Objects.isNotNull( profile.hasPointProperty( IWspmPointProperties.POINT_PROPERTY_RAUHEIT_KST ) ) )
      return true;
    else if( Objects.isNotNull( profile.hasPointProperty( IWspmPointProperties.POINT_PROPERTY_ROUGHNESS_CLASS ) ) )
      return true;

    return false;
  }

  public static boolean hasVegetation( final IProfile profile )
  {
    if( Objects.isNotNull( profile.hasPointProperty( IWspmPointProperties.POINT_PROPERTY_BEWUCHS_AX ) ) )
      return true;
    else if( Objects.isNotNull( profile.hasPointProperty( IWspmPointProperties.POINT_PROPERTY_BEWUCHS_CLASS ) ) )
      return true;

    return false;
  }

  // FIXME: this is a very specific clone -> comment; careful how to use this!
  public static IProfile clone( final IProfile profile )
  {
    if( TuhhProfiles.class.equals( profile.getClass() ) )
      throw new IllegalStateException( String.format( "Cloning of profile class %s not supported", profile.getClass().getName() ) ); //$NON-NLS-1$

    final TuhhProfil clone = new TuhhProfil( null );

    TupleResultUtilities.copyValues( profile.getResult(), clone.getResult() );

    clone.setName( profile.getName() );
    clone.setDescription( profile.getDescription() );
    clone.setComment( profile.getComment() );
    clone.setStation( profile.getStation() );
    clone.setSrsName( profile.getSrsName() );

    return clone;
  }
}
