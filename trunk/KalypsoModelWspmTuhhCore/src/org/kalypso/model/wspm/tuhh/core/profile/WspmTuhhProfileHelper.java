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
package org.kalypso.model.wspm.tuhh.core.profile;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InvalidObjectException;

import org.kalypso.model.wspm.core.profil.IProfile;
import org.kalypso.model.wspm.core.profil.serializer.ProfileSerializerUtilitites;
import org.kalypso.model.wspm.tuhh.core.wspwin.prf.PrfSink;
import org.kalypso.model.wspm.tuhh.core.wspwin.prf.PrfSource;

/**
 * @author kimwerner
 */
public final class WspmTuhhProfileHelper
{
  private WspmTuhhProfileHelper( )
  {
    // Helperclass, do not instantiate
  }

  /**
   * Highly dubious, cloning the profile via serialization to/from prf format -> data loss is certain, do not use.
   */
  @Deprecated
  public static IProfile copyProfile( final IProfile profile ) throws InvalidObjectException
  {
    final ByteArrayOutputStream out = new ByteArrayOutputStream();
    try
    {
      ProfileSerializerUtilitites.writeProfile( new PrfSink(), profile, new BufferedOutputStream( out ) );
      final ByteArrayInputStream in = new ByteArrayInputStream( out.toByteArray() );
      return ProfileSerializerUtilitites.readProfile( new PrfSource(), new BufferedInputStream( in ), profile.getType() );
    }
    catch( final IOException e )
    {
      throw new InvalidObjectException( e.getLocalizedMessage() );
    }
  }
}