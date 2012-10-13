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
package org.kalypso.model.wspm.tuhh.core.wspwin.prf;

import java.io.IOException;
import java.io.Writer;

import org.apache.commons.lang3.StringUtils;
import org.kalypso.model.wspm.core.profil.IProfile;
import org.kalypso.model.wspm.core.profil.serializer.IProfileSink;

/**
 * @author kimwerner
 */
public class PrfSink implements IProfileSink
{
  // depends on Tuhh 1D calculation, ks or kst
  private final String m_defaultRoughnessType;

  private final boolean m_preferRoughnessClasses;

  private final boolean m_preferVegetationClasses;

  public PrfSink( )
  {
    this( StringUtils.EMPTY, false, false ); //$NON-NLS-1$
  }

  public PrfSink( final String defaultRoughnessType, final boolean preferRoughnessClasses, final boolean preferVegetationClasses )
  {
    m_defaultRoughnessType = defaultRoughnessType;
    m_preferRoughnessClasses = preferRoughnessClasses;
    m_preferVegetationClasses = preferVegetationClasses;
  }

  /**
   * FIXME: do not use IProfile[] profile, but only ever the first gets written....
   *
   * @see org.kalypso.model.wspm.core.profil.serializer.IProfilSink#write(org.kalypso.model.wspm.core.profil.IProfil[], java.io.Writer)
   */
  @Override
  public boolean write( final IProfile[] profiles, final Writer writer ) throws IOException
  {
    if( profiles == null || profiles.length < 1 )
      return false;

    final IProfile profil = profiles[0];

    final PrfWriter prfWriter = new PrfWriter( profil, new IWaterlevel[0], m_defaultRoughnessType, m_preferRoughnessClasses, m_preferVegetationClasses );
    prfWriter.write( writer );

    return true;
  }
}