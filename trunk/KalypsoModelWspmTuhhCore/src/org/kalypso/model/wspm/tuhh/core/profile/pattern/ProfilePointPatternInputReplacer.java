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
package org.kalypso.model.wspm.tuhh.core.profile.pattern;

import java.util.Map.Entry;

import org.kalypso.commons.patternreplace.PatternInputReplacer;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.observation.result.IRecord;

/**
 * @author Gernot Belger
 */
public class ProfilePointPatternInputReplacer extends PatternInputReplacer<Entry<IProfil, IRecord>>
{
  private static ProfilePointPatternInputReplacer INSTANCE = new ProfilePointPatternInputReplacer();

  public static ProfilePointPatternInputReplacer getINSTANCE( )
  {
    return INSTANCE;
  }

  /**
   * @see org.kalypso.commons.patternreplace.PatternInputReplacer#replaceTokens(java.lang.String, java.lang.Object)
   */
  @Override
  public String replaceTokens( final String pattern, final Entry<IProfil, IRecord> context )
  {
    final IRecord point = context.getValue();
    // Fallback to profile pattern replacement if context has no profile point
    if( point == null )
    {
      final IProfil profile = context.getKey();
      return ProfilePatternInputReplacer.getINSTANCE().replaceTokens( pattern, profile );
    }

    return super.replaceTokens( pattern, context );
  }
}
