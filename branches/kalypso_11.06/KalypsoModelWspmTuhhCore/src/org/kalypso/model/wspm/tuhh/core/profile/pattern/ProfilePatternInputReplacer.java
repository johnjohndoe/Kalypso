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
package org.kalypso.model.wspm.tuhh.core.profile.pattern;

import java.util.Locale;
import java.util.regex.Matcher;

import org.kalypso.commons.pair.IKeyValue;
import org.kalypso.commons.pair.KeyValueFactory;
import org.kalypso.commons.patternreplace.IPatternInput;
import org.kalypso.commons.patternreplace.PatternInputReplacer;

/**
 * @author Gernot Belger
 */
public final class ProfilePatternInputReplacer extends PatternInputReplacer<IProfilePatternData>
{
  private static ProfilePatternInputReplacer INSTANCE = new ProfilePatternInputReplacer( Locale.getDefault() );

  public static ProfilePatternInputReplacer getINSTANCE( )
  {
    return INSTANCE;
  }

  public ProfilePatternInputReplacer( final Locale locale )
  {
    /* Needs profile */
    addReplacer( new ProfileNamePattern() );
    addReplacer( new ProfileDescriptionPattern() );
    addReplacer( new ProfileStationPattern() );
    addReplacer( new ProfileWspWinFilenamePattern() );
    addReplacer( new ProfileResultPattern() );

    /* Needs profile feature */
    addReplacer( new ProfileRiverNamePattern() );
    addReplacer( new ProfileRiverIdPattern() );

    /* Needs points */
    addReplacer( new PointComponentPattern( locale ) );
  }

  public IKeyValue<IPatternInput<IProfilePatternData>, String> getSinglePatternValue( final String pattern )
  {
    final Matcher matcher = createPatternMatcher( pattern );
    if( !matcher.matches() )
      return null;

    final IPatternInput<IProfilePatternData> tokenReplacer = getMatchedTokenReplacer( matcher );
    if( tokenReplacer == null )
      return null;

    final String params = getMatchedParameters( matcher );

    return KeyValueFactory.createPairEqualsBoth( tokenReplacer, params );
  }
}
