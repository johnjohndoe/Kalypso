package org.kalypso.model.wspm.tuhh.core.profile.pattern;

import org.apache.commons.lang.StringUtils;
import org.kalypso.commons.patternreplace.AbstractPatternInput;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.tuhh.core.i18n.Messages;

/**
 * @author Gernot Belger
 */
public final class ProfileStationPattern extends AbstractPatternInput<IProfilePatternData>
{
  public ProfileStationPattern( )
  {
    super( "Station", Messages.getString( "ProfilePatternInputReplacer_3" ) ); //$NON-NLS-1$  //$NON-NLS-2$
  }

  @Override
  public String getReplacement( final IProfilePatternData data, final String param )
  {
    final IProfil profile = data.getProfile();
    if( profile == null )
      return StringUtils.EMPTY;

    return String.format( "%.4f", profile.getStation() );
  }
}