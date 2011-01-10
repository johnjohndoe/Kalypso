package org.kalypso.model.wspm.tuhh.core.profile.pattern;

import org.kalypso.commons.patternreplace.AbstractPatternInput;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.tuhh.core.i18n.Messages;

/**
 * @author Gernot Belger
 */
public final class ProfileStationPattern extends AbstractPatternInput<IProfil>
{
  public ProfileStationPattern( )
  {
    super( "Station", Messages.getString( "ProfilePatternInputReplacer_3" ) ); //$NON-NLS-1$  //$NON-NLS-2$
  }

  /**
   * @see org.kalypso.commons.patternreplace.IPatternInput#getReplacement(java.lang.Object, java.lang.String)
   */
  @Override
  public String getReplacement( final IProfil profile, final String param )
  {
    final String station = String.format( "%.4f", profile.getStation() ); //$NON-NLS-1$
    return station;
  }
}