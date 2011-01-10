package org.kalypso.model.wspm.tuhh.core.profile.pattern;

import org.kalypso.commons.patternreplace.AbstractPatternInput;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.tuhh.core.i18n.Messages;

/**
 * @author Gernot Belger
 *
 */
public final class ProfileWspWinFilenamePattern extends AbstractPatternInput<IProfil>
{
  public ProfileWspWinFilenamePattern( )
  {
    super( "WspWin", Messages.getString( "ProfilePatternInputReplacer_4" ) ); //$NON-NLS-1$  //$NON-NLS-2$
  }

  /**
   * @see org.kalypso.commons.patternreplace.IPatternInput#getReplacement(java.lang.Object, java.lang.String)
   */
  @Override
  public String getReplacement( final IProfil profile, final String param )
  {
    final double station = profile.getStation();
    final String stationString = String.format( "%.4f", station ).replace( '.', '+' ).replace( ' ', '0' ); //$NON-NLS-1$
    return stationString;
  }
}