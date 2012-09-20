package org.kalypso.model.wspm.tuhh.core.profile.pattern;

import org.kalypso.model.wspm.core.profil.IProfile;
import org.kalypso.model.wspm.tuhh.core.i18n.Messages;

/**
 * @author Gernot Belger
 */
public final class ProfileWspWinFilenamePattern extends AbstractProfileStringPattern
{
  public ProfileWspWinFilenamePattern( )
  {
    super( "WspWin", Messages.getString( "ProfilePatternInputReplacer_4" ) ); //$NON-NLS-1$  //$NON-NLS-2$
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.core.profile.pattern.IValueWithFormat#getValue(org.kalypso.model.wspm.tuhh.core.profile.pattern.IProfilePatternData,
   *      java.lang.String)
   */
  @Override
  public String getValue( final IProfilePatternData data, final String params )
  {
    final IProfile profile = data.getProfile();
    if( profile == null )
      return null;

    final double station = profile.getStation();
    return String.format( "%.4f", station ).replace( '.', '+' ).replace( ' ', '0' ); //$NON-NLS-1$
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.core.profile.pattern.IValueWithFormat#getDefaultWidth(java.lang.String)
   */
  @Override
  public int getDefaultWidth( final String params )
  {
    return 10;
  }
}