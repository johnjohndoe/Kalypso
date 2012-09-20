package org.kalypso.model.wspm.tuhh.core.profile.pattern;

import java.math.BigDecimal;

import org.apache.commons.lang3.StringUtils;
import org.kalypso.commons.patternreplace.AbstractPatternInput;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.core.profil.IProfile;
import org.kalypso.model.wspm.core.profil.util.ProfileUtil;
import org.kalypso.model.wspm.tuhh.core.i18n.Messages;

/**
 * @author Gernot Belger
 */
public final class ProfileStationPattern extends AbstractPatternInput<IProfilePatternData> implements IValueWithFormat<Double>
{
  public ProfileStationPattern( )
  {
    super( "Station", Messages.getString( "ProfilePatternInputReplacer_3" ) ); //$NON-NLS-1$  //$NON-NLS-2$
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.core.profile.pattern.IValueWithFormat#getType(java.lang.String)
   */
  @Override
  public Class<Double> getType( final String params )
  {
    return Double.class;
  }

  @Override
  public String getReplacement( final IProfilePatternData data, final String param )
  {
    final Double value = getValue( data, param );
    if( value == null )
      return StringUtils.EMPTY;

    final BigDecimal bigStation = ProfileUtil.stationToBigDecimal( value );
    return String.format( "%s", bigStation ); //$NON-NLS-1$
  }

  @Override
  public Double getValue( final IProfilePatternData data, final String params )
  {
    final IProfile profile = data.getProfile();
    if( profile == null )
      return null;

    return profile.getStation();
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.core.profile.pattern.IValueWithFormat#getDefaultWidth(java.lang.String)
   */
  @Override
  public int getDefaultWidth( final String params )
  {
    return 10;
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.core.profile.pattern.IValueWithFormat#getDefaultPrecision(java.lang.String)
   */
  @Override
  public int getDefaultPrecision( final String params )
  {
    return IProfileFeature.STATION_SCALE;
  }
}