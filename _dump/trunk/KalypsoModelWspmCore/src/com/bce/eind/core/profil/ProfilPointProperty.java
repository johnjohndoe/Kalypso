/*
 * Created on 22.02.2005
 */
package com.bce.eind.core.profil;

/**
 * @author kimwerner
 */
public class ProfilPointProperty
{
  private final String m_label;

  private final boolean m_optional;

  private final boolean m_visible;

  private final boolean m_interpolation;

  private final int m_precision;

  public final static ProfilPointProperty BEWUCHS_AX = new ProfilPointProperty( "AX", true, true,
      false, 4 );

  public final static ProfilPointProperty BEWUCHS_AY = new ProfilPointProperty( "AY", true, true,
      false, 4 );

  public final static ProfilPointProperty BEWUCHS_DP = new ProfilPointProperty( "DP", true, true,
      false, 4 );

  public final static ProfilPointProperty BREITE = new ProfilPointProperty( "Breite", false, true,
      true, 4 );

  public final static ProfilPointProperty DURCHSTROEMTE = new ProfilPointProperty( "Durchströmte",
      false, false, false, 4 );

  public final static ProfilPointProperty HOCHWERT = new ProfilPointProperty( "Hochwert", true,
      true, true, 4 );

  public final static ProfilPointProperty UNTERKANTEBRUECKE = new ProfilPointProperty(
      "Brückenunterkante", true, true, false, 4 );

  public final static ProfilPointProperty OBERKANTEBRUECKE = new ProfilPointProperty(
      "Brückenoberkante", true, true, false, 4 );

  public final static ProfilPointProperty HOEHE = new ProfilPointProperty( "Höhe", false, true,
      true, 4 );

  public final static ProfilPointProperty RAUHEIT = new ProfilPointProperty( "Rauheit", false,
      true, false, 4 );

  public final static ProfilPointProperty RECHTSWERT = new ProfilPointProperty( "Rechtswert", true,
      true, true, 4 );

  public final static ProfilPointProperty TRENNFLAECHE = new ProfilPointProperty( "Trennefläche",
      false, false, false, 4 );

  public final static ProfilPointProperty BORDVOLL = new ProfilPointProperty( "Bordvollpunkt",
      true, true, false, 4 );

  private ProfilPointProperty( final String label, final boolean optional, final boolean visible,
      final boolean interpolation, final int precision )
  {
    m_visible = visible;
    m_optional = optional;
    m_label = label;
    m_interpolation = interpolation;
    m_precision = precision;
  }

  /**
   * @return Returns the visible.
   */
  public boolean isVisible( )
  {
    return m_visible;
  }

  /**
   * @return Returns the label.
   */
  public String getLabel( )
  {
    return m_label;
  }

  /**
   * @return Returns the optional.
   */
  public boolean isOptional( )
  {
    return m_optional;
  }

  /**
   * @return Returns the interpolation.
   */
  public boolean isInterpolation( )
  {
    return m_interpolation;
  }

  /**
   * @return Returns the precision.
   */
  public int getPrecision( )
  {
    return m_precision;
  }

}
