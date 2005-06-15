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
  
  private final boolean m_clonable;

  public final static ProfilPointProperty BEWUCHS_AX = new ProfilPointProperty( "AX", true, true,
      false,true, 4 );

  public final static ProfilPointProperty BEWUCHS_AY = new ProfilPointProperty( "AY", true, true,
      false,true, 4 );

  public final static ProfilPointProperty BEWUCHS_DP = new ProfilPointProperty( "DP", true, true,
      false,true, 4 );

  public final static ProfilPointProperty BREITE = new ProfilPointProperty( "Breite", false, true,
      true,true, 4 );

  public final static ProfilPointProperty DURCHSTROEMTE = new ProfilPointProperty( "Durchströmte",
      false, false, false,false, 4 );

  public final static ProfilPointProperty HOCHWERT = new ProfilPointProperty( "Hochwert", true,
      true, true,true, 4 );

  public final static ProfilPointProperty UNTERKANTEBRUECKE = new ProfilPointProperty(
      "Brückenunterkante", true, true, false,true, 4 );

  public final static ProfilPointProperty OBERKANTEBRUECKE = new ProfilPointProperty(
      "Brückenoberkante", true, true, false,true, 4 );

  public final static ProfilPointProperty HOEHE = new ProfilPointProperty( "Höhe", false, true,
      true,true, 4 );

  public final static ProfilPointProperty RAUHEIT = new ProfilPointProperty( "Rauheit", false,
      true, false,true, 4 );

  public final static ProfilPointProperty RECHTSWERT = new ProfilPointProperty( "Rechtswert", true,
      true, true,true, 4 );

  public final static ProfilPointProperty TRENNFLAECHE = new ProfilPointProperty( "Trennefläche",
      false, false, false,false, 4 );

  public final static ProfilPointProperty BORDVOLL = new ProfilPointProperty( "Bordvollpunkt",
      true, false, false,false, 4 );

  
  private ProfilPointProperty( final String label, final boolean optional, final boolean visible,
      final boolean interpolation,final boolean clonable, final int precision )
  {
    m_visible = visible;
    m_optional = optional;
    m_label = label;
    m_interpolation = interpolation;
    m_precision = precision;
    m_clonable = clonable;
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

  /**
   * @return Returns the clonable.
   */
  public boolean isClonable( )
  {
    return m_clonable;
  }
  
  @Override
  public String toString( )
  {
    return m_label;
  }
}
