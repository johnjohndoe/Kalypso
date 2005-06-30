package com.bce.eind.core.profil;


public class ProfilBuildingProperty
{
  public final static ProfilBuildingProperty BREITE = new ProfilBuildingProperty(
      "größte Breite/Durchmesser", null );

  public final static ProfilBuildingProperty HOEHE = new ProfilBuildingProperty( "Gesamthöhe [m]", null );

  public final static ProfilBuildingProperty SOHLGEFAELLE = new ProfilBuildingProperty(
      "Sohlgefälle [1/1000]", null );

  public final static ProfilBuildingProperty BEZUGSPUNKT_X = new ProfilBuildingProperty(
      "Bezugspunkt Breite [m]", null );

  public final static ProfilBuildingProperty BEZUGSPUNKT_Y = new ProfilBuildingProperty(
      "Bezugspunkt Höhe [NN+m]", null );

  public final static ProfilBuildingProperty STEIGUNG = new ProfilBuildingProperty(
      "Verhältnis der Dreieckseiten [1/100]", null );

  public final static ProfilBuildingProperty RAUHEIT = new ProfilBuildingProperty( "Rauheit", "Rauheitsbeiwert im Durchlass" );

  public final static ProfilBuildingProperty PFEILERFORM = new ProfilBuildingProperty(
      "Pfeilerformbeiwert", null );

  public final static ProfilBuildingProperty UNTERWASSER = new ProfilBuildingProperty(
      "Unterwasser [NN+m]", "Höhe der Gewässersohle im Unterwasser" );

  private final String m_label;

  private final String m_tooltip;

  private ProfilBuildingProperty( final String label, final String tooltip )
  {
    m_label = label;
    m_tooltip = tooltip;
  }

  /**
   * @return Returns the label.
   */
  public String getLabel( )
  {
    return m_label;
  }
  
  public String getTooltip( )
  {
    return m_tooltip;
  }
}
