package com.bce.eind.core.profil;


public class ProfilBuildingProperty
{
  public final static ProfilBuildingProperty BREITE = new ProfilBuildingProperty(
      "gr��te Breite/Durchmesser", null );

  public final static ProfilBuildingProperty HOEHE = new ProfilBuildingProperty( "Gesamth�he [m]", null );

  public final static ProfilBuildingProperty SOHLGEFAELLE = new ProfilBuildingProperty(
      "Sohlgef�lle [1/1000]", null );

  public final static ProfilBuildingProperty BEZUGSPUNKT_X = new ProfilBuildingProperty(
      "Bezugspunkt Breite [m]", null );

  public final static ProfilBuildingProperty BEZUGSPUNKT_Y = new ProfilBuildingProperty(
      "Bezugspunkt H�he [NN+m]", null );

  public final static ProfilBuildingProperty STEIGUNG = new ProfilBuildingProperty(
      "Verh�ltnis der Dreieckseiten [1/100]", null );

  public final static ProfilBuildingProperty RAUHEIT = new ProfilBuildingProperty( "Rauheit", "Rauheitsbeiwert im Durchlass" );

  public final static ProfilBuildingProperty PFEILERFORM = new ProfilBuildingProperty(
      "Pfeilerformbeiwert", null );

  public final static ProfilBuildingProperty UNTERWASSER = new ProfilBuildingProperty(
      "Unterwasser [NN+m]", "H�he der Gew�ssersohle im Unterwasser" );

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
