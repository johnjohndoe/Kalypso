package com.bce.eind.core.profil;


public class ProfilBuildingProperty
{
  private final String m_label;

  public final  static ProfilBuildingProperty BREITE = new ProfilBuildingProperty("größte Breite/Durchmesser");
  public final  static ProfilBuildingProperty HOEHE = new ProfilBuildingProperty("Gesamthöhe"); 
  public final  static ProfilBuildingProperty SOHLGEFAELLE = new ProfilBuildingProperty("Sohlgefälle[Promille]"); 
  public final  static ProfilBuildingProperty BEZUGSPUNKT_X = new ProfilBuildingProperty("Bezugspunkt X-Wert"); 
  public final  static ProfilBuildingProperty BEZUGSPUNKT_Y = new ProfilBuildingProperty("Bezugspunkt Y-Wert"); 
  public final  static ProfilBuildingProperty STEIGUNG = new ProfilBuildingProperty("Verhältnis der Dreieckseiten"); 
 
  private ProfilBuildingProperty(final String label)
  {
    m_label=label;
  }
  /**
   * @return Returns the label.
   */
  public String getLabel( )
  {
    return m_label;
  }
  

}
