package com.bce.eind.core.profil;

import java.util.Arrays;
import java.util.List;

public class ProfilBuildingProperty
{
  private final String m_label;

  public final static ProfilBuildingProperty BREITE = new ProfilBuildingProperty(
      "gr��te Breite/Durchmesser" );

  public final static ProfilBuildingProperty HOEHE = new ProfilBuildingProperty( "Gesamth�he" );

  public final static ProfilBuildingProperty SOHLGEFAELLE = new ProfilBuildingProperty(
      "Sohlgef�lle[Promille]" );

  public final static ProfilBuildingProperty BEZUGSPUNKT_X = new ProfilBuildingProperty(
      "Bezugspunkt X-Wert" );

  public final static ProfilBuildingProperty BEZUGSPUNKT_Y = new ProfilBuildingProperty(
      "Bezugspunkt Y-Wert" );

  public final static ProfilBuildingProperty STEIGUNG = new ProfilBuildingProperty(
      "Verh�ltnis der Dreieckseiten" );

  public final static List<ProfilBuildingProperty> asList = Arrays.asList(
      ProfilBuildingProperty.BREITE, ProfilBuildingProperty.HOEHE, ProfilBuildingProperty.STEIGUNG,
      ProfilBuildingProperty.STEIGUNG, ProfilBuildingProperty.SOHLGEFAELLE,
      ProfilBuildingProperty.BEZUGSPUNKT_X, ProfilBuildingProperty.BEZUGSPUNKT_Y );

  private ProfilBuildingProperty( final String label )
  {
    m_label = label;
  }

  /**
   * @return Returns the label.
   */
  public String getLabel( )
  {
    return m_label;
  }

}
