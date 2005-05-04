package com.bce.eind.core.profil;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

public class ProfilBuildingProperty
{
  private final String m_label;

  public final static ProfilBuildingProperty BREITE = new ProfilBuildingProperty(
      "größte Breite/Durchmesser" );

  public final static ProfilBuildingProperty HOEHE = new ProfilBuildingProperty( "Gesamthöhe" );

  public final static ProfilBuildingProperty SOHLGEFAELLE = new ProfilBuildingProperty(
      "Sohlgefälle[Promille]" );

  public final static ProfilBuildingProperty BEZUGSPUNKT_X = new ProfilBuildingProperty(
      "Bezugspunkt X-Wert" );

  public final static ProfilBuildingProperty BEZUGSPUNKT_Y = new ProfilBuildingProperty(
      "Bezugspunkt Y-Wert" );

  public final static ProfilBuildingProperty STEIGUNG = new ProfilBuildingProperty(
      "Verhältnis der Dreieckseiten" );

  public final static ProfilBuildingProperty RAUHEIT = new ProfilBuildingProperty(
  "Sohlrauheit" );

public final static ProfilBuildingProperty PFEILERFORM = new ProfilBuildingProperty(
  "Formbeiwert der Pfeiler" );

public final static ProfilBuildingProperty UNTERWASSER = new ProfilBuildingProperty(
  "Höhe der Gewässersohle im Unterwasser" );
 
  public final static List<ProfilBuildingProperty> asList = Collections.unmodifiableList(Arrays.asList(
      ProfilBuildingProperty.BREITE, ProfilBuildingProperty.HOEHE, ProfilBuildingProperty.STEIGUNG,
      ProfilBuildingProperty.STEIGUNG, ProfilBuildingProperty.SOHLGEFAELLE,
      ProfilBuildingProperty.BEZUGSPUNKT_X, ProfilBuildingProperty.BEZUGSPUNKT_Y,ProfilBuildingProperty.RAUHEIT,ProfilBuildingProperty.PFEILERFORM,ProfilBuildingProperty.UNTERWASSER ));

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
