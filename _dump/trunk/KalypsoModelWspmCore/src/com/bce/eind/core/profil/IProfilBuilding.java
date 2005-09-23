package com.bce.eind.core.profil;

import java.util.Collection;

import com.bce.eind.core.profil.IProfilPoint.POINT_PROPERTY;

/**
 * @author kimwerner
 */
public interface IProfilBuilding
{
  public static enum BUILDING_TYP
  {
    BRUECKE, EI, KREIS, MAUL, NONE, TRAPEZ, WEHR;
  }

  public BUILDING_TYP getTyp( );

  public POINT_PROPERTY[] getPointProperties( );

  public Collection<BUILDING_PROPERTY> getBuildingProperties( );

  public double getValue( final BUILDING_PROPERTY buildingValue ) throws ProfilBuildingException;

  public boolean hasProperty( final BUILDING_PROPERTY profilBuildingProperty );

  public static enum BUILDING_PROPERTY
  {
    BREITE( "größte Breite/Durchmesser", null ), HOEHE( "Gesamthöhe [m]", null ), SOHLGEFAELLE(
        "Sohlgefälle [1/1000]", null ), BEZUGSPUNKT_X( "Bezugspunkt Breite [m]", null ), BEZUGSPUNKT_Y(
        "Bezugspunkt Höhe [NN+m]", null ), STEIGUNG( "Verhältnis der Dreieckseiten [1/100]", null ), RAUHEIT(
        "Rauheit", "Rauheitsbeiwert im Durchlass" ), PFEILERFORM( "Pfeilerformbeiwert", null ), UNTERWASSER(
        "Unterwasser [NN+m]", "Höhe der Gewässersohle im Unterwasser" ), WEHRART( "Wehrart",
        "Form der Wehrkrone" );

    private BUILDING_PROPERTY( final String label, final String tooltip )
    {
      m_label = label;
      m_tooltip = tooltip;
    }

    public final String toString( )
    {
      return m_label;
    }

    public final String getTooltip( )
    {
      return m_tooltip;
    }

    private final String m_label;

    private final String m_tooltip;
  }
}
