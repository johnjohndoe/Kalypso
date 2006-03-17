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
    BRUECKE, EI, KREIS, MAUL, TRAPEZ, WEHR;
  }

  public BUILDING_TYP getTyp( );

  public POINT_PROPERTY[] getPointProperties( );

  public Collection<BUILDING_PROPERTY> getBuildingProperties( );

  public Object getValueFor( final BUILDING_PROPERTY buildingValue ) throws ProfilDataException;

  public Object setValue( final BUILDING_PROPERTY property, final Object value )
      throws ProfilDataException;

  public boolean hasProperty( final BUILDING_PROPERTY profilBuildingProperty );

  public static enum BUILDING_PROPERTY
  {
    BREITE( "gr��te Breite/Durchmesser", null ), HOEHE( "Gesamth�he [m]", null ), SOHLGEFAELLE(
        "Sohlgef�lle [1/1000]", null ), BEZUGSPUNKT_X( "Bezugspunkt Breite [m]", null ), BEZUGSPUNKT_Y(
        "Bezugspunkt H�he [NN+m]", null ), STEIGUNG( "Verh�ltnis der Dreieckseiten [1/100]", null ), RAUHEIT(
        "Rauheit", "Rauheitsbeiwert im Durchlass" ), FORMBEIWERT( "Formbeiwert", null ), UNTERWASSER(
        "Unterwasser [NN+m]", "H�he der Gew�ssersohle im Unterwasser" ), WEHRART( "Wehrart",
        "Form der Wehrkrone" );

    private BUILDING_PROPERTY( final String label, final String tooltip )
    {
      m_label = label;
      m_tooltip = tooltip;
    }

    @Override
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
