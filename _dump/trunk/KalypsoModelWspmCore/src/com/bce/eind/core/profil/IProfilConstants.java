package com.bce.eind.core.profil;

public interface IProfilConstants
{
  /** Format String für Formatierung der Station in der GUI */
  public final String FMT_STATION = "%.4f";

  /** default RauheitenTyp für ein neues Profil */
  public final RAUHEIT_PROPERTY DEFAULT_RAUHEIT_TYP = RAUHEIT_PROPERTY.ks;

  /** alle möglichen Bauwerke eines Profils */
  public static enum BUILDING_TYP
  {
    BRUECKE, EI, KREIS, MAUL, NONE, TRAPEZ, WEHR;
  }

  /** alle möglichen Trenner eines Profils */
  public static enum DEVIDER_TYP
  {
    BORDVOLL, DURCHSTROEMTE, FLIESSZONE, WEHR;
  }

  public enum DEVIDER_PROPERTY
  {
    BOESCHUNG, SOHLE
  };

  /** alle möglichen Rauheiten eines Profils */
  public static enum RAUHEIT_PROPERTY
  {
    ks, kst
  };

  public static enum PROFIL_PROPERTY
  {
    KOMMENTAR, MEHRFELDBRUECKE, METASTRINGS, STATION,RAUHEIT_TYP, STATUS, VERZWEIGUNGSKENNUNG, WASSERSPIEGEL
  }

  public static enum BUILDING_PROPERTY
  {
    BREITE( "größte Breite/Durchmesser", null ),
    HOEHE ( "Gesamthöhe [m]", null ),
    SOHLGEFAELLE ( "Sohlgefälle [1/1000]", null ),
   BEZUGSPUNKT_X ( "Bezugspunkt Breite [m]", null ),
   BEZUGSPUNKT_Y ( "Bezugspunkt Höhe [NN+m]", null ),
    STEIGUNG ( "Verhältnis der Dreieckseiten [1/100]", null ),
    RAUHEIT( "Rauheit", "Rauheitsbeiwert im Durchlass" ),
    PFEILERFORM ( "Pfeilerformbeiwert", null ),
    UNTERWASSER ( "Unterwasser [NN+m]", "Höhe der Gewässersohle im Unterwasser" ),
   WEHRART ( "Wehrart", "Form der Wehrkrone" );;

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
