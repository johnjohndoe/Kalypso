package com.bce.eind.core.profil;


public interface IProfilConstants
{
  /** Format String f�r Formatierung der Station in der GUI */
  public final String FMT_STATION = "%.4f";

  /*
   * default RauheitenTyp f�r das Profil
   */
  public enum RAUHEIT_TYP
  {
    ks, kst
  };

  public final RAUHEIT_TYP DEFAULT_RAUHEIT_TYP = RAUHEIT_TYP.ks;
  
  public enum DEVIDER_PROPERTY
  {
    BOESCHUNG,SOHLE
  };
}
