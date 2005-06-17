package com.bce.eind.core.result;

/** Ein Ergebnisdatensatz einer 1D Berechnungen 
 * 
 * Eine Zuordnung Station(String) -> Ergebniswert(double)
 * */
public interface IResult extends Iterable<Double>
{
  enum TYPE { SOHLHOEHE, BOESCHNG_LI, BOESCHUNG_RE, WSP, Q, LAENGE, PROFILART, VZK, WSP_BR, PROFILKENNUNG, SCHLEPPSPANNUNG, AUSUFERUNG_LI, AUSUFERUNG_RE, H };
  
  public String getName();
  
  public Double getResult( final double station );
  
  public void addResult( final double station, final double value );
  
  public Double removeResult( final double station );
  
  public TYPE getType();
}
