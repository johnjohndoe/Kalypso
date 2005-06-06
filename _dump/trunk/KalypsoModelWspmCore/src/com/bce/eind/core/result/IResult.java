package com.bce.eind.core.result;

/** Ein Ergebnisdatensatz einer 1D Berechnungen 
 * 
 * Eine Zuordnung Station(String) -> Ergebniswert(double)
 * */
public interface IResult extends Iterable<Double>
{
  enum TYPE { WSP, v, H };
  
  public String getName();
  
  public Double getResult( final double station );
  
  public void addResult( final double station, final double value );
  
  public Double removeResult( final double station );
  
  public TYPE getType();
}
