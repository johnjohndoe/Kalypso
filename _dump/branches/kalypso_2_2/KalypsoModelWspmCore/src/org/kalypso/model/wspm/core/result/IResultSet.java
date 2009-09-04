package org.kalypso.model.wspm.core.result;

/**
 * Ein Ergebnisdatensatz einer 1D Berechnungen Eine Zuordnung Station(String) ->
 * Ergebniswert(double)
 */
public interface IResultSet extends Iterable<String>
{
  public String getName( );

  /** Returns the previoulsy asgined value or null, if no value was asigned. */
  public Double putValue( final String station, final String type, final double value );

  public Double getValue( final String station, final String type );

  public IStationResult getValues( final String station );
}
