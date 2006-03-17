package com.bce.eind.core.result;

/**
 * Ein Ergebnisdatensatz einer 1D Berechnungen Eine Zuordnung Station(String) ->
 * Ergebniswert(double)
 */
public interface IResultSet extends Iterable<String>
{
  enum TYPE
  {
    SOHLHOEHE( "Sohlh�he", "NN + m" ), BOESCHNG_LI( "B�schung links", "NN + m" ), BOESCHUNG_RE(
        "B�schung rechts", "NN + m" ), WSP( "Wasserspiegel H�he", "NN + m" ), Q( "Abfluss", "m�/s" ), LAENGE(
        "L�nge", "m" ), PROFILART( "Profilart", "" ), VZK( "Verzweigungskennung", "" ), WSP_BR(
        "Wasserspiegel Breite", "m" ), PROFILKENNUNG( "Profilkennung", "" ), SCHLEPPSPANNUNG(
        "Schleppspannung", "" ), AUSUFERUNG_LI( "Ausuferung links", "NN + m" ), AUSUFERUNG_RE(
        "Ausuferung rechts", "NN + m" ), H( "", "" );

    private final String m_name;
    private final String m_unit;

    private TYPE( final String name, final String unit )
    {
      m_name = name;
      m_unit = unit;
    }
    
    @Override
    public String toString( )
    {
      return m_name;
    }
    
    public String getUnit( )
    {
      return m_unit;
    }
  };

  public String getName( );

  /** Returns the previoulsy asgined value or null, if no value was asigned. */
  public Double putValue( final String station, final TYPE type, final double value );

  public Double getValue( final String station, final TYPE type );

  public IStationResult getValues( final String station );
}
