package org.deegree_impl.gml.schema;

import java.util.Date;

/**
 * DateTime dient nur als Klassifikationstyp. Es wird benutzt, um klar zu deuten
 * dass es sich um eine Date ohne Time-Angabe handelt.
 * 
 * @author schlienger
 */
public class DateWithoutTime extends Date
{
  public DateWithoutTime( )
  {
    super();
  }
  
  public DateWithoutTime( final Date date )
  {
    this( date.getTime() );
  }

  public DateWithoutTime( int year, int month, int date )
  {
    super( year, month, date );
  }

  public DateWithoutTime( int year, int month, int date, int hrs, int min )
  {
    super( year, month, date, hrs, min );
  }

  public DateWithoutTime( int year, int month, int date, int hrs, int min, int sec )
  {
    super( year, month, date, hrs, min, sec );
  }

  public DateWithoutTime( long date )
  {
    super( date );
  }

  public DateWithoutTime( String s )
  {
    super( s );
  }
}
