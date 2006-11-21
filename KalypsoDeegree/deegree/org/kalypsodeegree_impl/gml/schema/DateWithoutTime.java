package org.kalypsodeegree_impl.gml.schema;

import java.util.Date;

/**
 * DateTime dient nur als Klassifikationstyp. Es wird benutzt, um klar zu deuten dass es sich um eine Date ohne
 * Time-Angabe handelt.
 * 
 * @author schlienger
 */
public class DateWithoutTime extends Date
{
  public DateWithoutTime()
  {
    super();
  }

  public DateWithoutTime( final Date date )
  {
    this( date.getTime() );
  }

  public DateWithoutTime( long date )
  {
    super( date );
  }
}
