package org.kalypso.ui.calcwizard.bericht;

import java.util.Properties;

/**
 * @author belger
 */
public class AbstractBerichtExporter implements IBerichtExporter
{
  private Properties m_arguments = new Properties();
  private static final String ARG_NAME = "name";

  /**
   * @see org.kalypso.ui.calcwizard.bericht.IBerichtExporter#setArguments(java.util.Properties)
   */
  public void setArguments( final Properties arguments )
  {
    m_arguments.putAll( arguments );
  }
  
  /**
   * @see org.kalypso.ui.calcwizard.bericht.IBerichtExporter#toString()
   */
  public String toString()
  {
    return m_arguments.getProperty( ARG_NAME, "<unbekannt>" );
  }
}
