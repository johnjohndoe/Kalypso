package org.kalypso.ui.calcwizard.bericht;

import java.util.Properties;

/**
 * @author belger
 */
public interface IBerichtExporter
{
  public void setArguments( final Properties arguments );
  
  public String toString();
}
