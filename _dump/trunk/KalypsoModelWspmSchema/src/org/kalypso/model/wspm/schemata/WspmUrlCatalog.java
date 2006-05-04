package org.kalypso.model.wspm.schemata;

import java.net.URL;
import java.util.Map;

import org.kalypso.contribs.java.net.AbstractUrlCatalog;

public class WspmUrlCatalog extends AbstractUrlCatalog
{
  /**
   * @see org.kalypso.contribs.java.net.AbstractUrlCatalog#fillCatalog(java.lang.Class, java.util.Map)
   */
  @Override
  protected void fillCatalog( final Class myClass, final Map<String, URL> catalog )
  {
    catalog.put( "org.kalypso.model.wspm", myClass.getResource( "wspm.xsd" ) );
  }
}
