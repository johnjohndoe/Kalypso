package org.kalypso.model.wspm.schema;

import java.net.URL;
import java.util.Map;

import org.kalypso.contribs.java.net.AbstractUrlCatalog;
import org.kalypso.model.wspm.core.IWspmConstants;

public class WspmUrlCatalog extends AbstractUrlCatalog implements IWspmConstants
{
  /**
   * @see org.kalypso.contribs.java.net.AbstractUrlCatalog#fillCatalog(java.lang.Class, java.util.Map)
   */
  @Override
  protected void fillCatalog( final Class myClass, final Map<String, URL> catalog, Map<String, String> prefixes )
  {
    catalog.put( NS_WSPM, myClass.getResource( "schemata/wspm.xsd" ) );
    prefixes.put( NS_WSPM, "wspm" );

    catalog.put( NS_WSPMCOMMONS, myClass.getResource( "schemata/wspmCommons.xsd" ) );
    prefixes.put( NS_WSPMCOMMONS, "wspmcommon" );

    catalog.put( NS_WSPMPROJ, myClass.getResource( "schemata/wspmProject.xsd" ) );
    prefixes.put( NS_WSPMPROJ, "wspmproj" );

    catalog.put( NS_WSPMPROF, myClass.getResource( "schemata/profile.xsd" ) );
    prefixes.put( NS_WSPMPROF, "prof" );

    catalog.put( NS_NA_WSPM, myClass.getResource( "schemata/couplingNaWspm.xsd" ) );
    prefixes.put( NS_NA_WSPM, "wspmna" );
  }
}
