package org.kalypso.model.wspm.tuhh.schema.schemata;

import java.net.URL;
import java.util.Map;

import org.kalypso.contribs.java.net.AbstractUrlCatalog;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;

public class WspmTuhhUrlCatalog extends AbstractUrlCatalog implements IWspmTuhhConstants
{
  /**
   * @see org.kalypso.contribs.java.net.AbstractUrlCatalog#fillCatalog(java.lang.Class, java.util.Map)
   */
  @Override
  protected void fillCatalog( final Class myClass, final Map<String, URL> catalog, Map<String, String> prefixes )
  {
    catalog.put( NS_WSPM_TUHH, myClass.getResource( "wspmTuhhSteadyState.xsd" ) );
    prefixes.put( NS_WSPM_TUHH, "tuhh" );
  }
}
