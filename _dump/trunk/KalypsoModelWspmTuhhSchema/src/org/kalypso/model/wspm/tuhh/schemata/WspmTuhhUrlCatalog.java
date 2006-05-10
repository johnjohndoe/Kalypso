package org.kalypso.model.wspm.tuhh.schemata;

import java.net.URL;
import java.util.Map;

import org.kalypso.contribs.java.net.AbstractUrlCatalog;
import org.kalypso.ui.model.wspm.IWspmConstants;

public class WspmTuhhUrlCatalog extends AbstractUrlCatalog
{
  /**
   * @see org.kalypso.contribs.java.net.AbstractUrlCatalog#fillCatalog(java.lang.Class, java.util.Map)
   */
  @Override
  protected void fillCatalog( final Class myClass, final Map<String, URL> catalog )
  {
    catalog.put( IWspmConstants.NS_WSPM_TUHH, myClass.getResource( "wspmTuhhSteadyState.xsd" ) );
  }
}
