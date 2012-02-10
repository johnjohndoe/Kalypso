package org.kalypso.calculation;


import java.net.URL;
import java.util.Map;

import org.kalypso.contribs.java.net.AbstractUrlCatalog;

public class UrlCatalog extends AbstractUrlCatalog
{
  public final static String NS_CCHAINRESULTS = "org.kalypso.calculation.plc.postprocessing"; //$NON-NLS-1$

  @Override
  protected void fillCatalog( final Class< ? > myClass, final Map<String, URL> catalog, final Map<String, String> prefixes )
  {
    catalog.put( NS_CCHAINRESULTS, myClass.getResource( "plc/postprocessing/schema/ResultDataModel.xsd" ) ); //$NON-NLS-1$
    prefixes.put( NS_CCHAINRESULTS, "cchainres" ); //$NON-NLS-1$
  }

}
