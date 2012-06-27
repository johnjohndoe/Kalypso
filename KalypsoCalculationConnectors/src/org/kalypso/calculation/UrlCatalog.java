package org.kalypso.calculation;

import java.net.URL;
import java.util.Map;

import org.kalypso.calculation.plc.postprocessing.binding.IScenarioResults;
import org.kalypso.contribs.java.net.AbstractUrlCatalog;

public class UrlCatalog extends AbstractUrlCatalog
{
  @Override
  protected void fillCatalog( final Class< ? > myClass, final Map<String, URL> catalog, final Map<String, String> prefixes )
  {
    catalog.put( IScenarioResults.POSTPROCESSING_NAMESPACE, myClass.getResource( "plc/postprocessing/schema/ResultDataModel.xsd" ) ); //$NON-NLS-1$
    prefixes.put( IScenarioResults.POSTPROCESSING_NAMESPACE, "cchainres" ); //$NON-NLS-1$
  }

}
