package org.kalypso.flows.xplanung;

import java.net.MalformedURLException;
import java.net.URL;

import org.kalypso.contribs.java.net.UrlUtilities;
import org.kalypso.core.catalog.CatalogManager;
import org.kalypso.core.catalog.ICatalog;
import org.kalypso.core.catalog.ICatalogContribution;

public class FeatureTypeStyleCatalogContribution implements ICatalogContribution
{

  public void contributeTo( final CatalogManager catalogManager )
  {

    try
    {
      final UrlUtilities utilities = new UrlUtilities();
      final URL archiveURL = getClass().getResource( "resources/xplanungStyleCatalog.zip" );
      final URL catalogURL = utilities.resolveURL( new URL( "jar:" + archiveURL.toString() + "!/" ), "urn/catalog.xml" );
      final ICatalog baseCatalog = catalogManager.getBaseCatalog();
      baseCatalog.addNextCatalog( catalogURL );
    }
    catch( MalformedURLException e )
    {
      e.printStackTrace();
    }
  }
}
