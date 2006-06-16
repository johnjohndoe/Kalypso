package org.kalypso.model.wspm.tuhh.ui.sld;

import java.net.URL;

import org.kalypso.core.catalog.CatalogManager;
import org.kalypso.core.catalog.ICatalog;
import org.kalypso.core.catalog.ICatalogContribution;

public class WspmTuhhFeatureTypeStyleCatalogContribution implements ICatalogContribution
{

  public void contributeTo( final CatalogManager catalogManager )
  {
    final URL catalogURL = getClass().getResource( "resources/urn/catalog.xml" );
    final ICatalog baseCatalog = catalogManager.getBaseCatalog();
    baseCatalog.addNextCatalog( catalogURL );
  }
}
