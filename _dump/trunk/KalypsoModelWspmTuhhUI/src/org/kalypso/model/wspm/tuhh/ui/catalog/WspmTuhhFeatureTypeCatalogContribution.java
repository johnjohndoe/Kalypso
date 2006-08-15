package org.kalypso.model.wspm.tuhh.ui.catalog;

import java.net.URL;

import org.kalypso.core.catalog.CatalogManager;
import org.kalypso.core.catalog.ICatalog;
import org.kalypso.core.catalog.ICatalogContribution;

public class WspmTuhhFeatureTypeCatalogContribution implements ICatalogContribution
{
  public void contributeTo( final CatalogManager catalogManager )
  {
    final URL catalogURL = getClass().getResource( "resources/catalog.xml" );
    final ICatalog baseCatalog = catalogManager.getBaseCatalog();
    baseCatalog.addNextCatalog( catalogURL );
  }
}
