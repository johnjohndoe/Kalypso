package org.kalypso.ui.rrm.catalog;

import java.net.URL;

import org.kalypso.core.catalog.CatalogManager;
import org.kalypso.core.catalog.ICatalogContribution;

public class RRMFeatureTypeStyleCatalogContribution implements ICatalogContribution
{
  @Override
  public void contributeTo( final CatalogManager catalogManager )
  {
    final URL catalogURL = getClass().getResource( "resources/catalog.xml" ); //$NON-NLS-1$
    catalogManager.addNextCatalog( catalogURL );
  }
}
