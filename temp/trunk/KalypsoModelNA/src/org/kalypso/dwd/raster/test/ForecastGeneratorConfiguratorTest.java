package org.kalypso.dwd.raster.test;

import junit.framework.TestCase;

import org.kalypso.dwd.raster.ForecastGeneratorConfigurator;

/**
 * @author doemming
 */
public class ForecastGeneratorConfiguratorTest extends TestCase
{
  public void testForecastGeneratorConfigurator()
  {
    final String[] a2 = new String[]
    {
        "c:\\\\TMP\\raster\\modell.gml",
        "c:\\\\TMP\\raster\\base.dwd",
    };
    ForecastGeneratorConfigurator.main( a2 );
  }
}