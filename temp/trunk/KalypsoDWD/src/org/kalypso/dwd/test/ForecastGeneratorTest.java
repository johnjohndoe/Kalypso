package org.kalypso.dwd.test;

import org.kalypso.dwd.ForecastGenerator;

import junit.framework.TestCase;

/**
 * @author doemming
 */
public class ForecastGeneratorTest extends TestCase
{
  public void test()
  {
    final String[] args=new String[]{"C:\\TMP\\raster\\raster.conf"};
    ForecastGenerator.main(args);
  }
}
