package com.bce.datacenter;

import junit.framework.TestCase;

import com.bce.datacenter.db.IngresDatabase;

/**
 * Use this TestCase because it performs db connection in the setUp() method.
 * 
 * @author ingres
 */
public class AbstractTest extends TestCase
{
  protected IngresDatabase ingres;

  /**
   * @see com.bce.datacenter.AbstractTest#setUp()
   */
  @Override
  protected void setUp( ) throws Exception
  {
    ingres = new IngresDatabase( "jdbc:edbc://pc072:I57/BCE_PC072::mertesdorf/INGRES", "ingres", "ingres" );
    
    super.setUp();
  }
  
  /**
   * @see junit.framework.TestCase#tearDown()
   */
  @Override
  protected void tearDown( ) throws Exception
  {
    ingres.getConnection().close();
    
    super.tearDown();
  }
}