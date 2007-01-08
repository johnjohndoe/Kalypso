package org.kalypso.kalypsosimulationmodel.core.util;

import java.net.URL;
import java.util.Map;

import org.kalypso.contribs.java.net.AbstractUrlCatalog;

/**
 * Catalog that provides the url to the shape conversion default schema
 * 
 * @author Dejan Antanaskovic, <a href="mailto:dejan.antanaskovic@tuhh.de">dejan.antanaskovic@tuhh.de</a>
 */

public class UrlCatalogShapeImport extends AbstractUrlCatalog
{
	  public static final String NS_SHAPEDATAMODEL     = "http://www.tu-harburg.de/wb/kalypso/schemata/simulationbase";
	  //public static final String NS_SHAPEDATAMODEL     = "http://www.tuhh.de/Kalypso1D2D/shapeData";
	  public static final String PREFIX_SHAPEDATAMODEL = "shpData";

	  /**
	   * @see org.kalypso.contribs.java.net.AbstractUrlCatalog#fillCatalog(java.lang.Class, java.util.Map)
	   */
	  @Override
	  protected void fillCatalog( final Class myClass, final Map<String, URL> catalog, Map<String, String> prefixes )
	  {
	    //catalog.put( NS_SHAPEDATAMODEL,  myClass.getResource( "shapeImportDefaultSchema.xsd" ) );
	    catalog.put( NS_SHAPEDATAMODEL,  myClass.getResource( "simulation_model_terrain.xsd" ) );
	    prefixes.put( NS_SHAPEDATAMODEL, PREFIX_SHAPEDATAMODEL );
	  }

	}