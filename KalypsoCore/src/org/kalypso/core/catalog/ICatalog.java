/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 * 
 *  and
 *  
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 * 
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 * 
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 * 
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 * 
 *  Contact:
 * 
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *   
 *  ---------------------------------------------------------------------------*/
package org.kalypso.core.catalog;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.List;

import javax.xml.bind.JAXBException;

/**
 * Interface for a catalog<br>
 * <b>definitions:</b><br>
 * systemID: anything to identify something, whatever applications use uri,urn,id,phrase<br>
 * publicID: anything to identify something, whatever applications use uri,urn,id,phrase<br>
 * uri: something that points to the resource<br>
 * <br>
 * remarks:<br>
 * 1. a catalog may have uri entries that are relative (against the catalog-location), see also resolve-methode<br>
 * 2. the only difference beetween systemID and publicID is order while resolving<br>
 * 
 * @author doemming
 */
public interface ICatalog
{

  // /**
  // * resolves catalog for baseURN<br>
  // * TODO this method is intended for categorized catalogs in kalypso and it must be checked if we can remove it from
  // * the interface (althought catalogs are kept categorized intern)
  // */
  // @SuppressWarnings("unchecked")
  // public ICatalog getCatalogFor( final String baseURN ) throws Exception;

  /**
   * returns a uri (absolute) or the systemID if no match was found int the catalog<br>
   * callers usually resolve the result against their local context
   */
  public String resolve( final String systemID, final String publicID );

  /**
   * resolves all URNs that fit to the pattern<br>
   * the pattern must start with "urn:" and may and with "*"<br>
   * example: pattern="urn:ogc:sld:www.kalypso.tu-harburg.de_na:catchment:*"<br>
   * callers may use this methode for a dialog that let the user choose among the matching URNs<br>
   * the choosen URN then must be resolved by ICatalog.resolve(urn,run)
   * 
   * @param urnPattern
   *          pattern to match
   * @return URNs for pattern
   */
  public List<String> getEnryURNS( String urnPattern ) throws MalformedURLException, JAXBException;

  /**
   * adds an entry to the catalog
   * 
   * @param uri
   *          absolute pointer to the resource
   * @param publicID
   *          the systemID
   * @param systemID
   *          the publicID
   */
  public void addEntry( String uri, String systemID, String publicID );

  /**
   * adds an entry to the catalog, but changes the uri to a relative reference to the catalog itself<br>
   * 
   * @param uri
   *          absolute pointer to the resource
   * @param publicID
   *          the systemID
   * @param systemID
   *          the publicID
   */
  public void addEntryRelative( String uri, String systemID, String publicID );

  public void addNextCatalog( URL catalogURL );

}