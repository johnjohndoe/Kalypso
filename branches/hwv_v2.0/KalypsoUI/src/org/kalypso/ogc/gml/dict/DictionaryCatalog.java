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
package org.kalypso.ogc.gml.dict;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.runtime.IStatus;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.core.catalog.CatalogManager;
import org.kalypso.core.catalog.ICatalog;
import org.kalypso.core.util.pool.IPoolableObjectType;
import org.kalypso.core.util.pool.PoolableObjectType;
import org.kalypso.i18n.Messages;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * A catalog to retrieve dictionary entries from.
 * 
 * @author Gernot Belger
 */
public class DictionaryCatalog
{
  private final Map<DictionaryFeature, DictionaryEntry> m_featureMap = new HashMap<DictionaryFeature, DictionaryEntry>();

  /**
   * Retrieves one feature from a dictionary.
   * <p>
   * If any exception occurs, it islogged to the Plugin-Log and null is returned.
   * 
   * @param urnAndRef
   *          Must be of the for<code>urn#id</code>
   * @return null, if the entry or the gml could not be found.
   */
  public Feature getEntry( final String urnAndRef )
  {
    try
    {
      final String[] strings = urnAndRef.split( "#" ); //$NON-NLS-1$
      if( strings.length != 2 )
        throw new IllegalArgumentException( "This catalog can only resolve entries of the form urn#ref: " + urnAndRef ); //$NON-NLS-1$

      final String urn = strings[0].trim();
      final String ref = strings[1].trim();

      if( urn.length() == 0 || ref.length() == 0 )
        return null;

      final URL location = resolveUrn( urn );
      if( urn == null )
        return null;

      // load gml from url
      final IPoolableObjectType key = new PoolableObjectType( "gml", location.toExternalForm(), null ); //$NON-NLS-1$
      final DictionaryEntry dictionaryEntry = new DictionaryEntry( key );

      // wait for the gml to be loaded
      dictionaryEntry.schedule();
      dictionaryEntry.join();

      if( !dictionaryEntry.getResult().isOK() )
        return null;

      final GMLWorkspace workspace = dictionaryEntry.getWorkspace();
      if( workspace == null )
        return null;

      // we wrap the feature, so it cannot be changed and we know how to release it
      final Feature originalFeature = workspace.getFeature( ref );
      if( originalFeature == null )
        return null;

      final DictionaryFeature feature = new DictionaryFeature( originalFeature );

      // Remeber the entry for disposal
      m_featureMap.put( feature, dictionaryEntry );

      return feature;
    }
    catch( final InterruptedException e )
    {
      final IStatus status = StatusUtilities.statusFromThrowable( e );
      KalypsoGisPlugin.getDefault().getLog().log( status );
      return null;
    }
    catch( final MalformedURLException e )
    {
      final IStatus status = StatusUtilities.statusFromThrowable( e );
      KalypsoGisPlugin.getDefault().getLog().log( status );
      return null;
    }

  }

  public void releaseEntry( final Feature entry )
  {
    if( !(entry instanceof DictionaryFeature) )
      throw new IllegalArgumentException( "This was no feature got from this catalog: " + entry ); //$NON-NLS-1$

    final DictionaryEntry dictEntry = m_featureMap.get( entry );
    if( dictEntry == null )
    {
      final IStatus status = StatusUtilities.createWarningStatus( Messages.getString("org.kalypso.ogc.gml.dict.DictionaryCatalog.releaseunknownentry") + entry ); //$NON-NLS-1$
      KalypsoGisPlugin.getDefault().getLog().log( status );
      return;
    }

    dictEntry.dispose();
    m_featureMap.remove( entry );
    return;
  }

  private URL resolveUrn( final String urn ) throws MalformedURLException
  {
    // search for url
    final CatalogManager catalogManager = KalypsoCorePlugin.getDefault().getCatalogManager();
    final ICatalog baseCatalog = catalogManager.getBaseCatalog();
    final String uri = baseCatalog.resolve( urn, urn );
    return new URL( uri );
  }

}
