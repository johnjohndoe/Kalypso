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
package org.kalypso.ui.catalogs;

import java.net.MalformedURLException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.xml.bind.JAXBException;
import javax.xml.namespace.QName;

import org.eclipse.core.runtime.IStatus;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.core.catalog.CatalogManager;
import org.kalypso.core.catalog.ICatalog;
import org.kalypso.core.catalog.urn.IURNGenerator;
import org.kalypso.gmlschema.annotation.DefaultAnnotation;
import org.kalypso.gmlschema.annotation.IAnnotation;
import org.kalypso.gmlschema.annotation.IAnnotationProvider;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.xml.QualifiedElement;
import org.kalypso.ui.KalypsoGisPlugin;

/**
 * @author Gernot Belger
 */
public class FeatureTypeAnnotationCatalog implements IAnnotationProvider
{
  private static Map<String, IAnnotation> m_annotationCache = new HashMap<String, IAnnotation>();

  /**
   * @see org.kalypso.gmlschema.adapter.IAnnotationProvider#getAnnotation(java.lang.String, java.lang.Object)
   */
  public IAnnotation getAnnotation( final String lang, final Object annotableObject )
  {
    if( annotableObject instanceof QualifiedElement )
    {
      final QualifiedElement qe = (QualifiedElement) annotableObject;
      final QName qname = qe.getQName();

      /* Try to get cached image descriptor */
      final String cacheKey = qname == null ? "null" : qname.toString();

      if( m_annotationCache.containsKey( cacheKey ) )
        return m_annotationCache.get( cacheKey );
      
      // REMARK: catalog is registered for feature type, not for qname
      // Hint for a refaktoring on the CatalogManager
      final CatalogManager catalogManager = KalypsoCorePlugin.getDefault().getCatalogManager();
      final IURNGenerator generator = catalogManager.getURNGeneratorFor( IFeatureType.class );
      if( generator == null )
        return null;

      final String baseURN = generator.generateURNFor( qname ) + ":annotation:";
      if( baseURN == null )
        return null;

      final ICatalog baseCatalog = catalogManager.getBaseCatalog();

      try
      {
        final List<String> enryURNS = baseCatalog.getEntryURNS( baseURN + "*:" + lang );
        if( enryURNS.size() == 0 )
        {
          /* Allways add cache value, so this lookup takes only place once (not finding anything is very expensive) */
          m_annotationCache.put( cacheKey, null );

          return null;
        }

        final DefaultAnnotation annotation = new DefaultAnnotation( lang, qname.getLocalPart() );
        annotation.putValue( IAnnotation.ANNO_NAME, resolveName( baseCatalog, baseURN, IAnnotation.ANNO_NAME, lang ) );
        annotation.putValue( IAnnotation.ANNO_LABEL, resolveName( baseCatalog, baseURN, IAnnotation.ANNO_LABEL, lang ) );
        annotation.putValue( IAnnotation.ANNO_DESCRIPTION, resolveName( baseCatalog, baseURN, IAnnotation.ANNO_DESCRIPTION, lang ) );
        annotation.putValue( IAnnotation.ANNO_TOOLTIP, resolveName( baseCatalog, baseURN, IAnnotation.ANNO_TOOLTIP, lang ) );
        
        m_annotationCache.put( cacheKey, annotation );
        
        return annotation;
      }
      catch( final MalformedURLException e )
      {
        final IStatus status = StatusUtilities.statusFromThrowable( e );
        KalypsoGisPlugin.getDefault().getLog().log( status );
      }
      catch( final JAXBException e )
      {
        final IStatus status = StatusUtilities.statusFromThrowable( e );
        KalypsoGisPlugin.getDefault().getLog().log( status );
      }
      
      /* Allways add cache value, so this lookup takes only place once (not finding anything is very expensive) */
      m_annotationCache.put( cacheKey, null );
    }

    return null;
  }

  private String resolveName( final ICatalog catalog, final String baseURN, final String annoType, final String lang )
  {
    final String publicID = baseURN + annoType + ":" + lang;
    final String resolve = catalog.resolve( null, publicID, false );
    return resolve;
  }
}
