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
package org.kalypso.core.catalog.urn;

import javax.xml.namespace.QName;

import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.core.catalog.CatalogManager;
import org.kalypso.core.catalog.URNUtilities;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypsodeegree.graphics.sld.FeatureTypeStyle;

/**
 * @author doemming
 */
public class URNGeneratorFeatureTypeStyle implements IURNGenerator
{
  private final static String BASETYPE = "sld";

  /**
   * @see org.kalypso.core.catalog.IURNGenerator#getSupportingClass()
   */
  public Class getSupportingClass( )
  {
    return FeatureTypeStyle.class;
  }

  /**
   * @see org.kalypso.core.catalog.IURNGenerator#isURNGeneratorFor(java.lang.Object)
   */
  public boolean isURNGeneratorFor( Object object )
  {
    if( object instanceof FeatureTypeStyle )
      return true;
    return false;
  }

  /**
   * @see org.kalypso.core.catalog.IURNGenerator#generateURNFor(java.lang.Object)
   */
  public String generateURNFor( final Object object )
  {
    if( object instanceof FeatureTypeStyle )
    {
      final FeatureTypeStyle fts = (FeatureTypeStyle) object;
      final QName featureTypeName = fts.getFeatureTypeName();
      final CatalogManager catalogManager = KalypsoCorePlugin.getDefault().getCatalogManager();
      final IURNGenerator generator = catalogManager.getURNGeneratorFor( IFeatureType.class );
      if( generator == null )
        return null;
      final String baseURN = generator.generateURNFor( featureTypeName );
      final String title = fts.getName();
      return baseURN + ":" + BASETYPE + ":" + URNUtilities.convertURN( title );
    }
    return null;
  }

  /**
   * @see org.kalypso.core.catalog.IURNGenerator#generateURNPatternForRelated(java.lang.Object)
   */
  public String generateURNPatternForRelated( Object related )
  {
    final CatalogManager catalogManager = KalypsoCorePlugin.getDefault().getCatalogManager();
    final IURNGenerator generator = catalogManager.getURNGeneratorFor( IFeatureType.class );
    if( generator == null )
      return null;
    final String baseURN =generator.generateURNFor( related );
    if( baseURN == null )
      return null;
    return baseURN + ":" + BASETYPE + ":*";
  }

  /**
   * @see org.kalypso.core.catalog.IURNGenerator#generateDefaultURNForRelated(java.lang.Object)
   */
  public String generateDefaultURNForRelated( Object related )
  {
    final CatalogManager catalogManager = KalypsoCorePlugin.getDefault().getCatalogManager();
    final IURNGenerator generator = catalogManager.getURNGeneratorFor( IFeatureType.class );
    if( generator == null )
      return null;
    final String baseURN = generator.generateURNFor( related );
    if( baseURN == null )
      return null;
    return baseURN + ":" + BASETYPE + ":default";
  }
}
