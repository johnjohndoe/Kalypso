/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
package org.kalypso.ogc.gml;

import org.apache.commons.lang.builder.EqualsBuilder;
import org.kalypso.ogc.gml.mapmodel.IKalypsoThemePredicate;
import org.kalypso.template.types.StyledLayerType;

/**
 * This predicates tests if a theme is a {@link GisTemplateFeatureTheme} which a given source and feature path.
 * 
 * @author Gernot Belger
 */
public class SoureAndPathThemePredicate implements IKalypsoThemePredicate
{
  private org.kalypso.template.types.ObjectFactory m_templateFactory;
  private final String m_href;
  private final String m_featurePath;

  public SoureAndPathThemePredicate( final String href, final String featurePath )
  {
    m_href = href;
    m_featurePath = featurePath;
    
    m_templateFactory = new org.kalypso.template.types.ObjectFactory();
    
  }

  /**
   * @see org.kalypso.ogc.gml.mapmodel.IKalypsoThemePredicate#decide(org.kalypso.ogc.gml.IKalypsoTheme)
   */
  public boolean decide( final IKalypsoTheme theme )
  {
    if( theme instanceof GisTemplateFeatureTheme )
    {
      final GisTemplateFeatureTheme gisTheme = (GisTemplateFeatureTheme) theme;
      final StyledLayerType layer = m_templateFactory.createStyledLayerType();
      gisTheme.fillLayerType( layer, "doesNotMatter", true ); //$NON-NLS-1$
      
      final EqualsBuilder equalsBuilder = new EqualsBuilder();
      equalsBuilder.append( m_href, layer.getHref() );
      equalsBuilder.append( m_featurePath, layer.getFeaturePath() );
      
      return equalsBuilder.isEquals();
    }

    return false;
  }

}
