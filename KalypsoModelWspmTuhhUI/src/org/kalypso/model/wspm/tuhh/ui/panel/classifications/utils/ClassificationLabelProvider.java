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
package org.kalypso.model.wspm.tuhh.ui.panel.classifications.utils;

import org.eclipse.jface.viewers.LabelProvider;
import org.kalypso.commons.java.lang.Objects;
import org.kalypso.model.wspm.core.IWspmPointProperties;
import org.kalypso.model.wspm.core.gml.classifications.IClassificationClass;
import org.kalypso.model.wspm.core.gml.classifications.IWspmClassification;
import org.kalypso.model.wspm.core.gml.classifications.helper.WspmClassifications;
import org.kalypso.model.wspm.core.profil.IProfile;

/**
 * @author Dirk Kuch
 */
public class ClassificationLabelProvider extends LabelProvider
{
  private final IProfile m_profile;

  private final String m_property;

  public ClassificationLabelProvider( final IProfile profile, final String property )
  {
    m_profile = profile;
    m_property = property;
  }

  @Override
  public String getText( final Object element )
  {
    if( element instanceof IClassificationClass )
    {
      final IClassificationClass clazz = (IClassificationClass) element;

      return clazz.getLabelWithValues();
    }
    else if( element instanceof String )
    {
      final IClassificationClass clazz = findClass( (String) element );
      if( Objects.isNotNull( clazz ) )
        return clazz.getLabelWithValues();
    }

    return super.getText( element );
  }

  private IClassificationClass findClass( final String identifier )
  {
    final IWspmClassification classification = WspmClassifications.getClassification( m_profile );
    if( Objects.isNull( classification ) )
      return null;

    if( IWspmPointProperties.POINT_PROPERTY_BEWUCHS_CLASS.equals( m_property ) )
    {
      return classification.findVegetationClass( identifier );
    }
    else if( IWspmPointProperties.POINT_PROPERTY_ROUGHNESS_CLASS.equals( m_property ) )
    {
      return classification.findRoughnessClass( identifier );
    }

    return null;
  }
}