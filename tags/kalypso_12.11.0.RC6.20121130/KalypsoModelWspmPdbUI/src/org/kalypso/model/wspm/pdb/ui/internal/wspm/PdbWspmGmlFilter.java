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
package org.kalypso.model.wspm.pdb.ui.internal.wspm;

import java.util.HashSet;
import java.util.Set;

import javax.xml.namespace.QName;

import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerFilter;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.core.gml.WspmWaterBody;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReach;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReachProfileSegment;
import org.kalypso.ui.editor.gmleditor.part.GMLContentProvider;
import org.kalypso.ui.editor.gmleditor.part.LinkedFeatureElement;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree_impl.gml.binding.commons.Image;

/**
 * Filters everything that should not be seen in the gml viewer.
 * 
 * @author Gernot Belger
 */
public class PdbWspmGmlFilter extends ViewerFilter
{
  private final Set<QName> m_filteredElements = new HashSet<>();

  public PdbWspmGmlFilter( )
  {
    m_filteredElements.add( IProfileFeature.FEATURE_PROFILE );

    m_filteredElements.add( WspmWaterBody.MEMBER_PROFILE );
    m_filteredElements.add( WspmWaterBody.MEMBER_RUNOFF );
    m_filteredElements.add( WspmWaterBody.PROPERTY_CENTER_LINE );

    m_filteredElements.add( TuhhReach.QNAME_MEMBER_MARKER );
    m_filteredElements.add( TuhhReach.QNAME_MEMBER_WATER_BODY_LINK );

    m_filteredElements.add( TuhhReachProfileSegment.MEMBER_PROFILE );
    m_filteredElements.add( TuhhReachProfileSegment.PROPERTY_PROFILE_LOCATION );

    m_filteredElements.add( Image.FEATURE_IMAGE );
  }

  @Override
  public boolean select( final Viewer viewer, final Object parentElement, final Object element )
  {
    if( element instanceof GM_Object )
      return false;

    /* Necessary to check parent in 'showAssociation=false' mode */
    final QName parentQName = getRealParentQName( element );
    if( parentQName != null )
    {
      final boolean isParentFiltered = m_filteredElements.contains( parentQName );
      if( isParentFiltered )
        return false;
    }

    final QName qname = GMLContentProvider.getQName( element );
    final boolean doFilter = m_filteredElements.contains( qname );
    if( doFilter )
      return false;

    return !doFilter;
  }

  private QName getRealParentQName( final Object element )
  {
    if( element instanceof Feature )
    {
      final Feature feature = (Feature)element;
      final IRelationType parentRelation = feature.getParentRelation();
      if( parentRelation == null )
        return null;

      return parentRelation.getQName();
    }

    if( element instanceof LinkedFeatureElement )
    {
      final LinkedFeatureElement linkedElement = (LinkedFeatureElement)element;
      return linkedElement.getParentElement().getPropertyType().getQName();
    }

    return null;
  }
}