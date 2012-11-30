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
package org.kalypso.model.hydrology.binding;

import java.util.List;

import javax.xml.namespace.QName;

import org.eclipse.core.runtime.IStatus;
import org.kalypso.afgui.model.UnversionedModel;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.hydrology.NaModelConstants;
import org.kalypso.model.hydrology.binding.PolygonIntersectionHelper.ImportType;
import org.kalypso.model.hydrology.internal.i18n.Messages;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree.model.geometry.GM_MultiSurface;
import org.kalypsodeegree_impl.model.feature.FeatureBindingCollection;

/**
 * Binding class for rrmHydo:OverlayCollection's
 *
 * @author Gernot Belger
 */
public class OverlayCollection extends UnversionedModel
{
  public static final QName FEATURE_OVERLAY_COLLECTION = new QName( NaModelConstants.NS_NAOVERLAY, "OverlayCollection" ); //$NON-NLS-1$

  public static final QName MEMBER_ELEMENTS = new QName( NaModelConstants.NS_NAOVERLAY, "elementMembers" ); //$NON-NLS-1$

  private final IFeatureBindingCollection<OverlayElement> m_overlayElements;

  public OverlayCollection( final Object parent, final IRelationType parentRelation, final IFeatureType ft, final String id, final Object[] propValues )
  {
    super( parent, parentRelation, ft, id, propValues );

    m_overlayElements = new FeatureBindingCollection<>( this, OverlayElement.class, MEMBER_ELEMENTS );
  }

  public IFeatureBindingCollection<OverlayElement> getOverlayElements( )
  {
    return m_overlayElements;
  }

  /**
   * Create/Import a new soilType into this collection.
   *
   * @return <code>null</code> if the given geometry is <code>null</code>.
   */
  public OverlayElement importOverlayElement( final String label, final GM_MultiSurface geometry, final ImportType importType, final String refDRWBMDefinition, final IStatusCollector log )
  {
    if( geometry == null )
      return null;

    // Handle existing soilTypes that intersect the new one
    final List<OverlayElement> existingSoilTypes = m_overlayElements.query( geometry.getEnvelope() );
    for( final OverlayElement existingSoiltype : existingSoilTypes )
    {
      switch( importType )
      {
        case DIFFERENCE:
        {
          // TODO: Dirk: check
          final GM_MultiSurface existingGeometry = existingSoiltype.getGeometry();
          final GM_MultiSurface difference = PolygonIntersectionHelper.createDifference( geometry, existingGeometry );
          if( difference != null )
          {
            existingSoiltype.setGeometry( difference );
            final String message = Messages.getString( "org.kalypso.convert.namodel.schema.binding.OverlayCollection.3", existingSoiltype.getId(), label ); //$NON-NLS-1$
            log.add( IStatus.INFO, message );
          }
          else
          {
            m_overlayElements.remove( existingSoiltype );
            final String message = Messages.getString( "org.kalypso.convert.namodel.schema.binding.OverlayCollection.4", existingSoiltype.getId(), label ); //$NON-NLS-1$
            log.add( IStatus.INFO, message );
          }
        }

        case CLEAR_OUTPUT:
          // nothing to do, we add all soilTypes
          break;
      }
    }

    // Create a new overlay element
    final OverlayElement element = m_overlayElements.addNew( OverlayElement.FEATURE_OVERLAY_ELEMENT );
    element.setName( label );
    element.setGeometry( geometry );
    element.setDRWBMDefinition( refDRWBMDefinition );

    return element;
  }
}
