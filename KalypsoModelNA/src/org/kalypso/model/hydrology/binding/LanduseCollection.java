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
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.hydrology.NaModelConstants;
import org.kalypso.model.hydrology.binding.PolygonIntersectionHelper.ImportType;
import org.kalypso.model.hydrology.internal.i18n.Messages;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree.model.geometry.GM_MultiSurface;
import org.kalypsodeegree_impl.model.feature.FeatureBindingCollection;
import org.kalypsodeegree_impl.model.feature.Feature_Impl;

/**
 * Binding class for rrmLanduse:LanduseCollection's
 * 
 * @author Gernot Belger
 */
public class LanduseCollection extends Feature_Impl
{
  public static final QName QNAME_PROP_LANDUSEMEMBER = new QName( NaModelConstants.NS_NALANDUSE, "landuseMember" ); //$NON-NLS-1$

  private final IFeatureBindingCollection<Landuse> m_landuses;

  public LanduseCollection( final Object parent, final IRelationType parentRelation, final IFeatureType ft, final String id, final Object[] propValues )
  {
    super( parent, parentRelation, ft, id, propValues );

    m_landuses = new FeatureBindingCollection<Landuse>( this, Landuse.class, QNAME_PROP_LANDUSEMEMBER );
  }

  public IFeatureBindingCollection<Landuse> getLanduses( )
  {
    return m_landuses;
  }

  /**
   * Create/Import a new landuse into this collection.
   * 
   * @return <code>null</code> if the given geometry is <code>null</code>.
   */
  public Landuse importLanduse( final String label, final GM_MultiSurface geometry, final ImportType importType, final IStatusCollector log )
  {
    if( geometry == null )
      return null;

    // Handle existing landuses that intersect the new one
    final List<Landuse> existingLanduses = m_landuses.query( geometry.getEnvelope() );
    for( final Landuse existingLanduse : existingLanduses )
    {
      switch( importType )
      {
        case DELETE_INTERSECTING:
        {
          m_landuses.remove( existingLanduse );
          final String message = Messages.getString("org.kalypso.convert.namodel.schema.binding.LanduseCollection.1", existingLanduse.getId() ); //$NON-NLS-1$
          log.add( IStatus.WARNING, message );
        }
        break;

        case IGNORE_INTERSECTING:
        {
          final String message =  Messages.getString("org.kalypso.convert.namodel.schema.binding.LanduseCollection.2", label ); //$NON-NLS-1$
          log.add( IStatus.WARNING, message );
        }
        return null;

        case INTERSECT:
        {
          final GM_MultiSurface existingGeometry = existingLanduse.getGeometry();
          final GM_MultiSurface difference = PolygonIntersectionHelper.createDifference( geometry, existingGeometry );
          if( difference != null )
          {// TODO: check if area of difference is > 0!
            existingLanduse.setGeometry( difference );
            final String message = Messages.getString("org.kalypso.convert.namodel.schema.binding.LanduseCollection.3", existingLanduse.getId(), label ); //$NON-NLS-1$
            log.add( IStatus.INFO, message );
          }
          else
          {
            m_landuses.remove( existingLanduse );
            final String message =  Messages.getString("org.kalypso.convert.namodel.schema.binding.LanduseCollection.4", existingLanduse.getId(), label ); //$NON-NLS-1$
            log.add( IStatus.INFO, message );
          }
        }

        case CLEAR_OUTPUT:
          // nothing to do, we add all landuses
          break;
      }
    }

    // Create new landuse
    final Landuse landuse = m_landuses.addNew( Landuse.QNAME );
    landuse.setName( label );
    landuse.setGeometry( geometry );
    return landuse;
  }
}
