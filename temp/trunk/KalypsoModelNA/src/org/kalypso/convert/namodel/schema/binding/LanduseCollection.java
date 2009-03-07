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
package org.kalypso.convert.namodel.schema.binding;

import java.util.List;

import javax.xml.namespace.QName;

import org.eclipse.core.runtime.IStatus;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.convert.namodel.NaModelConstants;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree.model.geometry.GM_MultiSurface;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree_impl.model.feature.FeatureBindingCollection;
import org.kalypsodeegree_impl.model.feature.Feature_Impl;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * Binding class for rrmLanduse:LanduseCollection's
 *
 * @author Gernot Belger
 */
public class LanduseCollection extends Feature_Impl
{
  /**
   * Defines how existing landuse classes are handled when imported.
   */
  public static enum ImportType
  {
    /** Delete all existing landuses */
    CLEAR_OUTPUT,
    /** Delete all existing landuses, that intersect with any existing one (produces a warning) */
    DELETE_INTERSECTING,
    /** Ignore all imported landuses, that intersect with any existing one (produces warning) */
    IGNORE_INTERSECTING,
    /**
     * from existing landuses, intersecting imported landuses are removed; imported landuses are imported as new
     * landuses
     */
    INTERSECT
  }

  private static final QName QNAME_PROP_LANDUSEMEMBER = new QName( NaModelConstants.NS_NALANDUSE, "landuseMember" );

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
  public Landuse importLanduse( final String label, final GM_MultiSurface geometry, final ImportType importType, final List<IStatus> log )
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
          final String message = String.format( "Landuse '%s' was deleted due to intersection", existingLanduse.getId() );
          log.add( StatusUtilities.createStatus( IStatus.WARNING, message, null ) );
        }
          break;

        case IGNORE_INTERSECTING:
        {
          final String message = String.format( "Ingoring imported landuse '%s' due to intersection", label );
          log.add( StatusUtilities.createStatus( IStatus.WARNING, message, null ) );
        }
          return null;

        case INTERSECT:
        {
          final GM_MultiSurface existingGeometry = existingLanduse.getGeometry();
          final GM_MultiSurface difference = createDifference( geometry, existingGeometry );
          if( difference != null )
          {
            existingLanduse.setGeometry( difference );
            final String message = String.format( "Landuse '%s' was reduced by imported landuse '%s'", existingLanduse.getId(), label );
            log.add( StatusUtilities.createStatus( IStatus.INFO, message, null ) );
          }
          else
          {
            m_landuses.remove( existingLanduse );
            final String message = String.format( "Landuse '%s' was removed by imported landuse '%s'", existingLanduse.getId(), label );
            log.add( StatusUtilities.createStatus( IStatus.INFO, message, null ) );
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

  /**
   * Creates the difference for the {@link ImportType#INTERSECT} import type. Anything not resulting in a surface is
   * ignored.
   */
  @SuppressWarnings("unchecked")
  private GM_MultiSurface createDifference( final GM_MultiSurface geometry, final GM_MultiSurface existingGeometry )
  {
    final GM_Object difference = existingGeometry.difference( geometry );
    if( difference instanceof GM_MultiSurface )
      return (GM_MultiSurface) difference;

    if( difference instanceof GM_Surface )
    {
      final GM_Surface< ? > surface = (GM_Surface< ? >) difference;
      return GeometryFactory.createGM_MultiSurface( new GM_Surface[] { surface }, difference.getCoordinateSystem() );
    }

    /* Ignore all another cases */
    return null;
  }

}
