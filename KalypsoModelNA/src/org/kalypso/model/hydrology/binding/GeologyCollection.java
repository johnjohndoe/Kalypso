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
 * Binding class for rrmGeology:GeologyCollection's
 *
 * @author Gernot Belger
 */
public class GeologyCollection extends UnversionedModel
{
  public static final QName FEAUTRE_GEOLOGYCOLLECTION = new QName( NaModelConstants.NS_NAGEOLOGY, "GeologyCollection" ); //$NON-NLS-1$

  public static final QName MEMBER_GEOLOGY = new QName( NaModelConstants.NS_NAGEOLOGY, "geologyMember" ); //$NON-NLS-1$

  private final IFeatureBindingCollection<Geology> m_geologyMembers;

  public GeologyCollection( final Object parent, final IRelationType parentRelation, final IFeatureType ft, final String id, final Object[] propValues )
  {
    super( parent, parentRelation, ft, id, propValues );

    m_geologyMembers = new FeatureBindingCollection<>( this, Geology.class, MEMBER_GEOLOGY );
  }

  public IFeatureBindingCollection<Geology> getGeologies( )
  {
    return m_geologyMembers;
  }

  /**
   * Create/Import a new geology into this collection.
   *
   * @return <code>null</code> if the given geometry is <code>null</code>.
   */
  public Geology importGeology( final String label, final GM_MultiSurface geometry, final ImportType importType, final IStatusCollector log )
  {
    if( geometry == null )
      return null;

    // Handle existing geologys that intersect the new one
    final List<Geology> existingMembers = m_geologyMembers.query( geometry.getEnvelope() );
    for( final Geology existingMember : existingMembers )
    {
      switch( importType )
      {
        case DIFFERENCE:
        {
          final GM_MultiSurface existingGeometry = existingMember.getGeometry();
          final GM_MultiSurface difference = PolygonIntersectionHelper.createDifference( geometry, existingGeometry );
          if( difference != null )
          {
            existingMember.setGeometry( difference );
            final String message = Messages.getString( "org.kalypso.convert.namodel.schema.binding.GeologyCollection.3", existingMember.getId(), label ); //$NON-NLS-1$
            log.add( IStatus.INFO, message, null );
          }
          else
          {
            m_geologyMembers.remove( existingMember );
            final String message = Messages.getString( "org.kalypso.convert.namodel.schema.binding.GeologyCollection.4", existingMember.getId(), label ); //$NON-NLS-1$
            log.add( IStatus.INFO, message, null );
          }
        }

        case CLEAR_OUTPUT:
          // nothing to do, we add all geologys
          break;
      }
    }

    // Create new geology
    final Geology geology = m_geologyMembers.addNew( Geology.FEATURE_GEOLOGY );
    geology.setName( label );
    geology.setGeometry( geometry );
    return geology;
  }
}
